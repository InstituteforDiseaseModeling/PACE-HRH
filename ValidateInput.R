options(install.packages.check.source = "no")
packages = c("validate","readxl", "dplyr","ggplot2", "tidyr", "kableExtra", "stringr", "plyr", "reshape2", "scales")
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}
library(pacehrh)
.Success <- 0L
.errValidationRuleFailed <- -1L

# define key columns for each sheet (use for reporting)
key_cols <- list (PopValues = c("Description", "Sex", "InitValue", "ChangeRate"), 
                  StochasticParameters = c("value"),
                  TaskValues_ref = c("Indicator", "CommonName"))
# define rule-specific extra columns (use for reporting)
rule_cols <- list(AnnualDeltaRatio_value_Range = c("StartingRateInPop"),
                  RateMultiplier = c("StartingRateInPop", "MultiplierReason")
                  )


#' Perform Validation on Input Excel File
#'
#' @description
#' Validate if all inputs comply with parameter rules
#'
#' @param inputFile Excel file to examine.
#' @param outputDir Results output directory. default is in log folder
#'
#' @return Error code.
#' 0 = Success
#' -1 = validation error
Validate <- function(inputFile, outputDir = "log"){
  
  # Make sure outputDir is empty
  tryCatch(
    {
      unlink(outputDir,recursive=TRUE)
    },
    error=function(cond){
      message(cond)
    }
  )
  dir.create(outputDir, showWarnings = FALSE)
  
  result_d <- ValidateInputExcelFileContent(inputFile = inputFile, outputDir = outputDir, sheetNames = NULL)
  sink(file = paste(outputDir, "model_input_check.log", sep = "/"))
  result_m <- pacehrh::CheckInputExcelFileFormat(inputFile = inputFile)
  sink()
  .custom_check(inputFile = inputFile, outputDir = outputDir)
  return (if (result_d==.Success & result_m==pacehrh:::.Success) .Success else (.errValidationRuleFailed))
}


ValidateInputExcelFileContent <- function(inputFile,
                                          outputDir = "log",
                                          sheetNames = NULL){
  errcode <- .Success
  
  # check all available rules
  if (is.null(sheetNames)){
    sheetNames <- gsub("^rules_([A-Za-z_0-9]+).yaml$", "\\1", list.files("config/validation/rules"))
  }
 
  # loop over each sheet and apply corresponding rules
  result <- data.frame()
  rules_combined <- data.frame()
  plots <- vector()
  for (i in sheetNames){
    f = file.path("config/validation/rules", paste("rules_", i, ".yaml", sep=""))
    if (!file.exists(f)){
      stop(paste("rule not found for:", i))
    }
    else{
      rules <- validator(.file=f)
      if (length(rules_combined)==0){
        rules_combined <- validate::as.data.frame(rules)
      }
      else {
        rules_combined <- rbind(rules_combined, validate::as.data.frame(rules))
      }
      
      data <- read_xlsx(inputFile, sheet = i)
      data_target <- data.frame(data)
      
      # Confront the rules and check if any error occurs
      out <- confront(data_target, rules,  lin.ineq.eps=0, lin.eq.eps=1e-08)
      if(TRUE %in% validate::summary(out)$error){
        stop(paste("Some error occurred evaluating rules:", i))
      }
      # Apply rules and save the violation results
      check <- .get_violation_rows(out, data_target, rules, outputDir, i)
      errcode = min(errcode, check)
      
      # combine results in the loop
      if (length(result)==0){
        result <- validate::as.data.frame(validate::summary(out))
      }
      else {
        result <- rbind(result,  validate::as.data.frame(validate::summary(out)))
      }
      
    }
  }
  result_file <- file.path(outputDir, "input_validation_results.csv")
  result_details <- result %>%
    inner_join(rules_combined, by = c("name")) %>%
    select(-c("language","created"))
  write.csv(result_details, result_file)
  return (errcode)
}


.get_violation_rows <- function(out, target, rules, outputDir, sheetName){
  errcode <- .Success
  for (i in names(out)){
    severity <- meta(rules[i])$severity
    df_violations <- NULL
    if (nrow(validate::values(out[i])) > 1 | validate::values(out[i])[[1]] == FALSE){
      if (severity == "error"){
        errcode <- .errValidationRuleFailed
      }
      tryCatch(
        {
          df_violations <- violating(target, out[i])
        },
        error=function(e){
          message(paste("Rule failed but No record-wise info: ", e))
          df_violations <- NA
          return (df_violations)
        }
      )
      if (!is.null(df_violations)){
        # only fail the validation for critical rules
        if(nrow(df_violations) > 0){
          if (severity == "error"){
            errcode <- .errValidationRuleFailed
          }
          write.csv(df_violations, file.path(outputDir, paste(sheetName, "_", severity, "_violation_", i, ".csv", sep="")))
        }
      }
    }
  }
  return (errcode)
}

.simple_plot <- function(result_details){
  
  # summary and plot check result
  final_result <- result_details %>%
    select(c("name", "severity", "items", "fails", "passes", "expression")) %>%
    pivot_longer(cols=c("passes", "fails"), names_to = "result", values_to = "total") %>%
    mutate(result = if_else(severity != 'error' & result == 'fails', severity, result))
  
  outfile <- file.path(outputDir, "input_validation_results.png")
  p <- ggplot(final_result, aes(x=total/items, y=name, fill=result)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("passes"="green", "fails"="red", "warning"="yellow", "info"="blue")) +
    scale_x_continuous(labels=scales::percent) +
    geom_text(data=subset(final_result,total> 0), aes(label=total, y =name), size=1.5, position=position_fill()) +
    ggtitle("Input Spreadsheet Validation Results")
  ggsave(outfile, p, width = 160, height = 100, units="mm")

}

.custom_check <- function(inputFile, outputDir = "log"){
  # define custom rules, for each rules, we choose to produce a graph (with custom_ prefix)
  # or a csv with name, description, severity written to "custom_validation_results.csv"
  custom_dir <- file.path(outputDir, "custom")
  df_reason <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df_reason) <- c("name", "description", "severity")

  ##### Check if the seasonality curve setting is valid #####
  ##### (Code provided by Brittany Hagedorn)                  #####

  DS <- read_xlsx(inputFile ,sheet="SeasonalityCurves")

  #reshape into long format
  DSmelt <- melt(DS,id.vars = c("Month"),variable.name = "CurveName",value.name="RatioValue")

  #set color scheme limits
  YellowMax <- 1/12 * 1.5
  YellowMin <- 1/12 * 0.5
  RedMax <- .25

  #set color categories for monthly values
  DSmelt$colorcode <- "Okay"
  DSmelt$colorcode[DSmelt$RatioValue>YellowMax | DSmelt$RatioValue<YellowMin] = "Possible error"
  DSmelt$colorcode[DSmelt$RatioValue>RedMax] = "Validation needed"

  #Organize months
  monthslist <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")
  DSmelt$Month <- factor(DSmelt$Month,ordered=TRUE,levels=monthslist)

  g_monthly <- ggplot(DSmelt,aes(x=Month,y=RatioValue,fill=as.factor(colorcode)))+
    geom_bar(stat="identity")+facet_wrap(~CurveName)+
    scale_fill_manual(values=c("grey","goldenrod1","darkred"))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.title = element_blank())+
    xlab("Month")+ylab("Monthly proportion")+
    labs(title="Validation: Monthtly variation in seasonality values")
  ggsave(file.path(custom_dir, "custom_seasonality.png"), g_monthly, width = 160, height = 80, units="mm")

  AnnualTotals <- ddply(DSmelt, .(CurveName), summarize, AnnualTotal=sum(RatioValue))

  #Set color code categories for annual totals
  AnnualTotals$colorcode <- "Okay"
  AnnualTotals$colorcode[abs(AnnualTotals$AnnualTotal -1.0) > 1e-2] = "Error"

  g_total <- ggplot(AnnualTotals,aes(x=CurveName,y=AnnualTotal,fill=colorcode))+
    geom_bar(stat="identity")+
    scale_fill_manual(values=c("Error"="darkred","Okay"="darkgreen"))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.title = element_blank())+
    xlab("Seasonality Curve")+ylab("Annual Total")+
    labs(title="Validation: Seasonality curves should sum to 1.0")
  ggsave(file.path(custom_dir, "custom_seasonality_total.png"), g_total, width = 120, height = 80, units="mm")

  ##### Check seasonality offset validity #####
  filename <- file.path(custom_dir, "violation_offsets_exceed_range.csv")
  description <- "Seasonality offsets should represent an adjustment period within 1 year (12 months) of the originating event,
  so values less than -11 or greater than 11 are not accepted."
  severity <- "error"
  SO <- read_xlsx(inputFile ,sheet="SeasonalityOffsets")
  violation <- SO %>% filter_at(vars(starts_with('Offset')), any_vars(.< -11 | .>11))
  if(nrow(violation) > 0){
    write.csv(violation, filename)
    df_reason[nrow(df_reason) + 1,] = c(filename, description, severity)
  }
  ##### Check seasonality offset is integer #####
  filename <- file.path(custom_dir, "violation_offsets_not_integer.csv")
  description <- "The simulation operates on a monthly basis, so all offsets must be integers."
  severity <- "error"
  violation <- SO %>% filter_at(vars(starts_with('Offset')), any_vars(!round(.)==.))
  write.csv(violation, filename)
  df_reason[nrow(df_reason) + 1,] = c(filename, description, severity)

  ##### Check seasonality offset is in taskValue sheet" #####
  filename <- file.path(custom_dir, "violation_offsets_not_in_task.csv")
  description <- "Task has a seasonality offset, but it is not listed in the Task Values sheet.
  Thus, this task will not have time allocated and thus the seasonality offset will not have any impact
  on the model’s results. Please verify that this is expected behavior."
  severity <- "warning"
  TV <- read_xlsx(inputFile ,sheet="TaskValues_ref")
  violation <- SO %>%
    filter(!(Task %in%  unique(TV$Indicator))) %>%
    select(Task, Description, Curve)
  if(nrow(violation) > 0){
    write.csv(violation, filename)
    df_reason[nrow(df_reason) + 1,] = c(filename, description, severity)
  }

  ##### Check seasonality offset is in seasonalityCurve" #####
  filename <- file.path(custom_dir, "violation_offsets_not_in_curve.csv")
  description <- "Seasonality offsets link to the Seasonality curves sheet columns.
  This offset does not match any seasonality curve, and thus will not be used
  or impact the model’s results. Please verify that this is expected behavior."
  severity <- "warning"
  SC <- read_xlsx(inputFile ,sheet="SeasonalityCurves")
  violation <- SO %>%
    filter(!(Curve %in% colnames(SC))) %>%
    select(Task, Description, Curve)
  if(nrow(violation) > 0){
    write.csv(violation, filename)
    df_reason[nrow(df_reason) + 1,] = c(filename, description, severity)
    }

  # writing metadata for custom check
  write.csv(df_reason, file.path(custom_dir, "custom_validation_results.csv"))

}
