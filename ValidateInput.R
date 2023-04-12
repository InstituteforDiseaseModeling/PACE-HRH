options(install.packages.check.source = "no")
packages = c("validate","readxl", "dplyr","ggplot2", "tidyr", "kableExtra", "stringr", "plyr", "reshape2", "scales", "glue", "logr")
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}
library(pacehrh)
.Success <- 0L
.errValidationRuleFailed <- -1L
.errorLogfile <- "validation_details.log"

# define key columns for each sheet (use for reporting)
key_cols <- list (PopValues = c("Description", "Sex", "InitValue", "ChangeRate"), 
                  StochasticParameters = c("value"),
                  TaskValues_ref = c("Indicator", "CommonName"))
# define rule-specific extra columns (use for reporting)
rule_cols <- list(AnnualDeltaRatio_value_Range = c("StartingRateInPop"),
                  RateMultiplier = c("StartingRateInPop")
                  )

# store metadata for custom check
custom_test <- new.env()
custom_test$df_reason <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(custom_test$df_reason) <- c("name", "description", "severity")

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
Validate <- function(inputFile, outputDir = "log", optional_sheets = NULL){
  
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
  
  result_d <- ValidateInputExcelFileContent(inputFile = inputFile, outputDir = outputDir, optional_sheets = optional_sheets)
  sink(file = paste(outputDir, "model_input_check.log", sep = "/"))
  result_m <- pacehrh::CheckInputExcelFileFormat(inputFile = inputFile)
  sink()
  .custom_check(inputFile = inputFile, outputDir = outputDir)
  return (if (result_d==.Success & result_m==pacehrh:::.Success) .Success else (.errValidationRuleFailed))
}


ValidateInputExcelFileContent <- function(inputFile,
                                          outputDir = "log",
                                          optional_sheets = NULL){
  
  logfile <- file.path(outputDir, .errorLogfile)
  lf <- log_open(file_name = logfile, show_notes = FALSE)
  
  input_sheetNames <- readxl::excel_sheets(path = inputFile)
  tryCatch(
    {
      errcode <- .Success
      checklist <- data.frame(
        sheet = character(),
        rule = character(),
        stringsAsFactors = FALSE)
      
      all_sheetNames <- gsub("^rules_([A-Za-z_0-9]+).yaml$", "\\1", list.files("config/validation/rules"))
      
      # check all available rules
   
      # Identify rules to apply from scenario tab
      scenarios <- read_xlsx(inputFile, "Scenarios")
      checklist = rbind(checklist, scenarios %>% mutate (rule = "rules_PopValues.yaml", sheet = sheet_PopValues ) %>% select(c(sheet, rule)) %>% unique())
      checklist = rbind(checklist, scenarios %>% mutate (rule = "rules_SeasonalityCurves.yaml", sheet = sheet_SeasonalityCurves ) %>% select(c(sheet, rule)) %>% unique())
      checklist = rbind(checklist, scenarios %>% mutate (rule = "rules_TaskValues_ref.yaml", sheet = sheet_TaskValues ) %>% select(c(sheet, rule)) %>% unique())
      
      for (f in list.files("config/validation/rules")){
        default_rulename <- gsub("^rules_([A-Za-z_0-9]+).yaml$", "\\1", f)
        if (!f %in% checklist$rule){
          # although we assume the rule names will match sheet names, if sheet does not exist, we will not check it
          if (default_rulename %in% input_sheetNames){
            checklist[nrow(checklist) + 1,] <- list(sheet=default_rulename, rule=f)
          }
          else{
            log_print(glue("sheet: {default_rulename} is missing, check will be skipped unless specified in optional_sheets argument."))
          }
          
        }
        else{
          # add key column to the sheets specified in the Scenarios
          new_keys <- as.list(checklist %>% filter (rule ==f) %>% select (sheet))
          if (default_rulename %in% names(key_cols)){
            for (k in new_keys){
              key_cols[k] <- list(key_cols[[default_rulename]])
            }
          }
        }
      }
      key_cols <<- key_cols
    
      if (!is.null(optional_sheets)){
        for (i in 1:length(optional_sheets)){
          if (optional_sheets[i] %in% list.files("config/validation/rules")){
            checklist[nrow(checklist) + 1,] <- list(sheet=names(optional_sheets[i]), rule=paste(optional_sheets[i]))
          }
          else{
            stop(glue("Unable to check sheet: {names(optional_sheets[i])}. Cannot find a matching rule : {paste(optional_sheets[i])}"))
          }
        }
      }
      
      
      # loop over each sheet and apply corresponding rules
      result <- data.frame()
      rules_combined <- data.frame()
      plots <- vector()
      for (i in seq(1, nrow(checklist))){
        
        f = file.path("config/validation/rules", checklist[[i, "rule"]])
        sheet <- checklist[[i, "sheet"]]
        if (!file.exists(f)){
          stop(paste("rule not found for:", sheet))
        }
        else{
          rules <- validator(.file=f)
          if (length(rules_combined)==0){
            rules_combined <- validate::as.data.frame(rules)
          }
          else {
            rules_combined <- rbind(rules_combined, validate::as.data.frame(rules))
          }
          
          # Fail if sheet exist
          if (!sheet %in% readxl::excel_sheets(inputFile)){
            stop(glue('sheet: "{sheet}" does not exist in {inputFile}'))  
          }
          
          data <- read_xlsx(inputFile, sheet =sheet)
          data_target <- data.frame(data)
          
          # Confront the rules and check if any error occurs
          out <- confront(data_target, rules,  lin.ineq.eps=0, lin.eq.eps=1e-08)
          if(TRUE %in% validate::summary(out)$error){
            log_print(glue("some rules cannot be applied to sheet: {sheet} \nMaybe columns in expression is missing?"))
            log_print(validate::summary(out) %>% filter (error == 1) %>% select (expression) %>% unique())
            log_print(glue("Some error occurred evaluating {sheet}: {validate::errors(out)}"))
          }
          # Apply rules and save the violation results
          check <- .get_violation_rows(out, data_target, rules, outputDir, sheet)
          errcode = min(errcode, check)
          
          # combine results in the loop
          if (length(result)==0){
            result <- validate::as.data.frame(validate::summary(out))
            result <- result %>% mutate (sheet_name = sheet)
          }
          else {
            new_result <- validate::as.data.frame(validate::summary(out))
            new_result <- new_result %>% mutate (sheet_name = sheet)
            result <- rbind(result,  new_result, make.row.names=TRUE, stringsAsFactors = FALSE)
          }
          
        }
      }
      result_file <- file.path(outputDir, "input_validation_results.csv")
      rules_combined <- rules_combined %>%
        select(-c("language","created")) %>%
        unique()
      
      result_details <- result %>%
        inner_join(rules_combined, by = c("name"))
      
      write.csv(result_details, result_file)
      log_print("validation completed.")
      return (errcode)
    },
    error = function(e){
      if (log_status() == 'open') print(writeLines(readLines(lf)))
      stop(e)
    },
    finally = {
      if (log_status() != 'closed') log_close()
    }
  )
}


.get_violation_rows <- function(out, target, rules, outputDir, sheetName){
  errcode <- .Success
  for (i in names(out)){
    severity <- meta(rules[i])$severity
    df_violations <- NULL
    if (length(validate::errors(out[i])) > 0){
      next # ignore rules which encounter errors
    }
    if (nrow(validate::values(out[i])) > 0 & any(validate::values(out[i]) == FALSE)){
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

.custom_check_write_result <- function(description, filename, severity, violation){
  if(nrow(violation) > 0){
    write.csv(violation, filename)
    custom_test$df_reason[nrow(custom_test$df_reason) + 1,] <- c(filename, description, severity)
  }
}

.custom_check <- function(inputFile, outputDir = "log"){
  # define custom rules, for each rules, we choose to produce a graph (with custom_ prefix)
  # or a csv with name, description, severity written to "custom_validation_results.csv"
  custom_dir <- file.path(outputDir, "custom")
  dir.create(custom_dir, showWarnings = F)

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
  .custom_check_write_result(description, filename, severity, violation)

  ##### Check seasonality offset is integer #####
  filename <- file.path(custom_dir, "violation_offsets_not_integer.csv")
  description <- "The simulation operates on a monthly basis, so all offsets must be integers."
  severity <- "error"
  violation <- SO %>% filter_at(vars(starts_with('Offset')), any_vars(!round(.)==.))
  .custom_check_write_result(description, filename, severity, violation)

  ##### Check seasonality offset is in taskValue sheet" #####
  scenarios <- read_xlsx(inputFile, sheet="Scenarios")
  taskvalue_sheets <- unique(scenarios$sheet_TaskValues)
  for (taskvalue_sheet in taskvalue_sheets){
    filename <- file.path(custom_dir, glue("violation_offsets_not_in_{taskvalue_sheet}.csv"))
    description <- glue("Task has a seasonality offset, but it is not listed in the {taskvalue_sheet} sheet. 
    Thus, this task will not have time allocated and thus the seasonality offset will not have any impact
    on the model’s results. Please verify that this is expected behavior.")
    severity <- "warning"
    TV <- read_xlsx(inputFile ,sheet=taskvalue_sheet)
    violation <- SO %>%
      filter(!(Task %in%  unique(TV$Indicator))) %>%
      select(Task, Description, Curve)
    .custom_check_write_result(description, filename, severity, violation)
  }
  
  ##### Check seasonality offset is in seasonalityCurve" #####
  seasonality_sheets <- unique(scenarios$sheet_SeasonalityCurves)
  for (seasonality_sheet in seasonality_sheets){
    filename <- file.path(custom_dir, glue("violation_offsets_not_in_{seasonality_sheet}.csv"))
    description <- glue("Seasonality offsets link to the {seasonality_sheet} sheet columns.
    This offset does not match any seasonality curve, and thus will not be used
    or impact the model’s results. Please verify that this is expected behavior.")
    severity <- "warning"
    SC <- read_xlsx(inputFile ,sheet=seasonality_sheet)
    violation <- SO %>%
      filter(!(Curve %in% colnames(SC))) %>%
      select(Task, Description, Curve)
    .custom_check_write_result(description, filename, severity, violation)
  }
  
  ##### Check if cadre sheets have all years cover the startYear, endYear defined in CadreRoles sheet
  defaultEndYear = 2040
  cadreRoles <- read_xlsx(inputFile, sheet="CadreRoles")
  cadreRoles <- cadreRoles %>% 
    inner_join(scenarios, by=c('ScenarioID'='UniqueID')) %>%
    select(ScenarioID, RoleID, StartYear, EndYear, sheet_Cadre) %>%
    mutate(EndYear = if_else(is.na(EndYear), 2040, EndYear))
  cadre_list <- split(cadreRoles, cadreRoles$sheet_Cadre)
  for (i in seq_along(cadre_list)) {
    df <- cadre_list[[i]]
    filename <- file.path(custom_dir, glue("violation_header_not_in_{unique(df$sheet_Cadre)}.csv"))
    description <- glue("Expected headers based on CadreRoles should be found in {unique(df$sheet_Cadre)}.")
    severity <- "warning"
    
    cadre_headers <- read_xlsx(inputFile, sheet=unique(df$sheet_Cadre), n_max=2, col_names = FALSE)
    df_violations <-  data.frame(RoleID =character(), Expected_header = character(), stringsAsFactors = FALSE)
    for (j in 1:nrow(df)){
      expected_cols <- unname(as.list(paste0("StartYear", seq(floor(df$StartYear[j]/5)*5, ceiling(df$EndYear[j]/5)*5, by=5))))
      if(!any(cadre_headers[2,]==df$RoleID[j])){
        # if the role is defined in the CadreRoles, it must appear in cadre sheet
        for (k in expected_cols){
          df_violations[nrow(df_violations)+1, ] = c(df$RoleID[j],expected_cols[k])
        }
      }
      else{
        # check for year that is not defined within StartYear and EndYear
        actual <- unname(as.list(cadre_headers[1,which(cadre_headers[2,]==df$RoleID[j])]))
        missing <- setdiff(expected_cols, actual)
        for (k in length(missing)){
          df_violations[nrow(df_violations)+1, ] = c(df$RoleID[j], missing[k])
        }
      }
     
    }
    .custom_check_write_result(description, filename, severity, df_violations)
  }
 
  # writing metadata for custom check
  write.csv(custom_test$df_reason, file.path(custom_dir, "custom_validation_results.csv"))

}
