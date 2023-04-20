options(install.packages.check.source = "no")
packages = c("validate","readxl", "dplyr","ggplot2", "tidyr", "kableExtra", "stringr", "plyr", "reshape2", "scales", "glue", "logr", "gridExtra")
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
  log_file <- file.path(outputDir, .errorLogfile)
  lf <- log_open(file_name = log_file, show_notes = FALSE)
  result_d <- ValidateInputExcelFileContent(inputFile = inputFile, logFile =lf, outputDir = outputDir, optional_sheets = optional_sheets)
  sink(file = paste(outputDir, "model_input_check.log", sep = "/"))
  result_m <- pacehrh::CheckInputExcelFileFormat(inputFile = inputFile)
  sink()
  .custom_check(inputFile = inputFile, logFile = lf, outputDir = outputDir)
  if (log_status() != 'closed') log_close()
  
  return (if (result_d==.Success & result_m==pacehrh:::.Success) .Success else (.errValidationRuleFailed))
}


ValidateInputExcelFileContent <- function(inputFile,
                                          logFile,
                                          outputDir = "log",
                                          optional_sheets = NULL){
 
 
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
      if (log_status() == 'open') print(writeLines(readLines(logFile)))
      stop(e)
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

.cadre_check_scenarios <- function(df, inputFile, custom_dir){
  
  ### Read Cadre headers in long format
  cadre_headers <- read_xlsx(inputFile, sheet=unique(df$sheet_Cadre), n_max=2, col_names = FALSE)
  cadre_headers <- cadre_headers[,grepl("StartYear", cadre_headers[1, ])] %>% 
    mutate_all(~str_replace(.,"StartYear", ""))
  cadre_headers <- as.data.frame(t(cadre_headers))
  colnames(cadre_headers) <- c("StartYear", "RoleID")
  cadre_headers <- cadre_headers %>% mutate(StartYear = as.numeric(StartYear))
  cadre_headers <- cadre_headers[cadre_headers$RoleID!="Total",]
  
  ### Plot cadre headers
  cadreoplot <- ggplot(cadre_headers, aes(x = StartYear, y = RoleID, color= RoleID)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = glue('Headers in sheet: {unique(df$sheet_Cadre)}'),
         x ="StartYear",
         y = "RoleID",
         color ="RoleID")
  
  ### Check2: (EndYear+1) must appear in StartYear 
  StartYears <- df %>% select(StartYear) %>% unique()
  violation2 <- df %>% mutate(EndYear_plus = EndYear+1) %>%
    select(-StartYear) %>%
    filter(!is.na(EndYear)) %>%
    anti_join(StartYears, by=c("EndYear_plus" = "StartYear")) %>%
    select(-EndYear_plus)
  filename2 <- file.path(custom_dir, glue("violation_endyear_not_in_startyear_{unique(df$sheet_Cadre)}.csv"))
  description2 <- glue("For{unique(df$sheet_Cadre)}, (EndYear+1) must appear in StartYear Column.")
  if (nrow(violation2) >0 ){
    # Stop the check here as this is fatal
    .custom_check_write_result(description2, filename2, "error", violation2)
    log_print(glue::glue("Fatal error found in CadreRoles sheet: {description2}, unable to check further. Please fix it and rerun the validation."))
    return (cadreoplot)
  }
  
  ### Check3: "ScenarioID" + "RoleID" + "StartYear" must appear in the corresponding cadre_ sheets' headers
  bucket_rank <- as.data.frame(sort(unique(cadre_headers$StartYear), index.return=TRUE), col.names = c("StartYear", "Rank"))
  cadre_rank <- cadre_headers %>% 
    inner_join(bucket_rank) 
  violation3 <- df %>% anti_join(cadre_rank)
  filename3 <- file.path(custom_dir, glue("violation_startyear_not_in_header_{unique(df$sheet_Cadre)}.csv"))
  description3 <- glue("Sheet: {unique(df$sheet_Cadre)} is missing columns corresponding to the following pair: RoleID + StartYear")
  .custom_check_write_result(description3, filename3, "error", violation3)
  
  ### Check4: For each "ScenarioID" with no "EndYear" it must appear in the max(StartYear) section in the corresponding cadre_ sheets
  max_startYear <- max(df$StartYear)
  max_year_roles <- df %>% filter(is.na(EndYear)) %>%
    select(-StartYear) %>% 
    inner_join(cadre_rank, multiple = "all") %>%
    filter(StartYear == max_startYear) %>%
    select(RoleID) 
  violation4 <- df  %>% filter(is.na(EndYear)) %>% anti_join(max_year_roles)
  filename4 <- file.path(custom_dir, glue("violation_startyear_max_missing_{unique(df$sheet_Cadre)}.csv"))
  description4 <- glue("Sheet: {unique(df$sheet_Cadre)} is missing columns corresponding to the following RoleID + {max_startYear}")
  .custom_check_write_result(description4, filename4, "error", violation4)
  
  
  ### Check5: For each "RoleID", it should appear in continuous sections on the cadre_ sheets' headers in between StartYear and EndYear.
  expected <- df %>% 
    mutate(no_EndYear = if_else(is.na(EndYear), T , F)) %>%
    mutate(EndYear = if_else(is.na(EndYear), max(StartYear)-1 , EndYear)) %>%
    mutate(lastBucketYear = EndYear + 1) %>%
    select(RoleID, StartYear, lastBucketYear, no_EndYear)
  expected <- bind_cols(expected, 
                       expected %>%  #look up for rank for Start/End
                         mutate_all(~bucket_rank$Rank[match(., bucket_rank$StartYear)]) %>% 
                         select(-RoleID, -no_EndYear) %>% 
                         rename(c('StartYear' = 'StartRank', 'lastBucketYear' = "EndRank"))) %>%
    drop_na(StartRank, EndRank) %>%
    group_by(RoleID) %>% 
    mutate(EndRank = if_else(no_EndYear==T, EndRank, EndRank-1)) %>%
    complete(StartRank = StartRank:EndRank) %>%
    rename(c('StartRank' = "Rank")) %>%
    select(RoleID, Rank)
  
  ### expected to see the role appears in 1 to LastRank
  violation5 <- expected %>% anti_join(
    df %>%
      select(-StartYear) %>%
      inner_join(cadre_rank, multiple="all")
  ) %>% inner_join(df) %>%
    select(-StartYear)  %>% 
    inner_join(bucket_rank, by=c("Rank"="Rank")) %>%
    dplyr::rename(missing_bucket= StartYear) %>%
    select(-Rank)
  filename5 <- file.path(custom_dir, glue("roles_missing_years_{unique(df$sheet_Cadre)}.csv"))
  description5 <- glue("sheet {unique(df$sheet_Cadre)} is missing columns corresponding the following RoleID + missing_bucket")
  .custom_check_write_result(description5, filename5, "error", violation5)
  
  ### Check6: On each scenario-specific cadre_sheet, the pair "StartYear" + "RoleID" must appear in CadreRoles for the corresponding ScenarioID
  violation6 <- cadre_headers %>% anti_join(df, by=c("RoleID" = "RoleID")) %>% arrange({{StartYear}})
  description6 <- glue('On Sheet {unique(df$sheet_Cadre)}, the pair "StartYear" + "RoleID" must appear in CadreRoles for the corresponding ScenarioID')
  filename6 <- file.path(custom_dir, glue("no_definition_in_cadreroles_{unique(df$sheet_Cadre)}.csv"))
  .custom_check_write_result(description6, filename6, "error", violation6)
 
  if (all(sapply(2:6,FUN = \(x) {nrow(get(paste0("violation", x))) == 0}) == TRUE)){
    # if no violation objects are present, it means sheet is good
    log_print(glue::glue("Sheet: {unique(df$sheet_Cadre)} passed the CadreRoles checks!"))
  }
  return (cadreoplot)
  
}

.custom_check <- function(inputFile, logFile, outputDir = "log"){
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
  
  ##### Check if cadre sheets are valid
  defaultEndYear = pacehrh:::GPE$endYear
  defaultStartYear = pacehrh:::GPE$startYear
  cadreRoles <- read_xlsx(inputFile, sheet="CadreRoles")
  cadreRoles <- cadreRoles %>% 
    inner_join(scenarios, by=c('ScenarioID'='UniqueID')) %>%
    select(ScenarioID, RoleID, StartYear, EndYear, sheet_Cadre) %>%
    mutate(StartYear = as.numeric(StartYear), EndYear = as.numeric(EndYear))
  
  ### Check 1: columns StartYear and EndYear can not be outside of the model default range
  violation1 <- cadreRoles %>% filter(StartYear < defaultStartYear | EndYear > defaultEndYear)
  filename1 <- file.path(custom_dir, glue("violation_cadreroles_def.csv"))
  description1 <- glue("StartYear and EndYear can not be outside of the model default range: {defaultStartYear} to {defaultEndYear}.")
  if (nrow(violation1) >0 ){
    # Stop the check here as this is fatal
    .custom_check_write_result(description1, filename1, "error", violation1)
    log_print(glue::glue("Fatal error found in CadreRoles sheet: {description1}, unable to check further. Please fix it and rerun the validation."))
  }
  else{
    # Split by scenario as the cader_sheet is different
    # Check rule 2 to rule 6
    cadre_list <- split(cadreRoles, cadreRoles$sheet_Cadre)
    plot_list <-lapply(cadre_list, .cadre_check_scenarios, inputFile=inputFile, custom_dir=custom_dir)
    combined_cadre_plots <- do.call(grid.arrange, c(plot_list, nrow = length(plot_list)))
    ggsave(file.path(custom_dir, "custom_cadres.png"), combined_cadre_plots, width = 160, height = 60*length(plot_list), units="mm")
  }
  
  # writing metadata for custom check
  write.csv(custom_test$df_reason, file.path(custom_dir, "custom_validation_results.csv"))

}
