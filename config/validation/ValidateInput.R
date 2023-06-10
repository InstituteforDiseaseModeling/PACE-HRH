options(install.packages.check.source = "no")
packages = c("validate","readxl", "plyr", "dplyr","ggplot2", "tidyr", "kableExtra", "stringr", "reshape2", "scales", "glue", "logr", "gridExtra")
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
Validate <- function(inputFile, outputDir, optional_sheets = NULL, rules_dir="config/validation/rules"){
  
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
  lf <- log_open(file_name = log_file, show_notes = FALSE, logdir = FALSE)
  result_d <- ValidateInputExcelFileContent(inputFile = inputFile, 
                                            logFile =lf, 
                                            outputDir = outputDir, 
                                            optional_sheets = optional_sheets,
                                            rules_dir=rules_dir)
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
                                          optional_sheets = NULL,
                                          rules_dir = "config/validation/rules"){
 
 
  input_sheetNames <- readxl::excel_sheets(path = inputFile)
  tryCatch(
    {
      errcode <- .Success
      checklist <- data.frame(
        sheet = character(),
        rule = character(),
        stringsAsFactors = FALSE)
      
      all_sheetNames <- gsub("^rules_([A-Za-z_0-9]+).yaml$", "\\1", list.files(rules_dir))
      
      # check all available rules
   
      # Identify rules to apply from scenario tab
      scenarios <- read_xlsx(inputFile, "Scenarios")
      checklist = rbind(checklist, scenarios %>% dplyr::mutate (rule = "rules_PopValues.yaml", sheet = sheet_PopValues ) %>% select(c(sheet, rule)) %>% unique())
      checklist = rbind(checklist, scenarios %>% dplyr::mutate (rule = "rules_SeasonalityCurves.yaml", sheet = sheet_SeasonalityCurves ) %>% select(c(sheet, rule)) %>% unique())
      checklist = rbind(checklist, scenarios %>% dplyr::mutate (rule = "rules_TaskValues_ref.yaml", sheet = sheet_TaskValues ) %>% select(c(sheet, rule)) %>% unique())
      
      for (f in list.files(rules_dir)){
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
          if (optional_sheets[i] %in% list.files(rules_dir)){
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
        
        f = file.path(rules_dir, checklist[[i, "rule"]])
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
            result <- result %>% dplyr::mutate (sheet_name = sheet)
          }
          else {
            new_result <- validate::as.data.frame(validate::summary(out))
            new_result <- new_result %>% dplyr::mutate (sheet_name = sheet)
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
    dplyr::mutate(result = dplyr::if_else(severity != 'error' & result == 'fails', severity, result))
  
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


.custom_check <- function(inputFile, logFile, outputDir = "log"){
  # define custom rules, for each rules, we choose to produce a graph (with custom_ prefix)
  # or a csv with name, description, severity written to "custom_validation_results.csv"
  custom_dir <- file.path(outputDir, "custom")
  dir.create(custom_dir, showWarnings = F)

  ##### Run plotting type validations
  ##### Functions prefix with "plot" should save plots in custom_dir
  check.env <- new.env()
  plot_check_files <- list.files(path=".", pattern="^(plot|check)_*", full.names=TRUE, recursive=TRUE)
  for(f in plot_check_files){
    sys.source(f,  envir = check.env)
  }
  functions <- lsf.str(envir=check.env)
  for (fun in functions){
    if (startsWith(fun, ".")){
      log_print(paste0("ignore check function: ", fun, " as this is treated as an internal function"))
      next
    }
    # Make sure the signature matches
    # the function must have "inputFile" as first argument and an optional "custom_dir" as the second argument
    arguments <- names(formals(fun, envir=check.env))
    if (length(arguments) > 2){
      log_print(paste0("ignore check function: ", fun, " as it does not have inputFile or custom_dir as arguments"))
      next
    }
    if ("inputFile" %in% arguments & "custom_dir" %in% arguments)
      result <- do.call(what = fun, args = list(inputFile, custom_dir), envir=check.env)
    else if (length(arguments) == 1 & "inputFile" %in% arguments){
      result <- do.call(what = fun, args = list(inputFile), envir=check.env)
    }
    if((is.list(result) && length(result) == 3)){
      # Functions prefix with "check" may return a list of (description, severity, violation_data) 
      filename <- file.path(custom_dir, paste0("violation_", fun, ".csv"))
      .custom_check_write_result(result[[1]], filename, result[[2]], result[[3]])
    }
  }
  rm(list = functions, envir = check.env)
  # writing metadata for custom check
  write.csv(custom_test$df_reason, file.path(custom_dir, "custom_validation_results.csv"))

}
