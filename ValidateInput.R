options(install.packages.check.source = "no")
packages = c("validate","readxl", "dplyr","ggplot2", "tidyr", "kableExtra")
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}
library(ehep)
.Success <- 0L
.errValidationRuleFailed <- -11L


#' Perform Validation on Input Excel File
#'
#' @description
#' Validate if all inputs comply with parameter rules
#'
#' @param inputFile Excel file to examine. If NULL, check the file defined in the global Configuration.
#' @param outputDir Results output directory. default is in log folder
#' @param sheetNames A vector of names of the sheets to be validate
#'
#' @return Error code.
#' 0 = Success
#' -1 = validation error
#'
#' @export
ValidateInputExcelFileContent <- function(inputFile,
                                          outputDir = "log",
                                          sheetNames = NULL){
  errcode <- .Success
  
  # check all available rules
  if (is.null(sheetNames)){
    sheetNames <- gsub("^rules_([A-Za-z_0-9]+).yaml$", "\\1", list.files("config/validation/rules"))
  }
  
  dir.create(outputDir, showWarnings = FALSE)
  
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
      rules <- validate::validator(.file=f)
      if (length(rules_combined)==0){
        rules_combined <- validate::as.data.frame(rules)
      }
      else {
        rules_combined <- rbind(rules_combined, validate::as.data.frame(rules))
      }
      
      data <- readxl::read_xlsx(inputFile, sheet = i)
      data_target <- data.frame(data)
      
      # Confront the rules and check if any error occurs
      out <- validate::confront(data_target, rules,  lin.ineq.eps=0, lin.eq.eps=1e-08)
      if(TRUE %in% validate::summary(out)$error){
        stop(paste("Some error occurred evaluating rules:", i))
      }
      # Apply rules and save the violation results
      check <- .get_violation_rows(out, data_target, rules, outputDir)
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
  write.csv(result, result_file)
  
  # summary and plot check result
  final_result <- rules_combined %>%
    dplyr::select(c("name", "severity")) %>%
    inner_join(result) %>%
    dplyr::select(c("name", "severity", "items", "fails", "passes", "expression")) %>%
    pivot_longer(cols=c("passes", "fails"), names_to = "result", values_to = "total") %>%
    mutate(result = dplyr::if_else(severity != 'error' & result == 'fails', severity, result))
  
  outfile <- file.path(outputDir, "input_validation_results.png")
  p <- ggplot(final_result, aes(x=total/items, y=name, fill=result)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("passes"="green", "fails"="red", "warning"="yellow", "info"="blue")) +
    scale_x_continuous(labels=scales::percent) +
    geom_text(data=subset(final_result,total> 0),ggplot2::aes(label=total, y =name), size=2, position=position_fill()) +
    ggtitle("Input Spreadsheet Validation Results")
  ggsave(outfile, p, width = 160, height = 100, units="mm")
  
  return (errcode)
}


.get_violation_rows <- function(out, target, rules, outputDir){
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
          df_violations <- validate::violating(target, out[i])
        },
        error=function(e){
          message(paste("Rule failed but No record-wise info: ", e))
          df_violations <- NULL
          return (df_violations)
        }
      )
      if (!is.null(df_violations) & nrow(df_violations) > 0){
        # only fail the validation for critical rules
        # severity <- validate::as.data.frame(rules) %>% dplyr::filter(name==i) %>% dplyr::select(severity)
       
        if (severity == "error"){
          errcode <- .errValidationRuleFailed
        }
        write.csv(df_violations, file.path(outputDir, paste(severity, "_violation_", i, ".csv", sep="")))
      }
    }
  }
  return (errcode)
}
