#' Load Experiment Scenario Information
#'
#' Read the experiment scenario information from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Data frame of experiment scenario parameters
#'
loadScenarios <- function(sheetName = "Scenarios"){
  scenarios <- readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetName)
  return(scenarios)
}

#' Initialize Experiment Scenario Information
#'
#' Read the healthcare task information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @param ... See \code{loadScenarios()}
#'
#' @export
#'
#' @return NULL (invisible)
#'
InitializeScenarios <- function(...){
  .checkAndLoadGlobalConfig()

  scenarios <- loadScenarios(...)

  # TODO: Insert error handling

  globalPackageEnvironment$scenarios <- scenarios
  invisible(NULL)
}
