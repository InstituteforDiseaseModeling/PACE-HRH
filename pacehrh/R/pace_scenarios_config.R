#' Load Experiment Scenario Information
#'
#' Read the experiment scenario information from the model inputs Excel file.
#' The name and location of the model inputs Excel file is loaded from the
#' global configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Data frame of experiment scenario parameters, or NULL on error
#'
loadScenarios <- function(sheetName = .defaultScenariosSheet) {
  traceMessage(paste0("Loading scenarios sheet ", sheetName))

  scenarios <- NULL

  scenarios <- readSheet(sheetName = sheetName)

  if (!is.null(scenarios)){
    scenarios <- validateTableAgainstSchema(scenarios, .scenarioMetaData)
  }

  return(scenarios)
}

#' Initialize Experiment Scenario Information
#'
#' Read experiment scenario information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @param loadFromExcel If TRUE, initialize the scenarios list from the model
#' inputs Excel file. If FALSE, initialize with a blank scenarios table.
#' @param ... See \code{loadScenarios()}
#'
#' @export
#'
#' @return NULL (invisible)
#'
#' @examples
#' \dontrun{
#' library(pacehrh)
#'
#' pacehrh::InitializePopulation()
#' pacehrh::InitializeScenarios()
#' pacehrh::InitializeStochasticParameters()
#' pacehrh::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#' }
InitializeScenarios <- function(loadFromExcel = TRUE, ...){
  .checkAndLoadGlobalConfig()

  if (loadFromExcel){
    scenarios <- loadScenarios(...)
  } else {
    scenarios <- CreateScenariosTable()
  }

  # TODO: Insert error handling

  GPE$scenarios <- scenarios
  invisible(NULL)
}
