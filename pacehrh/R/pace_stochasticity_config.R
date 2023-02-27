#' Load Stochastic Parameters
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Dataframe of stochastic parameters
#'
loadStochasticParameters <- function(sheetName = .defaultStochasticParametersSheet){
  traceMessage(paste0("Loading stochastic parameters sheet ", sheetName))
  
  stochData <- readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)

  # Keep the first three columns
  stochData <- stochData[1:3]
  return(stochData)
}

#' Initialize Stochastic Parameters
#'
#' Read the stochasticity information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @md
#' @param ... Parameters passed through to [loadStochasticParameters()]
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
InitializeStochasticParameters <- function(...){
  .checkAndLoadGlobalConfig()

  stochData <- loadStochasticParameters(...)

  # TODO: Insert error handling

  BVE$stochasticParams <- stochData
  return(invisible(NULL))
}
