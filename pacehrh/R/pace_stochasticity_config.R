#' Load Stochastic Parameters
#'
#' @param stochasticParametersSheetName Sheet name from the model input Excel file
#'
#' @return Dataframe of stochastic parameters
#'
#' @noRd
loadStochasticParameters <- function(stochasticParametersSheetName = .defaultStochasticParametersSheet) {
  traceMessage(paste0("Loading stochastic parameters sheet ", stochasticParametersSheetName))

  stochData <- tryCatch({
    readxl::read_xlsx(GPE$inputExcelFile, sheet = stochasticParametersSheetName)
  },
  error = function(e){
    return(NULL)
  },
  finally = {
  })

  if (is.null(stochData)){
    warning("Could not read stochastic parameters sheet")
    return(NULL)
  }

  stochData <-
    validateTableAgainstSchema(stochData,
                               .stochasticParametersMetaData,
                               convertType = FALSE)

  return(stochData)
}

#' Load Change Rate Limits
#'
#' @param changeRateLimitsSheetName Sheet name from the model input Excel file
#'
#' @return Dataframe of change rate limits
#'
#' @noRd
loadChangeRateLimits <- function(changeRateLimitsSheetName = .defaultChangeRateLimitsSheet) {
  traceMessage(paste0("Loading change rate limits sheet ", changeRateLimitsSheetName))

  limitData <- tryCatch({
    readxl::read_xlsx(GPE$inputExcelFile, sheet = changeRateLimitsSheetName)
  },
  error = function(e){
    return(NULL)
  },
  finally = {
  })

  if (is.null(limitData)){
    warning("Could not read change rate limits sheet")
    return(NULL)
  }

  limitData <-
    validateTableAgainstSchema(limitData,
                               .changeRateLimitsMetaData,
                               convertType = FALSE)

  return(limitData)
}

#' Initialize Stochastic Parameters
#'
#' Read the stochasticity information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @md
#' @param ... Parameters passed through to loadStochasticParameters()
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
InitializeStochasticParameters <- function(stochasticParametersSheetName = .defaultStochasticParametersSheet,
                                           changeRateLimitsSheetName = .defaultChangeRateLimitsSheet) {
  .checkAndLoadGlobalConfig()

  BVE$stochasticParams <- loadStochasticParameters(stochasticParametersSheetName)
  BVE$changeRateLimits <- loadChangeRateLimits(changeRateLimitsSheetName)
  return(invisible(NULL))
}
