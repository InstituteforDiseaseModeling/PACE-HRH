#' Initialize PACE-HRH
#'
#' @param globalConfigFile Path to the global configuration JSON file
#' @param inputFile Path to the model inputs Excel file
#' @param populationSheet Baseline population sheet name
#' @param scenariosSheet Scenarios sheet name
#' @param stochasticParametersSheet Stochastic parameters sheet name
#' @param seasonalityCurvesSheet Seasonality curves sheet name
#' @param seasonalityOffsetsSheet Seasonality offsets sheet name
#' @param forceGlobalConfigReload TRUE/FALSE, default = TRUE. Force a reload of
#'   the global configuration if `globalConfigFile` is specified.
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' \dontrun{
#` pacehrh::PaceInitialize()
#' }
PaceInitialize <- function(globalConfigFile = NULL,
                           inputFile = NULL,
                           populationSheet = NULL,
                           scenariosSheet = NULL,
                           stochasticParametersSheet = NULL,
                           seasonalityCurvesSheet = NULL,
                           seasonalityOffsetsSheet = NULL,
                           forceGlobalConfigReload = TRUE) {
  if (!is.null(inputFile)) {
    result <- SetInputExcelFile(inputFile)
    if (result == FALSE) {
      return(FALSE)
    }
  }

  # Reload the global configuration if either (1) configuration data has not
  # already been loaded, or (2) the forceGlobalConfigReload flag is set
  if (!GPE$globalConfigLoaded | forceGlobalConfigReload == TRUE) {
    if (!is.null(globalConfigFile)) {
      loadGlobalConfig(path = globalConfigFile)
    } else {
      loadGlobalConfig()
    }
    GPE$globalConfigLoaded <- TRUE
  }

  # Load populations sheet
  if (is.null(populationSheet)) {
    InitializePopulation()
  } else {
    InitializePopulation(popSheet = populationSheet)
  }

  # Load scenarios sheet
  if (is.null(scenariosSheet)) {
    InitializeScenarios()
  } else {
    InitializeScenarios(sheetName = scenariosSheet)
  }

  # Load stochastic parameters sheet
  if (is.null(stochasticParametersSheet)) {
    InitializeStochasticParameters()
  } else {
    InitializeStochasticParameters(sheetName = stochasticParametersSheet)
  }

  # Local seasonality curves sheet and seasonality offsets sheets
  args <- list()
  if (!is.null(seasonalityCurvesSheet)) {
    args$sheetNameCurves <- seasonalityCurvesSheet
  }

  if (!is.null(seasonalityOffsetsSheet)) {
    args$sheetNameOffsets <- seasonalityOffsetsSheet
  }

  do.call(InitializeSeasonality, args)

  return(TRUE)
}
