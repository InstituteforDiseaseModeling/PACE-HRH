#' Initialize PACE-HRH
#'
#' @param globalConfigFile stuff stuff stuff
#' @param inputFile One entry from the results structure as returned by
#'   \code{RunExperiments()}
#' @param populationSheet stuff stuff stuff
#' @param forceGlobalConfigReload TRUE/FALSE, default = TRUE. Force a reload of the global configuration if `globalConfigFile` is specified.
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

  if (is.null(populationSheet)) {
    InitializePopulation()
  } else {
    InitializePopulation(popSheet = populationSheet)
  }

  InitializeScenarios()
  InitializeStochasticParameters()
  InitializeSeasonality()

  return(TRUE)
}
