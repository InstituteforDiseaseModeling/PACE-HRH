#' Load Cadre Roles
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Dataframe of cadre roles information
#'
loadCadreRoles <- function(sheetName = .defaultCadreRolesSheet){
  traceMessage(paste0("Loading cadre roles sheet ", sheetName))

  cadreData <- loadTable(sheet = sheetName, schema = .cadreRolesMetaData)

  return(cadreData)
}

#' Initialize Cadre Roles
#'
#' Read the cadre roles information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @md
#' @param ... Parameters passed through to [loadCadreRoles()]
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
#' pacehrh::InitializeCadreRoles()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#' }
InitializeCadreRoles <- function(...){
  .checkAndLoadGlobalConfig()

  cadreRolesData <- loadCadreRoles(...)

  BVE$cadreRoles <- cadreRolesData
  return(invisible(NULL))
}
