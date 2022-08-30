# The functions in this module control how EHEP handles stochastic experiments.

#' Save Base Values For A Stochastic Experiment
#'
#' Copy configuration parameters from the Global Package Environment to the
#' Base Values Environment.
#'
#' @param scenarioName Scenario name string
#'
#' @return Scenario information, or NULL if the scenario is invalid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ehep)
#'
#' ehep::InitializePopulation()
#' ehep::InitializeHealthcareTasks()
#' ehep::InitializeScenarios()
#' ehep::InitializeStochasticParameters()
#' ehep::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' result <- ehep::SaveBaseSettings(scenario)
#' }
SaveBaseSettings <- function(scenarioName = ""){
  # GPE = globalPackageEnvironment = source environment
  # BVE = baseValuesEnvironment = destination environment

  .zeroExpBaseVariables()

  BVE$scenario <- .getScenarioConfig(scenarioName)

  if (is.null(BVE$scenario)){
    return(NULL)
  }

  # Load Population change parameter data from the appropriate Excel sheet, as specified
  # in the Scenarios sheet.

  popValsSheet <- BVE$scenario$sheet_PopValues

  if (!is.blank(popValsSheet)) {
    BVE$populationChangeRates <- loadPopulationChangeRates(popValsSheet)
  } else {
    BVE$populationChangeRates <- loadPopulationChangeRates()
  }

  # Load Seasonality parameter data from the appropriate Excel sheet, as specified
  # in the Scenarios sheet.

  seasonalitySheet <- BVE$scenario$sheet_SeasonalityCurves

  if (!is.blank(seasonalitySheet)){
    GPE$seasonalityCurves <- loadSeasonalityCurves(seasonalitySheet)
  } else {
    GPE$seasonalityCurves <- loadSeasonalityCurves()
  }

  if (exists("initialPopulation", where = GPE)){
    BVE$initialPopulation <- GPE$initialPopulation
  }

  # Load Task parameter data from the appropriate Excel sheet, as specified
  # in the Scenarios sheet.

  taskSheet <- BVE$scenario$sheet_TaskValues

  if (!is.blank(taskSheet)){
    GPE$taskData <- loadTaskParameters(taskSheet)
  } else {
    GPE$taskData <- loadTaskParameters()
  }

  if (!is.null(GPE$taskData)) {
    GPE$taskDataDims <- dim(GPE$taskData)
    GPE$stochasticTasks <- which(GPE$taskData$applyStochasticity)
    m <- .convertTaskDfToMatrix(GPE$taskData)
    BVE$taskParameters <- TaskParameters(values = m)
  }

  return(BVE$scenario)
}

.zeroExpBaseVariables <- function(){
  BVE$scenario <- NULL
  BVE$initialPopulation <- NULL
  BVE$taskParameters <- NULL
}

.getScenarioConfig <- function(scenarioName){
  n <- which(GPE$scenarios$UniqueID == scenarioName)

  if (length(n) == 0) {
    traceMessage(paste("Could not find scenario ", scenarioName, sep = ""))
    return(NULL)
  }

  if (length(n) > 1) {
    traceMessage(paste("More than one scenario ", scenarioName, ". Using first one.", sep = ""))
  }

  return(GPE$scenarios[n[1], ])
}

.taskDataCols <- c("StartingRateInPop",
                   "RateMultiplier",
                   "AnnualDeltaRatio",
                   "NumContactsPerUnit",
                   "NumContactsAnnual",
                   "MinsPerContact",
                   "HoursPerWeek",
                   "FTEratio")

.convertTaskDfToMatrix <- function(df){
  # Extract numeric data columns from a taskData dataframe as read from the
  # configuration Excel file
  rowNames <- df$Indicator
  df <- df[.taskDataCols]

  # Convert to a matrix with labeled rows (task types) and columns (variables)
  m <- as.matrix(df)
  row.names(m) <- rowNames

  return(m)
}

#' Generate A New Set Of Stochastic Variations
#'
#' Create the configuration values for an individual model experiment.
#'
#' @return NULL (invisible)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ehep)
#' ehep::Trace(TRUE)
#'
#' ehep::InitializePopulation()
#' ehep::InitializeHealthcareTasks()
#' ehep::InitializeScenarios()
#' ehep::InitializeStochasticParameters()
#' ehep::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' set.seed(54321)
#'
#' scenarioData <- SaveBaseSettings(scenario)
#' ConfigureExperimentValues()
#' results <- RunExperiment()
#' }
ConfigureExperimentValues <- function(){
  # TODO: Insert check that all the needed values exist

  pcr <- BVE$populationChangeRates

  for (label in names(pcr)){
    m <- generateRatesMatrix(label)
    pcr[[label]]$ratesMatrix <- m
  }

  EXP$populationChangeRates <- pcr
  EXP$initialPopulation <- BVE$initialPopulation
  EXP$taskParameters <- varyTaskValues(BVE$taskParameters)
  EXP$prevalenceRatesMatrix <- generatePrevalenceRatesMatrix()

  return(invisible(NULL))
}
