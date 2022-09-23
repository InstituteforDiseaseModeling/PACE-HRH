# The functions in this module control how PACE-HRH handles stochastic experiments.

#' Save Base Values For A Stochastic Experiment
#'
#' Copy configuration parameters from the Global Package Environment to the
#' Base Values Environment.
#'
#' @param scenarioName Scenario name string
#'
#' @return Scenario information, or NULL in case of error (such as an invalid
#' scenario)
#'
#' @export
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
#' result <- pacehrh::SaveBaseSettings(scenario)
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
    BVE$taskData <- loadTaskParameters(taskSheet)
  } else {
    BVE$taskData <- loadTaskParameters()
  }

  # Check that all the population labels in the tasks list are included in
  # the populationLabels lookup. (This connection is also enforced by logic
  # in the input spreadsheet.)
  #
  # Note that this test will fail if the Lookup table wasn't loaded during
  # initialization.

  s <- setdiff(BVE$taskData$RelevantPop, GPE$populationLabels$Labels)
  if (!.okLabels(s)){
    warning(paste0("Invalid population labels: ", paste0(s, collapse = ", ")))
    return(NULL)
  }

  # Set up baseline task data

  if (!is.null(BVE$taskData)) {
    BVE$taskDataDims <- dim(BVE$taskData)
    BVE$stochasticTasks <- which(BVE$taskData$applyStochasticity)
    m <- .convertTaskDfToMatrix(BVE$taskData)
    BVE$taskParameters <- TaskParameters(values = m)
  }

  # Set the year range for trials, which is just the specified year range
  # extended by a year to correct for seasonality edge effects.
  .setTrialYears()

  return(BVE$scenario)
}

.setTrialYears <- function(){
  BVE$startYear <- GPE$startYear
  BVE$endYear <- GPE$endYear + GPE$shoulderYears
  BVE$years <- seq(BVE$startYear, BVE$endYear, 1)
}

.okLabels <- function(diffOutput){
  if (length(diffOutput) == 0){
    return(TRUE)
  }

  return(FALSE)
}

.zeroExpBaseVariables <- function(){
  BVE$scenario <- NULL
  BVE$initialPopulation <- NULL
  BVE$taskParameters <- NULL
  BVE$taskData <- NULL
  BVE$taskDataDims <- NULL
  BVE$stochasticTasks <- NULL
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
#' library(pacehrh)
#' pacehrh::Trace(TRUE)
#'
#' pacehrh::InitializePopulation()
#' pacehrh::InitializeScenarios()
#' pacehrh::InitializeStochasticParameters()
#' pacehrh::InitializeSeasonality()
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
