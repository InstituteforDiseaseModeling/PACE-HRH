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
  # Check for tables that should have been loaded during initialization
  if (.checkForBaseTables() == FALSE){
    return(NULL)
  }

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

  if (is.null(BVE$populationChangeRates)){
    return(NULL)
  }

  # Load Seasonality parameter data from the appropriate Excel sheet, as specified
  # in the Scenarios sheet.

  seasonalitySheet <- BVE$scenario$sheet_SeasonalityCurves

  if (!is.blank(seasonalitySheet)){
    BVE$seasonalityCurves <- loadSeasonalityCurves(seasonalitySheet)
  } else {
    BVE$seasonalityCurves <- loadSeasonalityCurves()
  }

  if (is.null(BVE$seasonalityCurves)){
    return(NULL)
  }

  # Load Task parameter data from the appropriate Excel sheet, as specified
  # in the Scenarios sheet.

  taskSheet <- BVE$scenario$sheet_TaskValues

  if (!is.blank(taskSheet)){
    BVE$taskData <- loadTaskParameters(taskSheet)
  } else {
    BVE$taskData <- loadTaskParameters()
  }

  if (is.null(BVE$taskData)){
    return(NULL)
  }

  # Check that all the population labels in the tasks list are included in
  # the populationLabels lookup. (This connection is also enforced by logic
  # in the input spreadsheet.)
  #
  # Note that this test will fail if the Lookup table wasn't loaded during
  # initialization.

  s <- setdiff(BVE$taskData$RelevantPop, BVE$populationLabels$Labels)
  if (!.okLabels(s)){
    warning(paste0("Invalid population labels: ", paste0(s, collapse = ", ")))
    return(NULL)
  }

  # Set up baseline task data

  if (!is.null(BVE$taskData)) {
    BVE$taskDataDims <- dim(BVE$taskData)
    BVE$stochasticTasks <- which(BVE$taskData$applyStochasticity)
    m <- .convertTaskDfToMatrix(BVE$taskData)
    BVE$taskParameters <- m

    # Add a task data column pointing into the population range mask tables
    # associated with each RelevantPop range
    index <- sapply(BVE$taskData$RelevantPop, function(label){
      which(rownames(BVE$populationRangesTable$Female) == label)
    })

    BVE$taskData$popRangeMaskPtr <- as.vector(index)

    # Generate a list of tasks affected by seasonality
    tpIds <- dimnames(BVE$taskParameters)[[1]]
    soIds <- BVE$seasonalityOffsets$Task
    BVE$seasonalTasks <- intersect(soIds, tpIds)
  }

  # Set the year range for trials, which is just the specified year range
  # extended by a GPE$shoulderYears years to correct for seasonality edge effects.
  .setTrialYears()

  # Merge seasonality curves into the seasonality offsets table
  .mergeSeasonalityCurves()

  # Compute cadre member overhead times, etc
  BVE$cadreData <- computeCadreData(BVE$scenario, BVE$cadreRoles)

  # Load Cadre parameter data from the appropriate Excel sheet, as specified
  # in the Scenarios sheet.
  taskCadresSheet <- BVE$scenario$sheet_Cadre

  if (!is.blank(taskCadresSheet)){
    BVE$taskCadresData <- loadTaskCadres(taskCadresSheet)
  } else {
    BVE$taskCadresData <- loadTaskCadres()
  }

  return(BVE$scenario)
}

.checkForBaseTables <- function(){
  varsToCheck <- c(expression(GPE$scenarios),
                   expression(BVE$seasonalityOffsets),
                   expression(BVE$stochasticParams),
                   expression(BVE$initialPopulation),
                   expression(BVE$populationLabels),
                   expression(BVE$populationRangesTable)
                   )

  checks <- sapply(varsToCheck, function(v){
    return(!is.null(eval(v)))
  })

  if (all(checks)){
    return(TRUE)
  }

  varNames <- sapply(varsToCheck, deparse)
  badVars <- varNames[checks == FALSE]
  badVarsStr <- paste(badVars, collapse = ", " )

  errorMsg <- paste0("Uninitialized variables: ", badVarsStr)
  warning(errorMsg, call. = FALSE)

  return(FALSE)
}

# Extend the SeasonalityOffsets table with the seasonality curve values from the
# SeasonalityCurves table. This facilitates easier lookups later on.
.mergeSeasonalityCurves <- function(){
  seasonalityCurvesTable <- BVE$seasonalityCurves
  curveCols <- 2:length(seasonalityCurvesTable)
  monthNames <- seasonalityCurvesTable$Month
  curveNames <- names(seasonalityCurvesTable)[curveCols]

  # Convert [months x curves] table to [curves x months] table
  tsc <- t(seasonalityCurvesTable[curveCols])
  colnames(tsc) <- monthNames
  tsc <- tibble::as_tibble(tsc)
  tsc$Type <- curveNames

  BVE$seasonalityOffsetsEx <- merge(BVE$seasonalityOffsets, tsc, by.x = "Curve", by.y = "Type")
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
                   "HoursPerWeek")

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

#' Compute Time-Series of Population Rate Parameters
#'
#' @param pcr Population Change Rates structure
#' @param years Range of years to compute
#' @param stochasticParms Parameters controlling stochastic variation of generated rates
#'
#' @return Population Change Rates structure with predicted rates matrices added
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' initPop <- pacehrh:::loadInitialPopulation(sheetName = "Flat_Population")
#' pcr <- pacehrh:::loadPopulationChangeRates(sheetName = "Flat_Rates")
#' pars <- pacehrh:::loadStochasticParameters(sheetName = "Flat_StochasticParms")
#' years <- 2020:2040
#' pcr <- pacehrh:::addRatesMatricesToPopulationChangeRates(pcr, years, NULL)
#' }
addRatesMatricesToPopulationChangeRates <-
  function(pcr, years, stochasticParms = NULL) {
    if (is.null(stochasticParms)) {
      stochasticityFlag <- FALSE
    } else {
      stochasticityFlag <- TRUE
    }

    for (label in names(pcr)) {
      m <-
        .generateRatesMatrix(stochasticParms,
                             years,
                             pcr[[label]],
                             stochasticity = stochasticityFlag)
      pcr[[label]]$ratesMatrix <- m
    }

    return(pcr)
  }
