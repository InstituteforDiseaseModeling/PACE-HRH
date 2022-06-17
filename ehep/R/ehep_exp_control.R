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

  if (!is.blank(popValsSheet)){
    GPE$populationChangeParameters <- loadPopulationChangeParameters(popValsSheet)
  } else {
    GPE$populationChangeParameters <- loadPopulationChangeParameters()
  }

  if (!is.null(GPE$populationChangeParameters)) {
    BVE$populationChangeParameters <- GPE$populationChangeParameters
  }


  # Load Population change parameter data from the appropriate Excel sheet, as specified
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
  BVE$populationChangeParameters <- NULL
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

.createZeroTaskParametersObject <- function(){
  m <- matrix(0.0,
              nrow = GPE$taskDataDims[1],
              ncol = length(.taskDataCols))

  # TODO: If necessary, add row/column labels.

  return(TaskParameters(values = m))
}

.createZeroPopulationChangeParametersList <- function(){
  return(
    list(initValues = PopulationChangeParameters(),
         changeRates = PopulationChangeParameters())
  )
}

.createZeroPopulationPyramidList <- function(){
  return(
    list(
      age = GPE$ages,
      female = PopulationPyramid(),
      male = PopulationPyramid(),
      total = PopulationPyramid()
    )
  )
}

#' Zero Epsilon Values
#'
#' Zero the configuration values in the Epsilon Values Environment. These
#' values are added to the corresponding values from the Base Values Environment
#' to produce varied parameters to model.
#'
#' The Epsilon Values Environment gets its name from the practice
#' of labeling statistical noise values with the variable name 'epsilon'
#'
#' @export
#'
ZeroEpsilons <- function(){
  EPS$populationChangeParameters <- .createZeroPopulationChangeParametersList()
  EPS$initialPopulation <- .createZeroPopulationPyramidList()
  EPS$taskParameters <- .createZeroTaskParametersObject()

  EPS$fertilityRatesMatrix <- NULL
  EPS$mortalityRatesMatrix <- NULL
  EPS$prevalenceRatesMatrix <- NULL

  return(invisible(NULL))
}

#' Initilialize Stochastic Variation System
#'
#' @return NULL (invisible)
#'
#' @export
InitializeEpsilons <- function(){
  set.seed(12345)
  ZeroEpsilons()
  return(invisible(NULL))
}

#' Generate A New Set Of Stochastic Variations
#'
#' @return NULL (invisible)
#'
#' @export
NextEpsilons <- function(){
  mf <- generateFertilityRatesMatrix()
  mm <- generateMortalityRatesMatrix()
  mp <- generatePrevalenceRatesMatrix()

  EPS$fertilityRatesMatrix <- mf
  EPS$mortalityRatesMatrix <- mm
  EPS$prevalenceRatesMatrix <- mp

  tp <- generateTaskParameterEpsilons(BVE$taskParameters)
  EPS$taskParameters <- tp

  return(invisible(NULL))
}

#' Combine Base and Epsilon Values
#'
#' Create the configuration values for an individual model experiment by
#' adding together values from the Base Values Environment and the Epsilon
#' Values Environment.
#'
#' @export
#'
ConfigureExperimentValues <- function(){
  # TODO: Insert check that all the needed values exist

  # Combine populationChangeParameters values and copy to experimentValuesEnvironment
  bVar <- BVE$populationChangeParameters
  eVar <- EPS$populationChangeParameters
  pcp = list(initValues = add(bVar$initValues, eVar$initValues),
             changeRates = add(bVar$changeRates, eVar$changeRates))
  EXP$populationChangeParameters <- pcp

  # Combine initialPopulation values and copy to experimentValuesEnvironment
  bVar <- BVE$initialPopulation
  eVar <- EPS$initialPopulation
  pp = list(age = GPE$ages,
            female = add(bVar$female, eVar$female),
            male = add(bVar$male, eVar$female),
            total = add(bVar$total, eVar$total))
  EXP$initialPopulation <- pp

  # Copy taskParameters values (computed in NextEpsilons()) to
  # experimentValuesEnvironment. (No adding things together.)
  EXP$taskParameters <- EPS$taskParameters

  # Copy the rates matrices (computed in NextEpsilons())
  # to experimentValuesEnvironment. (No adding things together.)
  EXP$fertilityRatesMatrix = EPS$fertilityRatesMatrix
  EXP$mortalityRatesMatrix = EPS$mortalityRatesMatrix
  EXP$prevalenceRatesMatrix = EPS$prevalenceRatesMatrix

  invisible(NULL)
}
