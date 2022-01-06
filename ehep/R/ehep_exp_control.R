# The functions in this module control how EHEP handles stochastic experiments.

#' Save Base Values For A Stochastic Experiment
#'
#' Copy configuration parameters from the Global Package Environment to the
#' Base Values Environment.
#'
#' @export
#'
SaveBaseSettings <- function(){
  envs <- globalPackageEnvironment
  envd <- baseValuesEnvironment

  if (exists("populationChangeParameters", where = envs)){
    envd$populationChangeParameters <- envs$populationChangeParameters
  } else {
    envd$populationChangeParameters <- NULL
  }

  if (exists("initialPopulation", where = envs)){
    envd$initialPopulation <- envs$initialPopulation
  } else {
    envd$initialPopulation <- NULL
  }

  if (exists("taskData", where = envs)){
    m <- .convertTaskDfToMatrix(envs$taskData)
    envd$taskParameters <- TaskParameters(values = m)
  } else {
    envd$taskParameters <- NULL
  }

  invisible(NULL)
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
              nrow = globalPackageEnvironment$taskDataDims[1],
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
      age = globalPackageEnvironment$ages,
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
  eve <- epsilonValuesEnvironment

  eve$populationChangeParameters <- .createZeroPopulationChangeParametersList()
  eve$initialPopulation <- .createZeroPopulationPyramidList()
  eve$taskParameters <- .createZeroTaskParametersObject()

  invisible(NULL)
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

  bve <- baseValuesEnvironment
  eve <- epsilonValuesEnvironment
  exp <- experimentValuesEnvironment

  # Combine populationChangeParameters values and copy to experimentValuesEnvironment
  bVar <- bve$populationChangeParameters
  eVar <- eve$populationChangeParameters
  pcp = list(initValues = add(bVar$initValues, eVar$initValues),
             changeRates = add(bVar$changeRates, eVar$changeRates))
  exp$populationChangeParameters <- pcp

  # Combine initialPopulation values and copy to experimentValuesEnvironment
  bVar <- bve$initialPopulation
  eVar <- eve$initialPopulation
  pp = list(age = globalPackageEnvironment$ages,
            female = add(bVar$female, eVar$female),
            male = add(bVar$male, eVar$female),
            total = add(bVar$total, eVar$total))
  exp$initialPopulation <- pp

  # Combine taskParameters values and copy to experimentValuesEnvironment
  bVar <- bve$taskParameters
  eVar <- eve$taskParameters
  exp$taskParameters <- add(bVar, eVar)

  invisible(NULL)
}
