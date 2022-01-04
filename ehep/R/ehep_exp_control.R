# The functions in this module control how EHEP handles stochastic experiments.


#' Save Base Values For A Stochastic Experiment
#'
#' Copy configuration parameters from the Global Package Environment to the
#' Base Values Environment.
#'
#' @export
#'
SaveBaseSettings <- function(){
  # baseValuesEnvironment$fertilityRates <-
  #   ifelse(exists("fertilityRates", where = globalPackageEnvironment),
  #          globalPackageEnvironment$fertilityRates, NULL)
  #
  # baseValuesEnvironment$mortalityRates <-
  #   ifelse(exists("mortalityRates", where = globalPackageEnvironment),
  #          globalPackageEnvironment$mortalityRates, NULL)
  #
  # baseValuesEnvironment$initialPopulation <-
  #   ifelse(exists("initialPopulation", where = globalPackageEnvironment),
  #          globalPackageEnvironment$initialPopulation, NULL)
  #
  # baseValuesEnvironment$initialPopulation <-
  #   ifelse(exists("initialPopulation", where = globalPackageEnvironment),
  #          globalPackageEnvironment$initialPopulation, NULL)
  #

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

  return(NULL)
}

#' Zero Epsilon Values
#'
#' Zero the configuration values in the Epsilon Values Environment. These
#' values are added to the corresponding values from the Base Values Environment
#' to produce varied parameters to model.
#'
#' The Epsilon Values Environment gets its name from the standard practice
#' of labelling statistical noise values with the variable name 'epsilon'
#'
#' @export
#'
ZeroEpsilons <- function(){
  pcp <- list(initValues = PopulationChangeParameters(),
              changeRates = PopulationChangeParameters())
  epsilonValuesEnvironment$populationChangeParameters <- pcp

  pp <- list(female = PopulationPyramid(),
             male = PopulationPyramid(),
             total = PopulationPyramid())
  epsilonValuesEnvironment$initialPopulation <- pp

  return(NULL)
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

  baseVar <- bve$populationChangeParameters
  epsilonVar <- eve$populationChangeParameters
  pcp = list(initValues = add(baseVar$initValues, epsilonVar$initValues),
             changeRates = add(baseVar$changeRates, epsilonVar$changeRates))
  eve$populationChangeParameters <- pcp

  baseVar <- bve$initialPopulation
  epsilonVar <- eve$initialPopulation
  pp = list(female = add(baseVar$female, epsilonVar$female),
            male = add(baseVar$male, epsilonVar$female),
            total = add(baseVar$total, epsilonVar$total))
  eve$initialPopulation <- pp
}
