# The functions in this module control how EHEP handles stochastic experiments.

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
}

ZeroEpsilons <- function(){
  # Create default PopulationChangeParameters object
  pcp <- list(initValues = PopulationChangeParameters(),
              changeRates = PopulationChangeParameters())
  # Write into the 'epsilon' environment
  epsilonValuesEnvironment$populationChangeParameters <- pcp
}

SetExperimentValues <- function(){
  # TODO: Insert check that all the needed values exist

  baseVar <- baseValuesEnvironment$populationChangeParameters
  epsilonVar <- epsilonValuesEnvironment$populationChangeParameters
  pcp = list(initValues = add(baseVar$initValues, epsilonVar$initValues),
             changeRates = add(baseVar$changeRates, epsilonVar$changeRates))

  experimentValuesEnvironment$populationChangeParameters <- pcp
}
