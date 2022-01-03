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

  if (exists("initialPopulation", where = envs)){
    envd$initialPopulation <- envs$initialPopulation
  } else {
    envd$initialPopulation <- NULL
  }
}

ZeroEpsilons <- function(){
  # Create default PopulationChangeParameters object
  pcp <- list(initValues = PopulationChangeParameters(),
              changeRates = PopulationChangeParameters())
  # Write into the 'epsilon' environment
  epsilonValuesEnvironment$populationChangeParameters <- pcp

  # Create default PopulationPyramid object
  pp <- list(female = PopulationPyramid(),
             male = PopulationPyramid(),
             total = PopulationPyramid())
  # Write into the 'epsilon' environment
  epsilonValuesEnvironment$initialPopulation <- pp
}

SetExperimentValues <- function(){
  # TODO: Insert check that all the needed values exist

  baseVar <- baseValuesEnvironment$populationChangeParameters
  epsilonVar <- epsilonValuesEnvironment$populationChangeParameters
  pcp = list(initValues = add(baseVar$initValues, epsilonVar$initValues),
             changeRates = add(baseVar$changeRates, epsilonVar$changeRates))
  experimentValuesEnvironment$populationChangeParameters <- pcp

  baseVar <- baseValuesEnvironment$initialPopulation
  epsilonVar <- epsilonValuesEnvironment$initialPopulation
  pp = list(female = add(baseVar$female, epsilonVar$female),
            male = add(baseVar$male, epsilonVar$female),
            total = add(baseVar$total, epsilonVar$total))
  experimentValuesEnvironment$initialPopulation <- pp
}
