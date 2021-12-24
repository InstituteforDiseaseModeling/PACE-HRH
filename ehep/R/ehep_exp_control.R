# The functions in this module control how EHEP handles stochastic experiments.

SaveBaseSettings <- function(){
  baseValuesEnvironment$fertilityRates <-
    ifelse(exists("fertilityRates", where = globalPackageEnvironment),
           globalPackageEnvironment$fertilityRates, NULL)

  baseValuesEnvironment$mortalityRates <-
    ifelse(exists("mortalityRates", where = globalPackageEnvironment),
           globalPackageEnvironment$mortalityRates, NULL)

  baseValuesEnvironment$initialPopulation <-
    ifelse(exists("initialPopulation", where = globalPackageEnvironment),
           globalPackageEnvironment$initialPopulation, NULL)

  baseValuesEnvironment$initialPopulation <-
    ifelse(exists("initialPopulation", where = globalPackageEnvironment),
           globalPackageEnvironment$initialPopulation, NULL)

  baseValuesEnvironment$populationChangeParameters <-
    ifelse(exists("populationChangeParameters", where = globalPackageEnvironment),
           globalPackageEnvironment$populationChangeParameters, NULL)
}
