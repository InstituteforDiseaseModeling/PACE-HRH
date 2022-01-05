#
# Create and initialize an R environment for the package.
# The environment is created at the GlobalEnvironment level so its location
# is unambiguous.
#

globalPackageEnvironment <- new.env(parent = emptyenv())

globalPackageEnvironment$globalConfigLoaded <- FALSE

globalPackageEnvironment$traceState <- FALSE
globalPackageEnvironment$inputExcelFile <- "./config/R Model Inputs.xlsx"

globalPackageEnvironment$startYear <- 2020
globalPackageEnvironment$endYear <- 2040
globalPackageEnvironment$years <- seq(from = globalPackageEnvironment$startYear,
                                      to = globalPackageEnvironment$endYear,
                                      by = 1)

globalPackageEnvironment$age_min <- 0
globalPackageEnvironment$age_max <- 100
globalPackageEnvironment$ages <- seq(from = globalPackageEnvironment$age_min,
                                     to = globalPackageEnvironment$age_max,
                                     by = 1)

globalPackageEnvironment$ratio_females_at_birth <- 0.5
globalPackageEnvironment$ratio_males_at_birth <- 1.0 - globalPackageEnvironment$ratio_females_at_birth

globalPackageEnvironment$initialPopulation <- NULL
globalPackageEnvironment$populationChangeParameters <- NULL
globalPackageEnvironment$mortalityRates <- NULL
globalPackageEnvironment$fertilityRates <- NULL

globalPackageEnvironment$taskData <- NULL

baseValuesEnvironment <- new.env(parent = globalPackageEnvironment)
epsilonValuesEnvironment <- new.env(parent = globalPackageEnvironment)
experimentValuesEnvironment <- new.env(parent = globalPackageEnvironment)
