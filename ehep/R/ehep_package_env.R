#
# Create and initialize an R environment for the package.
# The environment is created at the GlobalEnvironment level so its location
# is unambiguous.
#

globalPackageEnvironment <- new.env(parent = emptyenv())
baseValuesEnvironment <- new.env(parent = globalPackageEnvironment)
epsilonValuesEnvironment <- new.env(parent = globalPackageEnvironment)
experimentValuesEnvironment <- new.env(parent = globalPackageEnvironment)

# Aliases for environments
GPE <- globalPackageEnvironment
BVE <- baseValuesEnvironment
EPS <- epsilonValuesEnvironment
EXP <- experimentValuesEnvironment

GPE$globalConfigLoaded <- FALSE

GPE$traceState <- FALSE
GPE$inputExcelFile <- "./config/R Model Inputs.xlsx"

GPE$startYear <- 2020
GPE$endYear <- 2040
GPE$years <- seq(from = GPE$startYear,
                 to = GPE$endYear,
                 by = 1)

GPE$age_min <- 0
GPE$age_max <- 100
GPE$ages <- seq(from = GPE$age_min,
                to = GPE$age_max,
                by = 1)

GPE$ratio_females_at_birth <- 0.5
GPE$ratio_males_at_birth <- 1.0 - GPE$ratio_females_at_birth

GPE$initialPopulation <- NULL
GPE$populationChangeParameters <- NULL
GPE$taskData <- NULL
GPE$scenarios <- NULL
