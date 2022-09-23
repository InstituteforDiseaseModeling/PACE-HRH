#
# Create and initialize an R environment for the package.
#

globalPackageEnvironment <- new.env(parent = parent.frame())
baseValuesEnvironment <- new.env(parent = globalPackageEnvironment)
experimentValuesEnvironment <- new.env(parent = globalPackageEnvironment)

# Aliases for environments
GPE <- globalPackageEnvironment
BVE <- baseValuesEnvironment
EXP <- experimentValuesEnvironment

GPE$globalConfigLoaded <- FALSE
GPE$globalDebug <- FALSE

GPE$traceState <- FALSE
GPE$inputExcelFile <- "./config/model_inputs.xlsx"

GPE$startYear <- 2020
GPE$endYear <- 2040
GPE$years <- seq(from = GPE$startYear,
                 to = GPE$endYear,
                 by = 1)
GPE$shoulderYears <- 1

GPE$ageMin <- 0
GPE$ageMax <- 100
GPE$ages <- seq(from = GPE$ageMin,
                to = GPE$ageMax,
                by = 1)

GPE$ratioFemalesAtBirth <- 0.5
GPE$ratioMalesAtBirth <- 1.0 - GPE$ratioFemalesAtBirth

GPE$initialPopulation <- NULL
GPE$scenarios <- NULL

GPE$rngSeed <- 12345

GPE$seasonalityCurves <- NULL
GPE$seasonalityOffsets <- NULL
GPE$populationLabels <- NULL

