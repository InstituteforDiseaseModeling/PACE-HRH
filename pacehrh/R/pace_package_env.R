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

# INTERNAL CONSTANTS

.roundingLaws <- c(
  "early",
  "late",
  "none"
)

.defaultRoundingLaw <- .roundingLaws[1] # "early"

.perAgeLevels <- c(
  "off",
  "annual",
  "monthly"
)

.defaultPerAgeLevel <- .perAgeLevels[1] # "off"

.colorM <- rgb(96,131,180, maxColorValue = 255)
.colorF <- rgb(210,120,135, maxColorValue = 255)

.defaultPopSheet <- "TotalPop"
.defaultPopLabelSheet <- "Lookup"
.defaultPopulationRatesSheet <- "PopValues"
.defaultSeasonalityCurvesSheet <- "SeasonalityCurves"
.defaultSeasonalityOffsetsSheet <- "SeasonalityOffsets"
.defaultTaskValuesSheet <- "TaskValues"
.defaultScenariosSheet <- "Scenarios"
.defaultStochasticParametersSheet <- "StochasticParameters"
.defaultCadreRolesSheet <- "CadreRoles"

# GLOBAL VARIABLES

GPE$globalConfigLoaded <- FALSE
GPE$globalDebug <- FALSE

GPE$traceState <- FALSE
GPE$inputExcelFile <- "./config/model_inputs.xlsx"
GPE$ignoreGlobalConfigExcelFileSetting <- FALSE

GPE$startYear <- 2020
GPE$endYear <- 2040
GPE$years <- seq(from = GPE$startYear,
                 to = GPE$endYear,
                 by = 1)
GPE$shoulderYears <- 1

GPE$stochasticity <- TRUE

GPE$ageMin <- 0
GPE$ageMax <- 100
GPE$ages <- seq(from = GPE$ageMin,
                to = GPE$ageMax,
                by = 1)

GPE$ratioFemalesAtBirth <- 0.5
GPE$ratioMalesAtBirth <- 1.0 - GPE$ratioFemalesAtBirth

GPE$scenarios <- NULL

GPE$rngSeed <- 12345
GPE$roundingLaw <- .defaultRoundingLaw
GPE$perAgeStats <- .defaultPerAgeLevel

BVE$seasonalityCurves <- NULL
BVE$seasonalityOffsets <- NULL
BVE$populationLabels <- NULL
BVE$initialPopulation <- NULL
BVE$stochasticParams <- NULL
BVE$populationRangesTable <- NULL
