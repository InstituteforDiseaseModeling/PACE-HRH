#' Run An EHEP Modeling Experiment
#'
#' Combine the parameter values in the Base and Epsilon environments, save the
#' new parameter values to \code{experimentValuesEnvironment}, then run a full
#' set of calculations.
#'
#' Results are written back into \code{experimentValuesEnvironment}.
#'
#' @param scenarioName (default = "ScenarioA")
#' @param normalize Whether or not to normalize the initial population (default = NULL)
#'
#' @return NULL (invisible)
#'
#' @export
#'
RunExperiment <- function(scenarioName = "ScenarioA", normalize = NULL){
  # Environment locations
  eve <- experimentValuesEnvironment
  gpe <- globalPackageEnvironment

  scenario <- .getScenarioConfig(scenarioName)

  # Combine base and epsilon environments to give experiment parameters
  ConfigureExperimentValues()

  # STEP 1 - BUILD POPULATION DEMOGRAPHICS
  pcp <- eve$populationChangeParameters

  # Compute mortality and fertility rates to build demographics curves
  mortalityRatesDf <- generateMortalityRates(eve$populationChangeParameters)
  fertilityRatesDf <- generateFertilityRates(eve$populationChangeParameters)

  # Convert the initial population data into a dataframe suitable to pass
  # to the ComputeDemographicProjection function.
  popData <- eve$initialPopulation

  initialPopulationDf <- data.frame(
    Age = popData$age,
    Female = popData$female@values,
    Male = popData$male@values,
    Total = popData$total@values)

  if (.normalizationOn(normalize)){
    initialPopulationDf <-
      .normalizePopulation(initialPopulationDf, normalize)
  }

  eve$demographics <- ComputeDemographicsProjection(initialPopulationDf,
                                 fertilityRatesDf,
                                 mortalityRatesDf,
                                 globalPackageEnvironment$years,
                                 debug = TRUE)

  # STEP 2 - COMPUTE TIMES FOR CLINICAL TASKS
  clinicalTaskIds <- which(gpe$taskData$ClinicalOrNon == "Clinical" &
                             gpe$taskData$Geography == "National")

  eve$clinicalTaskTimes <- ClinicalTaskTimesGroup(clinicalTaskIds, gpe$years)

  # STEP 3 - TOTAL THE CLINICAL TASK TIMES
  m <- as.matrix(eve$clinicalTaskTimes)
  aggAnnualClinicalTimes <- apply(m, 1, function(x){return(sum(x[-1]))})


  print(aggAnnualClinicalTimes)


  # STEP 4 - CORRECT FOR NON-CLINICAL TASKS
  nonClinicalTaskIds <- which(gpe$taskData$ClinicalOrNon == "Development" &
                                gpe$taskData$Geography == "National")

  # Assumption: one big bucket of non-clinical tasks taking up a pre-determined
  # amount of an FTE's time (FTEratio)
  n <- length(nonClinicalTaskIds)
  assertthat::assert_that(n <= 1)
  aggAnnualNonClinicalTimes = 0
  if (n == 1){
    task <- gpe$taskData[nonClinicalTaskIds[1],]
    print(task)
    fteRatio <- task$FTEratio
    aggAnnualNonClinicalTimes <- aggAnnualClinicalTimes * (fteRatio / (1 - fteRatio))
  }




  invisible(NULL)
}

.normalizationOn <- function(normalize) {
  if (is.null(normalize)) {
    return(FALSE)
  }
  if (!is.numeric(normalize)) {
    return(FALSE)
  }
  if (normalize < 10000) {
    return(FALSE)
  }
  return(TRUE)
}

.normalizePopulation <- function(popDf, normalizedTotal){
  total <- sum(popDf$Female) + sum(popDf$Male)
  normFactor <- normalizedTotal / total

  popDf$Male <- round(popDf$Male * normFactor, 0)
  popDf$Female <- round(popDf$Female * normFactor, 0)
  popDf$Total <- popDf$Male + popDf$Female

  return(popDf)
}

.getScenarioConfig <- function(scenarioName){
  gpe <- globalPackageEnvironment
  n <- which(gpe$scenarios$UniqueID == scenarioName)

  if (length(n) == 0) {
    TraceMessage(paste("Could not find scenario ", scenarioName, sep = ""))
    return(NULL)
  }

  if (length(n) > 1) {
    TraceMessage(paste("More than one scenario ", scenarioName, ". Using first one.", sep = ""))
  }

  return(gpe$scenarios[n[1], ])
}
