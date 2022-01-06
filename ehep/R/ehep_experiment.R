#' Run An EHEP Modeling Experiment
#'
#' Combine the parameter values in the Base and Epsilon environments, save the
#' new parameter values to \code{experimentValuesEnvironment}, then run a full
#' set of calculations.
#'
#' Results are written back into \code{experimentValuesEnvironment}.
#'
#' @return NULL (invisible)
#'
#' @export
#'
RunExperiment <- function(){
  # Combine base and epsilon environments to give experiment parameters
  ConfigureExperimentValues()

  # Environment locations
  eve <- experimentValuesEnvironment
  gpe <- globalPackageEnvironment

  # STEP 1 - BUILD POPULATION DEMOGRAPHICS
  pcp <- eve$populationChangeParameters

  # Compute mortality and fertility rates to build demographics curves
  mortalityRatesDf <- generateMortalityRates(eve$populationChangeParameters)
  fertilityRatesDf <- generateFertilityRates(eve$populationChangeParameters)

  # Convert the initial population data into a dataframe suitable to pass
  # to the ComputeDemographicProjection function.
  popData <- eve$initialPopulation
  initialPopulationDf <- data.frame(Age = popData$age,
                                    Female = popData$female@values,
                                    Male = popData$male@values,
                                    Total = popData$total@values)

  eve$demographics <- ComputeDemographicsProjection(initialPopulationDf,
                                 fertilityRatesDf,
                                 mortalityRatesDf,
                                 globalPackageEnvironment$years,
                                 debug = TRUE)


  # STEP 2 - COMPUTE CLINICAL TASK TIME TOTALS
  clinicalTaskIds <- which(gpe$taskData$ClinicalOrNon == "Clinical" &
                             gpe$taskData$Geography == "National")

  eve$clinicalTaskTimes <- ClinicalTaskTimesGroup(clinicalTaskIds, gpe$years)


  invisible(NULL)
}
