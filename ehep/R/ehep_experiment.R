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
  ConfigureExperimentValues()

  eve <- experimentValuesEnvironment
  pcp <- eve$populationChangeParameters

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

  invisible(NULL)
}
