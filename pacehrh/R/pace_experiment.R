#' Run A PACE-HRH Modeling Experiment
#'
#' Apply stochasticity to the values in the BVE (\code{baseValuesEnvironment}), save the
#' new parameter values to the EXP \code{experimentValuesEnvironment}, then run a full
#' set of calculations.
#'
#' Results are written back into \code{experimentValuesEnvironment} and as
#' a results structure.
#'
#' @param debug (default = FALSE)
#'
#' @return List of dataframes of per-task times, or NULL
#'
RunExperiment <- function(debug = FALSE){
  # INITIALIZE
  scenario <- BVE$scenario

  if (is.null(scenario)){
    traceMessage(paste("No scenario specified"))
    return(NULL)
  }

  # TODO: more extensive sanity checking that the Base Environment has been set
  # up correctly by SaveBaseSettings().

  results <- list()

  # BUILD POPULATION PREDICTIONS
  EXP$demographics <- ComputePopulationProjection(
    EXP$initialPopulation,
    EXP$populationChangeRates,
    BVE$years,
    normalize = scenario$BaselinePop,
    growthFlag = scenario$o_PopGrowth
  )

  # BUILD MATRICES OF LABELLED POPULATION RANGES
  EXP$populationRangeMatrices <-
    .computePopulationRangeMatrices(EXP$demographics, BVE$populationRangesTable)

  # COMPUTE ANNUAL TIMES FOR TASKS
  t <- TaskTimes()

  results$AnnualTimes <- t$Time
  results$AnnualCounts <- t$N

  # USE ANNUAL TIMES TO COMPUTE SEASONALLY ADJUSTED MONTHLY TIMES
  seasonalityResults <- runSeasonalityExperiment(results)
  results$SeasonalityResults <- seasonalityResults

  return(results)
}

.computeTotalTimes <- function(resultsObj){
  if (is.null(resultsObj)){
    return(0)
  }

  retVal <- tryCatch({
    apply(resultsObj[["Time"]], 1, sum)
  },
  error = function(e){
    return(0)
  })

  return(retVal)
}


# ---------------------------------------
#
# TODO: Remove this testing-only function
#
# ---------------------------------------


#' Compute Population Range Sizes Based On Population Predictions
#'
#' @param populations Population predictions as returned by [ComputePopulationProjection()]
#' @param popRanges Population range definitions as configured by [InitializePopulation()]
#'
#' @return List of population range sizes
#'
#' @examples
#' \dontrun{
#' EXP$populationRangeMatrices <-
#'   .computePopulationRangeMatrices(EXP$demographics, BVE$populationRangesTable)
#' }
.computePopulationRangeMatrices <- function(populations, popRanges){
  l <- lapply(populations, function(pop){pop$Female})
  popMatrix <- do.call(cbind, l)
  rangeMatrix <- popRanges$Female

  mf <- rangeMatrix %*% popMatrix

  l <- lapply(populations, function(pop){pop$Male})
  popMatrix <- do.call(cbind, l)
  rangeMatrix <- popRanges$Male

  mm <- rangeMatrix %*% popMatrix

  rf <- lapply(populations, function(pop){
    t(t(popRanges$Female) * pop$Female)
  })

  rm <- lapply(populations, function(pop){
    t(t(popRanges$Male) * pop$Male)
  })

  return(list(
    Female = mf,
    Male = mm,
    Total = mf + mm,
    FemaleRanges = rf,
    MaleRanges = rm
  ))
}
