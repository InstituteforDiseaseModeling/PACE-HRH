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
  # STEP 0 - INITIALIZE

  # Load scenario details
  scenario <- BVE$scenario

  if (is.null(scenario)){
    traceMessage(paste("No scenario specified"))
    return(NULL)
  }

  # TODO: more extensive sanity checking that the Base Environment has been set
  # up SaveBaseSettings().

  # Create a results list
  results <- list()

  # STEP 1 - BUILD POPULATION DEMOGRAPHICS
  EXP$demographics <- ComputePopulationProjection(
    EXP$initialPopulation,
    EXP$populationChangeRates,
    BVE$years,
    normalize = scenario$BaselinePop,
    growthFlag = scenario$o_PopGrowth
  )

  # STEP 2 - COMPUTE TIMES FOR NORMAL TASKS (CLINICAL)
  taskIds <- which(
    BVE$taskData$computeMethod == "TimePerTask" &
      BVE$taskData$ClinicalOrNon == "Clinical"
  )

  if (length(taskIds) > 0){
    EXP$clinicalTaskTimes <- TaskTimesGroup(taskIds, BVE$years)
  } else {
    EXP$clinicalTaskTimes <- NULL
  }

  aggAnnualClinicalTaskTimes <- .computeTotalTimes(EXP$clinicalTaskTimes)

  # STEP 3 - COMPUTE TIMES FOR NORMAL TASKS (NON-CLINICAL)
  taskIds <- which(
    BVE$taskData$computeMethod == "TimePerTask" &
      BVE$taskData$ClinicalOrNon != "Clinical"
  )

  if (length(taskIds) > 0){
    EXP$nonClinicalTaskTimes <- TaskTimesGroup(taskIds, BVE$years)
  } else {
    EXP$nonClinicalTaskTimes <- NULL
  }

  aggAnnualNonClinicalTaskTimes <- .computeTotalTimes(EXP$nonClinicalTaskTimes)

  # STEP 4 - COMPUTE TIMES FOR RATIO-BASED ALLOCATION TASKS
  taskIds <- which(BVE$taskData$computeMethod == "TimeRatio")

  if (length(taskIds) > 0){
    EXP$nonClinicalAllocationTimes <-
      AllocationTaskTimesGroup(taskIds, BVE$years, aggAnnualClinicalTaskTimes)
  } else {
    EXP$nonClinicalAllocationTimes <- NULL
  }

  aggAnnualNonClinicalAllocationTimes <- .computeTotalTimes(EXP$nonClinicalAllocationTimes)

  # STEP 5 - COMPUTE ADD-ON TIME (TRAVEL, ETC)
  taskIds <- which(BVE$taskData$computeMethod == "TimeAddedOn")

  if (length(taskIds) > 0){
    nonProductiveTaskTimes <-
      TaskTimesGroup(taskIds, BVE$years, weeksPerYear = scenario$WeeksPerYr)
  } else {
    nonProductiveTaskTimes <- NULL
  }

  aggAnnualAddOnTimesPerHcw <- .computeTotalTimes(nonProductiveTaskTimes)


  # TODO: nonProductive times are reported PER-PERSON, which is different
  # to all the other times. This is going to change as a consequence
  # of addressing Issue #51.

  N <- 1

  if (is.null(nonProductiveTaskTimes)){
    EXP$nonProductiveTimes <- NULL
  } else {
    nonProductiveTaskTimes$Time <- nonProductiveTaskTimes$Time * N
    EXP$nonProductiveTimes <- nonProductiveTaskTimes
  }

  results$AnnualTimes <-.computeAnnualTimesMatrix()
  results$AnnualCounts <-.computeAnnualCountsMatrix()

  seasonalityResults <- runSeasonalityExperiment(results)
  results$SeasonalityResults <- seasonalityResults

  return(results)
}

.taskTypeVarNames <- c(
  "clinicalTaskTimes",
  "nonClinicalTaskTimes",
  "nonClinicalAllocationTimes",
  "nonProductiveTimes")

.computeAnnualTimesMatrix <- function() {
  l <-
    lapply(.taskTypeVarNames, function(type) {
      data <- get(type, pos = EXP)
      if (is.null(data)) {
        return(NULL)
      } else {
        return(t(data$Time))
      }
    })

  return(do.call(rbind, l))
}

.computeAnnualCountsMatrix <- function() {
  l <-
    lapply(.taskTypeVarNames, function(type) {
      data <- get(type, pos = EXP)
      if (is.null(data)) {
        return(NULL)
      } else {
        return(t(data$N))
      }
    })

  return(do.call(rbind, l))
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
