#' Run An EHEP Modeling Experiment
#'
#' Combine the parameter values in the Base and Epsilon environments, save the
#' new parameter values to \code{experimentValuesEnvironment}, then run a full
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

  # Combine base and epsilon environments to produce experiment parameters
  ConfigureExperimentValues()

  # STEP 1 - BUILD POPULATION DEMOGRAPHICS
  pcp <- EXP$populationChangeParameters

  # Convert the initial population data into a dataframe suitable to pass
  # to the ComputeDemographicsProjection function.
  popData <- EXP$initialPopulation

  initialPopulationDf <- data.frame(
    Age = popData$age,
    Female = popData$female@values,
    Male = popData$male@values,
    Total = popData$total@values
  )

  EXP$demographics <-
    ComputeDemographicsProjection(
      initialPopulationDf,
      EXP$fertilityRatesMatrix,
      EXP$mortalityRatesMatrix,
      GPE$years,
      normalize = scenario$BaselinePop,
      growthFlag = scenario$o_PopGrowth,
      debug = TRUE
    )

  # STEP 2 - COMPUTE TIMES FOR NORMAL TASKS (CLINICAL)
  taskIds <- which(
    GPE$taskData$computeMethod == "TimePerTask" &
      GPE$taskData$Geography == scenario$PopType &
      GPE$taskData$ClinicalOrNon == "Clinical"
  )

  if (length(taskIds) > 0){
    EXP$clinicalTaskTimes <- TaskTimesGroup(taskIds, GPE$years)
  } else {
    EXP$clinicalTaskTimes <- NULL
  }

  aggAnnualClinicalTaskTimes <- .computeTotalTimes(EXP$clinicalTaskTimes)

  # STEP 3 - COMPUTE TIMES FOR NORMAL TASKS (NON-CLINICAL)
  taskIds <- which(
    GPE$taskData$computeMethod == "TimePerTask" &
      GPE$taskData$Geography == scenario$PopType &
      GPE$taskData$ClinicalOrNon != "Clinical"
  )

  if (length(taskIds) > 0){
    EXP$nonClinicalTaskTimes <- TaskTimesGroup(taskIds, GPE$years)
  } else {
    EXP$nonClinicalTaskTimes <- NULL
  }

  aggAnnualNonClinicalTaskTimes <- .computeTotalTimes(EXP$nonClinicalTaskTimes)

  # STEP 4 - COMPUTE TIMES FOR RATIO-BASED ALLOCATION TASKS
  taskIds <- which(
    GPE$taskData$computeMethod == "TimeRatio" &
      GPE$taskData$Geography == scenario$PopType
  )

  if (length(taskIds) > 0){
    EXP$nonClinicalAllocationTimes <-
      AllocationTaskTimesGroup(taskIds, GPE$years, aggAnnualClinicalTaskTimes)
  } else {
    EXP$nonClinicalAllocationTimes <- NULL
  }

  aggAnnualNonClinicalAllocationTimes <- .computeTotalTimes(EXP$nonClinicalAllocationTimes)

  # STEP 5 - COMPUTE ADD-ON TIME (TRAVEL, ETC)
  taskIds <- which(GPE$taskData$computeMethod == "TimeAddedOn" &
                     GPE$taskData$Geography == scenario$PopType)

  if (length(taskIds) > 0){
    nonProductiveTaskTimes <-
      TaskTimesGroup(taskIds, GPE$years, weeksPerYear = scenario$WeeksPerYr)
  } else {
    nonProductiveTaskTimes <- NULL
  }

  aggAnnualAddOnTimesPerHcw <- .computeTotalTimes(nonProductiveTaskTimes)

  # Note that at this point the calculation of add-on time is only
  # half-complete. Other task calculations return the total amount of time
  # required to perform a given task, based on the number of people needing the
  # task and the task duration. Add-on tasks are initially computed as extra
  # time required per-HCW (health-care worker). This reduces the availability of
  # the HCWs to do the other tasks. To calculate the actual total amount
  # of time spent doing add-on tasks, we need to compute the number of HCW's
  # needed to perform all the other tasks.
  #
  # Example: let's say HCW's work a 40 hour week, but spend 10 of those hours
  # doing add-on tasks. That means it will take 4 HCWs to do 120 hours of
  # clinical, etc tasks in a week (120/(40 - 10)). Once we know the number of
  # HCWs, we can finish the job and calculate that they will do 40 hours of
  # add-on task work.

  # STEP 6 - COMPUTE FTE EQUIVALENTS

  # T(c) + T(nc) + N * R(np) = N * R(total)
  #
  # where
  #   T(c) = total time (over whatever period) needed for clinical tasks,
  #   T(nc) = total time needed for non-clinical tasks,
  #   R(np) = time PER FTE needed for add-on/non-productive tasks,
  #   R(total) = available time PER FTE, and
  #   N = number of FTEs
  #
  # Rearranging, N = (T(c) + T(nc) / (R(total) - R(np))
  # And T(np) = N * R(np)

  # Compute available time per year per FTE (R_total)

  # R_total <- scenario$WeeksPerYr * scenario$HrsPerWeek * 60
  # assertthat::assert_that(R_total > 0)
  #
  # R_np <- aggAnnualAddOnTimesPerHcw
  #
  # T_c <- aggAnnualClinicalTaskTimes
  # T_nc <- aggAnnualNonClinicalTaskTimes + aggAnnualNonClinicalAllocationTimes
  #
  # N <- (T_c + T_nc) / (R_total - R_np)



  # New approach: nonProductive times are reported as per-person.
  N <- 1




  if (is.null(nonProductiveTaskTimes)){
    EXP$nonProductiveTimes <- NULL
  } else {
    nonProductiveTaskTimes$Time <- nonProductiveTaskTimes$Time * N
    EXP$nonProductiveTimes <- nonProductiveTaskTimes
  }

  results$Clinical <- EXP$clinicalTaskTimes
  results$NonClinical <- EXP$nonClinicalTaskTimes
  results$NonClinicalAllocation <- EXP$nonClinicalAllocationTimes
  results$NonProductive <- EXP$nonProductiveTimes
  results$FTEs <- data.frame("Years" = GPE$years, "FTEs" = N)

  if (scenario$o_Seasonality){
    seasonalityResults <- runSeasonalityExperiment(results)
    results$SeasonalityResults <- seasonalityResults
  }

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
