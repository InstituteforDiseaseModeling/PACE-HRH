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
    TraceMessage(paste("No scenario specified"))
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

  EXP$clinicalTaskTimes <- TaskTimesGroup(taskIds, GPE$years)

  # STEP 2A - COMPUTE TIMES FOR NORMAL TASKS (NON-CLINICAL)
  taskIds <- which(
    GPE$taskData$computeMethod == "TimePerTask" &
      GPE$taskData$Geography == scenario$PopType &
      GPE$taskData$ClinicalOrNon != "Clinical"
  )

  EXP$nonClinicalTaskTimes <- TaskTimesGroup(taskIds, GPE$years)

  # STEP 3 - TOTAL THE TASK TIMES
  aggAnnualClinicalTaskTimes <- apply(EXP$clinicalTaskTimes$Time, 1, sum)
  aggAnnualNonClinicalTaskTimes <- apply(EXP$nonClinicalTaskTimes$Time, 1, sum)

  # STEP 4 - CORRECT FOR RATIO-BASED TIME ALLOCATION
  taskIds <- which(
    GPE$taskData$computeMethod == "TimeRatio" &
      GPE$taskData$Geography == scenario$PopType
  )

  EXP$nonClinicalAllocationTimes <-
    AllocationTaskTimesGroup(taskIds, GPE$years, aggAnnualClinicalTaskTimes)
  aggAnnualNonClinicalAllocationTimes <-
    apply(EXP$nonClinicalAllocationTimes$Time, 1, sum)

  # STEP 5 - COMPUTE ADD-ON TIME (TRAVEL, ETC)
  taskIds <- which(GPE$taskData$computeMethod == "TimeAddedOn" &
                     GPE$taskData$Geography == scenario$PopType)

  if (length(taskIds) > 0){
    nonProductiveTaskTimes <-
      TaskTimesGroup(taskIds, GPE$years, weeksPerYear = scenario$WeeksPerYr)
  } else {
    nonProductiveTaskTimes <- NULL
  }

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
  annualWorkHours <- scenario$WeeksPerYr * scenario$HrsPerWeek

  R_total <- annualWorkHours * 60

  if (!is.null(nonProductiveTaskTimes)){
    R_np <- apply(nonProductiveTaskTimes$Time, 1, sum)
  } else {
    R_np <- 0.0
  }

  T_c <- aggAnnualClinicalTaskTimes
  T_nc <- aggAnnualNonClinicalTaskTimes + aggAnnualNonClinicalAllocationTimes

  N <- (T_c + T_nc) / (R_total - R_np)

  nonProductiveTaskTimes$Time <- nonProductiveTaskTimes$Time * N
  EXP$nonProductiveTimes <- nonProductiveTaskTimes

  if (debug){
    print("T(c)")
    print(T_c)
    print("Non-clinical allocation (MHH)")
    print(aggAnnualNonClinicalAllocationTimes)
    print("Travel + Record-Keeping")
    print(aggAnnualNonClinicalTaskTimes)
    print("T(nc)")
    print(T_nc)
    print("R(total)")
    print(R_total)
    print("R(np) (Admin)")
    print(R_np)
    print("T(np) (Admin)")
    print(R_np * N)
    print("FTEs")
    print(N)
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
