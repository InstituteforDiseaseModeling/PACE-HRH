#' Run An EHEP Modeling Experiment
#'
#' Combine the parameter values in the Base and Epsilon environments, save the
#' new parameter values to \code{experimentValuesEnvironment}, then run a full
#' set of calculations.
#'
#' Results are written back into \code{experimentValuesEnvironment}.
#'
#' @param scenarioName (default = "ScenarioA")
#' @param debug (default = FALSE)
#'
#' @return List of dataframes of per-task times, or NULL
#'
#' @export
#'
RunExperiment <- function(scenarioName = "ScenarioA", debug = FALSE){
  # STEP 0 - INITIALIZE

  # Load scenario details
  scenario <- .getScenarioConfig(scenarioName)

  if (is.null(scenario)){
    TraceMessage(paste("Unknown scenario ", scenarioName, sep = ""))
    return(NULL)
  }

  # Create a results list
  results <- list()

  # Combine base and epsilon environments to produce experiment parameters
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

  initialPopulationDf <- data.frame(
    Age = popData$age,
    Female = popData$female@values,
    Male = popData$male@values,
    Total = popData$total@values
  )

  eve$demographics <-
    ComputeDemographicsProjection(
      initialPopulationDf,
      fertilityRatesDf,
      mortalityRatesDf,
      globalPackageEnvironment$years,
      normalize = scenario$BaselinePop,
      growthFlag = scenario$o_PopGrowth,
      debug = TRUE
    )

  # STEP 2 - COMPUTE TIMES FOR NORMAL TASKS (CLINICAL)
  taskIds <- which(gpe$taskData$computeMethod == "TimePerTask" &
                             gpe$taskData$Geography == scenario$PopType &
                     gpe$taskData$ClinicalOrNon == "Clinical")

  eve$clinicalTaskTimes <- TaskTimesGroup(taskIds, gpe$years)

  # STEP 2A - COMPUTE TIMES FOR NORMAL TASKS (NON-CLINICAL)
  taskIds <- which(gpe$taskData$computeMethod == "TimePerTask" &
                     gpe$taskData$Geography == scenario$PopType &
                     gpe$taskData$ClinicalOrNon != "Clinical")

  eve$nonClinicalTaskTimes <- TaskTimesGroup(taskIds, gpe$years)

  # STEP 3 - TOTAL THE TASK TIMES
  m <- as.matrix(eve$clinicalTaskTimes)
  aggAnnualClinicalTaskTimes <- apply(m, 1, function(x){return(sum(x[-1]))})

  m <- as.matrix(eve$nonClinicalTaskTimes)
  aggAnnualNonClinicalTaskTimes <- apply(m, 1, function(x){return(sum(x[-1]))})

  # STEP 4 - CORRECT FOR RATIO-BASED TIME ALLOCATION
  taskIds <- which(
    gpe$taskData$computeMethod == "TimeRatio" &
      gpe$taskData$Geography == scenario$PopType
  )

  eve$nonClinicalAllocationTimes <-
    AllocationTaskTimesGroup(taskIds, gpe$years, aggAnnualClinicalTaskTimes)

  m <- as.matrix(eve$nonClinicalAllocationTimes)
  aggAnnualNonClinicalAllocationTimes <- apply(m, 1, function(x){return(sum(x[-1]))})

  # STEP 5 - COMPUTE ADD-ON TIME (TRAVEL, ETC)
  taskIds <- which(gpe$taskData$computeMethod == "TimeAddedOn" &
                     gpe$taskData$Geography == scenario$PopType)

  if (length(taskIds) > 0){
    weeklyNonProductiveTime <- sum(gpe$taskData$HoursPerWeek[taskIds]) * 60
  } else {
    weeklyNonProductiveTime <- 0
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
  R_np <- weeklyNonProductiveTime * scenario$WeeksPerYr

  T_c <- aggAnnualClinicalTaskTimes
  T_nc <- aggAnnualNonClinicalTaskTimes + aggAnnualNonClinicalAllocationTimes

  N <- (T_c + T_nc) / (R_total - R_np)

  T_np <- R_np * N

  eve$nonProductiveTimes <-
    data.frame("Years" = gpe$years, "Overhead" = T_np)

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

  results$Clinical <- eve$clinicalTaskTimes
  results$NonClinical <- eve$nonClinicalTaskTimes
  results$NonClinicalAllocation <- eve$nonClinicalAllocationTimes
  results$NonProductive <- eve$nonProductiveTimes
  results$FTEs <- N

  return(results)
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
