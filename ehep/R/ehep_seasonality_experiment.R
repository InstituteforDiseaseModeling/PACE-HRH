#' Run The Seasonality Component Of An EHEP Modeling Experiment
#'
#' @param results Results from the annual part of the run
#' @param debug (default = FALSE)
#'
#' @return List of dataframes of per-task times, or NULL
#'
runSeasonalityExperiment <- function(results, debug = FALSE){
  scenario <- baseValuesEnvironment$scenario

  if (is.null(scenario)){
    TraceMessage(paste("No scenario data", scenarioName, sep = ""))
    return(NULL)
  }

  # Boots and braces stuff: this function should never be called on a scenario
  # that isn't set up for seasonality.
  assertthat::assert_that(scenario$o_Seasonality == TRUE)

  gpe <- globalPackageEnvironment

  # The seasonality "curve" used when we don't know the seasonality
  dummyCurve <- c(1,1,1,1,1,1,1,1,1,1,1,1) / 12

  # Look for seasonality-affected tasks in the task list for this scenario
  taskIds <- which(gpe$taskData$Geography == scenario$PopType)
  taskNames <- gpe$taskData$Indicator[taskIds]
  seasonalityTaskNames <- gpe$seasonalityOffsets$Task
  seasonalityTaskCurves <- gpe$seasonalityOffsets$Curve

  l <- lapply(seq_along(taskIds), function(i){
    taskName <- taskNames[i]

    if (length(seasonalityTaskIndex <- which(seasonalityTaskNames == taskName)) == 1){
      # Grab already computed annual data for this task
      annualTimes <- results$Clinical$Time[, taskName]
      annualServices <- results$Clinical$N[, taskName]

      # Find the appropriate seasonality curve
      curve <-
        .getSeasonalityCurve(seasonalityTaskCurves[seasonalityTaskIndex],
                             scenario$PopType)

      # Apply the seasonality curve to the annual service counts and service
      # times vectors to get much longer vectors of monthly counts and services.
      monthlyTimes <- .convertAnnualToMonthly(annualTimes, curve)
      monthlyServices <- .convertAnnualToMonthly(annualServices, curve)

      # Build a scatter matrix
      nRows <- nCols <- length(monthlyServices)
      assertthat::are_equal(nRows, 12 * length(gpe$years))

      A <- matrix(0, nrow = nRows, ncol = nCols)

      offsets <-
        gpe$seasonalityOffsets[seasonalityTaskIndex,
                               c("Offset1", "Offset2", "Offset3",
                                 "Offset4", "Offset5", "Offset6")]
      offsets <- offsets[!is.na(offsets)]
      nOffsets <- length(offsets)

      for (r in 1:nRows){
        for (offset in offsets){
          c <- r - offset
          if ((c >= 1) & (c <= nCols)){
            A[r,c] <- 1
          }
        }
      }

      monthlyTimes <- as.vector(A %*% round((monthlyTimes / nOffsets), 0))
      monthlyServices <- as.vector(A %*% round((monthlyServices / nOffsets), 0))

      return(list(Time = monthlyTimes, N = monthlyServices))
    } else {
      annualServices <- NULL
      annualTimes <- NULL

      # Find the previous calculated values

      # HACK ALERT: This ugly code assumes all four types of tasks are present
      tnames <- dimnames(results$Clinical$Time)[[2]]
      if (length(taskIndex <- which(tnames == taskName)) == 1){
        annualTimes <- results$Clinical$Time[, taskName]
        annualServices <- results$Clinical$N[, taskName]
      }

      tnames <- dimnames(results$NonClinical$Time)[[2]]
      if (length(taskIndex <- which(tnames == taskName)) == 1){
        annualTimes <- results$NonClinical$Time[, taskName]
        annualServices <- results$NonClinical$N[, taskName]
      }

      tnames <- names(results$NonClinicalAllocation)
      if (length(taskIndex <- which(tnames == taskName)) == 1){
        annualTimes <- results$NonClinicalAllocation[[taskName]]
        annualServices <- NULL
      }

      tnames <- names(results$NonProductive)
      if (length(taskIndex <- which(tnames == taskName)) == 1){
        annualTimes <- results$NonProductive[[taskName]]
        annualServices <- NULL
      }

      names(annualTimes) <- NULL
      monthlyTimes <- .convertAnnualToMonthly(annualTimes, dummyCurve)

      if (is.null(annualServices)){
        monthlyServices <- replicate(length(monthlyTimes), 0)
      } else {
        monthlyServices <- .convertAnnualToMonthly(annualServices, dummyCurve)
        names(monthlyServices) <- NULL
      }

      return(list(Time = monthlyTimes, N = monthlyServices))
    }
  })

  names(l) <- taskNames

  return(l)
}

.convertAnnualToMonthly <- function(values, curve){
  monthly <- unlist(lapply(values, function(v){
    return(curve * v)
  }))
  return(round(monthly, 0))
}

.getSeasonalityCurve <- function(curveType, popType) {
  curve = NULL

  if (curveType == "Births") {
    if (popType == "National") {
      curve <- gpe$seasonalityCurves$`Births National`
    } else if (popType == "Rural") {
      curve <- gpe$seasonalityCurves$`Births Rural`
    } else if (popType == "Urban") {
      curve <- gpe$seasonalityCurves$`Births Urban`
    }
  } else if (curveType == "Malaria") {
    if (popType == "Urban") {
      curve <- gpe$seasonalityCurves$`Malaria Urban`
    } else if (popType == "Rural") {
      curve <- gpe$seasonalityCurves$`Malaria Rural`
    } else if (popType == "National") {
      # HACK ALERT! There's no Malaria National curve, but Urban and Rural
      # curves are identical (as of 1/17/2022) so this will do.
      curve <- gpe$seasonalityCurves$`Malaria Rural`
    }
  } else if (curveType == "Malnutrition") {
    curve <- gpe$seasonalityCurves$`Malnutrition`
  } else if (curveType == "TB") {
    curve <- gpe$seasonalityCurves$`TB`
  }

  if (is.null(curve)) {
    TraceMessage(paste("Unknown seasonality curve: ", curveType, "/", popType,
                 sep = ""))
    return(NULL)
  } else {
    return(curve)
  }
}
