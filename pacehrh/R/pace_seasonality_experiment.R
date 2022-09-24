#' Run The Seasonality Component Of A PACE-HRH Modeling Experiment
#'
#' @param results Results from the annual part of the run
#' @param debug (default = FALSE)
#'
#' @return List of dataframes of per-task times, or NULL
#'
runSeasonalityExperiment <- function(results, debug = FALSE){
  scenario <- BVE$scenario

  if (is.null(scenario)){
    traceMessage(paste("No scenario data", scenarioName, sep = ""))
    return(NULL)
  }

  # The seasonality "curve" used when we don't know the seasonality
  dummyCurve <- c(1,1,1,1,1,1,1,1,1,1,1,1) / 12

  # Look for seasonality-affected tasks in the task list for this scenario

  taskNames <- BVE$taskData$Indicator
  seasonalityTaskNames <- GPE$seasonalityOffsets$Task
  seasonalityTaskCurves <- GPE$seasonalityOffsets$Curve

  # We compute extra "shoulder" years to handle inaccuracies at the far end of
  # the seasonality time service resulting from negative seasonality offsets. We
  # truncate off the extra years when we're done.
  computedMonthCount <- 12 * length(BVE$years)
  returnedMonthCount <- 12 * length(GPE$years)

  l <- lapply(taskNames, function(taskName){
    if (length(seasonalityTaskIndex <- which(seasonalityTaskNames == taskName)) == 1){
      # Grab already computed annual data for this task
      annualTimes <- results$AnnualTimes[taskName,]
      annualServices <- results$AnnualCounts[taskName,]

      # Find the appropriate seasonality curve
      curve <-
        .getSeasonalityCurve(seasonalityTaskCurves[seasonalityTaskIndex])

      # Renormalize the seasonality curve if necessary
      if (abs(sum(curve) - 1.0) > 1e-6){
        curve <- curve / sum(curve)
      }

      # Apply the seasonality curve to the annual service counts and service
      # times vectors to get much longer vectors of monthly counts and services.
      monthlyTimes <- .convertAnnualToMonthly(annualTimes, curve)
      monthlyServices <- .convertAnnualToMonthly(annualServices, curve)

      # Build a scatter matrix
      nRows <- nCols <- length(monthlyServices)

      A <- matrix(0, nrow = nRows, ncol = nCols)

      offsets <-
        GPE$seasonalityOffsets[seasonalityTaskIndex,
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

      # Truncate to desired output length
      monthlyTimes <- monthlyTimes[1:returnedMonthCount]
      monthlyServices <- monthlyServices[1:returnedMonthCount]

      return(list(Time = monthlyTimes, N = monthlyServices))
    } else {
      annualTimes <- results$AnnualTimes[taskName,]
      annualServices <- results$AnnualCounts[taskName,]

      names(annualTimes) <- NULL
      monthlyTimes <- .convertAnnualToMonthly(annualTimes, dummyCurve)

      if (is.null(annualServices)){
        monthlyServices <- replicate(length(monthlyTimes), 0)
      } else {
        names(annualServices) <- NULL
        monthlyServices <- .convertAnnualToMonthly(annualServices, dummyCurve)
      }
      names(monthlyServices) <- NULL

      # Truncate to desired output length
      monthlyTimes <- monthlyTimes[1:returnedMonthCount]
      monthlyServices <- monthlyServices[1:returnedMonthCount]

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

.getSeasonalityCurve <- function(curveType) {
  curve = NULL

  curve <- GPE$seasonalityCurves[[curveType]]

  if (is.null(curve)) {
    traceMessage(paste0("Unknown seasonality curve: ", curveType))
    return(NULL)
  } else {
    return(curve)
  }
}
