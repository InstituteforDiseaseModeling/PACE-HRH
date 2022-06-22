#' Save Demographics Information From A Suite Of Experiments
#'
#' @param results Results structure
#' @param filename CSV file to write to
#' @param breaks Vector of population bucket boundaries
#'
#' @return Generated output as a data frame (invisible)
#' @export
SaveSuiteDemographics <- function(results, filename = "out.csv", breaks = NULL) {
  trials <- names(results)

  l <- lapply(trials, function(trial){
    trialResults <- results[[trial]]
    popData <- trialResults$Population

    .formatTrialDemographics(popData, trial, breaks)
  })

  outdf <- data.table::rbindlist(l)
  write.csv(outdf, file = filename, row.names = FALSE)
  return(invisible(outdf))
}


#' @importFrom magrittr %>%
.formatTrialDemographics <- function(popData, trial, breaks) {
  if (is.null(popData)) {
    return(invisible(NULL))
  }

  ages <- GPE$ages

  # Bucketize age ranges according to the "breaks" variable
  ageBucket <- 1:length(ages)

  if (!is.null(breaks)){
    # Both "ages" and "breaks" need to be in ascending numerical order
    assertthat::assert_that(identical(breaks, breaks[order(breaks)]))
    assertthat::assert_that(identical(ages, ages[order(ages)]))

    # Add a sentinel value to the breaks list
    breaks <- c(breaks, max(c(max(ages), max(breaks))) + 1)

    nBucket <- 1L
    compValue <- breaks[nBucket]
    for (i in seq_along(ages)){
      if (ages[i] > compValue){
        nBucket <- nBucket + 1L
        compValue <- breaks[nBucket]
      }

      ageBucket[i] <- nBucket
    }
  }

  outdf <- NULL
  yearList <- names(popData)

  l <- lapply(yearList, function(year) {
    pop <- popData[[year]]

    trials <- replicate(nrow(pop), trial)
    years <- replicate(nrow(pop), year)

    # Emit population data
    df <- data.frame(
      Trial = trials,
      Year = years,
      Age = pop$Range,
      Female = pop$Female,
      Male = pop$Male
    )

    if (!is.null(breaks)){
      df$AgeBucket <- ageBucket

      df <-
        df %>%
        dplyr::group_by(Trial, Year, AgeBucket) %>%
        dplyr::summarize(Female = sum(Female), Male = sum(Male))
    }

    return(df)
  })

  return(data.table::rbindlist(l))
}




#' Save Experiment Results As CSV File
#'
#' @param results TBD
#' @param filepath TBD
#' @param scenario TBD
#' @param trial TBD
#' @param run TBD
#'
#' @return NULL (invisible)
#'
#' @export
SaveResults <- function(results, filepath, scenario, trial, run){
  dfCsv <- data.frame()

  rows <- seq(1, dim(results$Clinical$Time)[1])
  years <- dimnames(results$Clinical$Time)[[1]]

  l <- lapply(rows, function(i) {
    year <- years[i]

    timeRowData <- results$Clinical$Time[i, ]
    countRowData <- results$Clinical$N[i, ]
    df1 <- .emitRowList(timeRowData, countRowData, scenario, trial, run, year)

    timeRowData <- results$NonClinical$Time[i, ]
    countRowData <- results$NonClinical$N[i, ]
    df2 <- .emitRowList(timeRowData, countRowData, scenario, trial, run, year)

    rowData <- results$NonClinicalAllocation[i, ]
    rowData <- rowData[-1]
    df3 <- .emitRowList(rowData, NULL, scenario, trial, run, year)

    rowData <- results$NonProductive[i, ]
    rowData <- rowData[-1]
    df4 <- .emitRowList(rowData, NULL, scenario, trial, run, year)

    if (nrow(dfCsv) == 0){
      dfCsv <<- data.table::rbindlist(list(df1, df2, df3, df4))
    } else {
      dfCsv <<- data.table::rbindlist(list(dfCsv, df1, df2, df3, df4))
    }

    return(1)
  })

  write.csv(dfCsv, file = filepath, row.names = FALSE)

  invisible(NULL)
}

.emitRowList <- function(timeRowData, countRowData, scenario, trial, run, year){
  taskNames <- names(timeRowData)

  l <- lapply(seq_along(timeRowData), function(i) {
    if (is.null(countRowData)){
      count <- 0
    } else {
      count <- countRowData[[i]]
    }

    dfOut <- data.frame(
      "Task_ID" = taskNames[[i]],
      "Scenario_ID" = scenario,
      "Trial_num" = trial,
      "Run_num" = run,
      "Year" = year,
      "Month" = NA,
      "Num_services" = count,
      "Service_time" = timeRowData[[i]],
      "Health_benefit" = NA
    )
  })

  return(data.table::rbindlist(l))
}

#' Save Experiment Results As CSV File
#'
#' @param results Results structure as returned by \code{RunExperiments()}
#' @param filepath Location to write CSV file
#' @param scenario Name of experiments scenario, as passed to \code{RunExperiments()}
#' @param run Run ID
#'
#' @return NULL (invisible)
#'
#' @export
SaveSuiteResults <- function(results, filepath, scenario, run){
  trialIds <- names(results)

  l <- lapply(seq_along(trialIds), function(index){
    r <- results[[index]]
    if ("SeasonalityResults" %in% names(r)){
      return(.saveSuiteResults2(r, scenario, index, run))
    } else {
      return(.saveSuiteResults1(r, scenario, index, run))
    }
  })

  out <- data.table::rbindlist(l)

#  write.csv(out, file = filepath, row.names = FALSE)

  data.table::fwrite(out, file = filepath, row.names = FALSE, na = "NA")

  invisible(NULL)
}

# func2 ... SEASONALITY VERSION
.saveSuiteResults2 <- function(results, scenario, trial, run){
  tasks <- names(results$SeasonalityResults)
  ntasks <- length(tasks)

  l <- lapply(results$SeasonalityResults, function(r){
    t(as.matrix(r$Time))
  })

  mT <- do.call(rbind, l)

  l <- lapply(results$SeasonalityResults, function(r){
    t(as.matrix(r$N))
  })

  mN <- do.call(rbind, l)

  rownames(mT) <- tasks
  rownames(mN) <- tasks

  msize <- dim(mT)[1] * dim(mT)[2]

  len <- dim(mT)[2]
  startYear <- GPE$startYear
  startMonth <- 1
  year <- ((1:len - 1) %/% 12) + startYear
  month <- ((1:len - 1) %% 12) + 1

  yearCol <- integer(length = msize)
  monthCol <- integer(length = msize)
  trialCol <- integer(length = msize)
  runCol <- integer(length = msize)
  taskIdCol <- character(length = msize)
  timesCol <- integer(length = msize)
  servicesCol <- integer(length = msize)
  scenarioCol <- character(length = msize)

  runCol[1:msize] <- run
  scenarioCol[1:msize] <- scenario
  trialCol[1:msize] <- trial

  pos <- 1

  for (i in 1:len){
    endpos <- pos + ntasks - 1
    yearCol[pos:endpos] <- year[i]
    monthCol[pos:endpos] <- month[i]
    taskIdCol[pos:endpos] <- tasks
    timesCol[pos:endpos] <- mT[,i]
    servicesCol[pos:endpos] <- mN[,i]

    pos <- pos + ntasks
  }

  df <-
    (data.frame("Task_ID" = taskIdCol,
                "Scenario_ID" = scenarioCol,
                "Trial_num" = trialCol,
                "Run_num" = runCol,
                "Year" = yearCol,
                "Month" = monthCol,
                "Num_services" = servicesCol,
                "Service_time" = timesCol,
                "Health_benefit" = NA))

  # s <- results$SeasonalityResults
  # taskIds <- names(s)
  # n = length(taskIds)
  #
  # NAs <- replicate(n, NA)
  #
  # dummyDf <- data.frame(
  #   "Task_ID" = taskIds,
  #   "Scenario_ID" = scenario,
  #   "Trial_num" = trial,
  #   "Run_num" = run
  # )
  #
  # l <- lapply(1:252, function(t){
  #   year <- ((t - 1) %/% 12) + GPE$startYear
  #   month <- ((t - 1) %% 12) + 1
  #
  #   times <- sapply(taskIds, function(taskId){
  #     time <- s[[taskId]]$Time[t]
  #   })
  #
  #   Ns <- sapply(taskIds, function(taskId){
  #     N <- s[[taskId]]$N[t]
  #   })
  #
  #   retDf <- dummyDf
  #   retDf$Year <- replicate(n, year)
  #   retDf$Month <- replicate(n, month)
  #   retDf$Num_services = Ns
  #   retDf$Service_time = times
  #   retDf$Health_benefit = NAs
  #
  #   return(retDf)
  #
  #   # return(data.frame("Task_ID" = taskIds,
  #   #                   "Scenario_ID" = scenario,
  #   #                   "Trial_num" = trial,
  #   #                   "Run_num" = run,
  #   #                   "Year" = year,
  #   #                   "Month" = month,
  #   #                   "Num_services" = Ns,
  #   #                   "Service_time" = times,
  #   #                   "Health_benefit" = NA))
  # })
  #
  # df <- data.table::rbindlist(l)
  return(df)
}


.saveSuiteResults1 <- function(results, scenario, trial, run){
  dfCsv <- data.frame()

  rows <- seq(1, dim(results$Clinical$Time)[1])
  years <- dimnames(results$Clinical$Time)[[1]]

  l <- lapply(rows, function(i) {
    year <- years[i]

    timeRowData <- results$Clinical$Time[i, ]
    countRowData <- results$Clinical$N[i, ]
    df1 <- .emitRowList(timeRowData, countRowData, scenario, trial, run, year)

    timeRowData <- results$NonClinical$Time[i, ]
    countRowData <- results$NonClinical$N[i, ]
    df2 <- .emitRowList(timeRowData, countRowData, scenario, trial, run, year)

    rowData <- results$NonClinicalAllocation[i, ]
    rowData <- rowData[-1]
    df3 <- .emitRowList(rowData, NULL, scenario, trial, run, year)

    rowData <- results$NonProductive[i, ]
    rowData <- rowData[-1]
    df4 <- .emitRowList(rowData, NULL, scenario, trial, run, year)

    if (nrow(dfCsv) == 0){
      dfCsv <<- data.table::rbindlist(list(df1, df2, df3, df4))
    } else {
      dfCsv <<- data.table::rbindlist(list(dfCsv, df1, df2, df3, df4))
    }

    return(1)
  })

  return(dfCsv)
}
