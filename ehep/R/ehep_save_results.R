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
#' @param results TBD
#' @param filepath TBD
#' @param scenario TBD
#' @param run TBD
#'
#' @return NULL (invisible)
#'
#' @export
SaveSuiteResults <- function(results, filepath, scenario, run){
  trialIds <- names(results)

  l <- lapply(seq_along(trialIds), function(index){
    r <- results[[index]]
    if (sum(names(r) == "SeasonalityResults") > 0){
      return(func2(r, scenario, index, run))
    } else {
      return(func(r, scenario, index, run))
    }
  })

  out <- data.table::rbindlist(l)

  write.csv(out, file = filepath, row.names = FALSE)

  invisible(NULL)
}

# func2 ... SEASONALITY VERSION
func2 <- function(results, scenario, trial, run){
  s <- results$SeasonalityResults
  taskIds <- names(s)

  l <- lapply(1:252, function(t){
    year <- ((t - 1) %/% 12) + 2020
    month <- ((t - 1) %% 12) + 1

    times <- sapply(taskIds, function(taskId){
      time <- s[[taskId]]$Time[t]
    })

    Ns <- sapply(taskIds, function(taskId){
      N <- s[[taskId]]$N[t]
    })

    return(data.frame("Task_ID" = taskIds,
                      "Scenario_ID" = scenario,
                      "Trial_num" = trial,
                      "Run_num" = run,
                      "Year" = year,
                      "Month" = month,
                      "Num_services" = Ns,
                      "Service_time" = times,
                      "Health_benefit" = NA))
  })

  df <- data.table::rbindlist(l)
  return(df)
}


func <- function(results, scenario, trial, run){
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
