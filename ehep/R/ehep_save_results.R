#' Save Demographics Information From A Suite Of Experiments
#'
#' @param results Results structure (as returned by [RunExperiments()])
#' @param filepath CSV file to write to (default = "out.csv")
#' @param breaks Vector of population bucket boundaries
#'
#' @return Generated output as a data frame. AgeBucket values can be used to
#' summarize the output, e.g. with dplyr.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @md
#' @examples
#' \dontrun{
#' scenario <- "ScenarioName"
#'
#'results <-
#'  ehep::RunExperiments(scenarioName = scenario,
#'                       trials = 100)
#'
#' df <- ehep::SaveSuiteDemographics(results, breaks = c(50))
#'
#' df <- df %>%
#'   dplyr::group_by(Trial, Year, AgeBucket) %>%
#'   dplyr::summarize(Female = sum(Female), Male = sum(Male))
#' }
SaveSuiteDemographics <- function(results, filepath = "out.csv", breaks = NULL) {
  trials <- names(results)

  l <- lapply(trials, function(trial){
    trialResults <- results[[trial]]
    popData <- trialResults$Population
    .formatTrialDemographics(popData, trial, breaks)
  })

  # Combine the list of matrices returned in l, and convert to a data frame
  outdf <- as.data.frame(do.call(rbind, l))

  # Prune and rename the column set
  colNames <- c("Trial", "Year", "AgeBucket", "Age", "Female", "Male")
  outdf <- outdf[1:length(colNames)]
  names(outdf) <- colNames

  # Write to target file and return
  data.table::fwrite(outdf, file = filename, row.names = FALSE, na = "NA")
  return(outdf)
}

.formatTrialDemographics <- function(popData, trial, breaks) {
  if (is.null(popData)) {
    return(invisible(NULL))
  }

  ages <- GPE$ages
  vsize <- length(ages)

  # Bucketize age ranges according to the "breaks" variable. Each value is the
  # end of an age bucket, so breaks = c(50) divides the age range into two
  # buckets: 0-50, 51-100
  ageBucket <- 1:vsize

  if (is.null(breaks)){
    breaks <- max(ages)
  }

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

  outdf <- NULL
  yearList <- names(popData)

  trials <- vector(mode = "numeric", length = vsize)
  years <- vector(mode = "numeric", length = vsize)
  nas <- vector(mode = "numeric", length = vsize)

  trials[] <- as.numeric(trial)
  nas[] <- NA_real_

  # The first year has no fertility/mortality rates, so add in some NA values.
  # We're going to drop all these values later.
  popData[[1]]$rates.femaleFertility <- nas
  popData[[1]]$rates.maleFertility <- nas
  popData[[1]]$rates.femaleMortality <- nas
  popData[[1]]$rates.maleMortality <- nas

  l <- lapply(yearList, function(year) {
    pop <- popData[[year]]
    m <- as.matrix(pop)
    years[] <- as.numeric(year)
    m <- cbind(trials, years, ageBucket, m)
    return(m)
  })

  return(do.call(rbind, l))
}

#' Save Individual Experiment Results From A Suite As CSV File
#'
#' @param results Results structure (as returned by [RunExperiments()])
#' @param filepath CSV file to write to (default = "out.csv")
#' @param scenario Name of experiments scenario, as passed to \code{RunExperiments()}
#' @param trial Integer index of trial in results structure
#' @param run Run ID
#'
#' @return CSV file contents as a data table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ehep)
#'
#' ehep::InitializePopulation()
#' ehep::InitializeHealthcareTasks()
#' ehep::InitializeScenarios()
#' ehep::InitializeStochasticParameters()
#' ehep::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   ehep::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#'
#' ehep::SaveResults(results, scenario = scenario, trial = 3, run = "RunID")
#' }
SaveResults <- function(results, filepath = "out.csv", scenario, trial = NULL, run){
  if (is.null(trial)) {
    traceMessage(paste0("SaveResults() requires a trial= parameter value"))
    return(NULL)
  }

  if (!assertthat::is.count(trial)) {
    traceMessage(paste0("SaveResults() trial parameter must be a positive integer"))
    return(NULL)
  }

  if (trial < 1 | trial > length(results)) {
    traceMessage(
      paste0(
        "SaveResults() trial parameter out of range. Must be between ",
        1,
        " and ",
        length(results),
        "."
      )
    )
    return(NULL)
  }

  r <- results[[trial]]
  r <- r[1:4]

  # Transpose the task times matrices from (years x tasks) to (tasks x rows),
  # then stitch into a long concatenated version.
  l <- lapply(r, function(taskType){
    m <- t(taskType$Time)
  })

  mt <- do.call(rbind, l)

  # Do the same thing for the task resource counts (no longer necessary, but
  # still computed.)
  l <- lapply(r, function(taskType){
    m <- t(taskType$N)
  })

  mn <- do.call(rbind, l)

  # Convert the task times matrix to a data table, then melt the table so
  # each row is a {year, taskID, service_time} tuple. Then do the same
  # for the task resource counts. (Note: keep.rownames preserves the matrix
  # row names in a column called "rn")
  DTt <- data.table::as.data.table(mt, keep.rownames = TRUE)
  DT <- data.table::melt(DTt, c("rn"))
  names(DT) <- c("Task_ID", "Year", "Service_time")
  DTn <- as.data.table(mn, keep.rownames = TRUE)
  DTn <- data.table::melt(DTn, c("rn"))

  assertthat::are_equal(nrow(DT), length(DTn$value))

  # Add the column of service resource counts column to the output data
  # table (DT). Then add a bunch of fixed value columns.
  DT[, ':=' (Num_services = DTn$value)]
  DT[, ':=' (Scenario_ID = scenario)]
  DT[, ':=' (Trial_num = trial)]
  DT[, ':=' (Run_num = run)]
  DT[, ':=' (Month = NA_integer_)]
  DT[, ':=' (Health_benefit = NA)]

  data.table::setcolorder(DT, c("Task_ID",
                                "Scenario_ID",
                                "Trial_num",
                                "Run_num",
                                "Year",
                                "Month",
                                "Num_services",
                                "Service_time",
                                "Health_benefit"))

  data.table::fwrite(DT, file = filepath, row.names = FALSE, na = "NA")

  return(DT)
}

#' Save Experiment Suite Results As CSV File
#'
#' @param results Results structure as returned by \code{RunExperiments()}
#' @param filepath Location to write CSV file
#' @param scenario Name of experiments scenario, as passed to \code{RunExperiments()}
#' @param run Run ID
#'
#' @return NULL (invisible)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ehep)
#'
#' ehep::InitializePopulation()
#' ehep::InitializeHealthcareTasks()
#' ehep::InitializeScenarios()
#' ehep::InitializeStochasticParameters()
#' ehep::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   ehep::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#'
#' runId <- 1
#'
#' ehep::SaveSuiteResults(results, "results.csv", scenario, runId)
#' }
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
  data.table::fwrite(out, file = filepath, row.names = FALSE, na = "NA")

  return(invisible(NULL))
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

  return(df)
}

.saveSuiteResults1 <- function(results, scenario, trial, run){
  if (is.null(trial)) {
    traceMessage(paste0("SaveResults() requires a trial= parameter value"))
    return(NULL)
  }

  if (!assertthat::is.count(trial)) {
    traceMessage(paste0("SaveResults() trial parameter must be a positive integer"))
    return(NULL)
  }

  if (trial < 1 | trial > length(results)) {
    traceMessage(
      paste0(
        "SaveResults() trial parameter out of range. Must be between ",
        1,
        " and ",
        length(results),
        "."
      )
    )
    return(NULL)
  }

  r <- results[[trial]]
  r <- r[1:4]

  # Transpose the task times matrices from (years x tasks) to (tasks x rows),
  # then stitch into a long concatenated version.
  l <- lapply(r, function(taskType){
    m <- t(taskType$Time)
  })

  mt <- do.call(rbind, l)

  # Do the same thing for the task resource counts (no longer necessary, but
  # still computed.)
  l <- lapply(r, function(taskType){
    m <- t(taskType$N)
  })

  mn <- do.call(rbind, l)

  # Convert the task times matrix to a data table, then melt the table so
  # each row is a {year, taskID, service_time} tuple. Then do the same
  # for the task resource counts. (Note: keep.rownames preserves the matrix
  # row names in a column called "rn")
  DTt <- data.table::as.data.table(mt, keep.rownames = TRUE)
  DT <- data.table::melt(DTt, c("rn"))
  names(DT) <- c("Task_ID", "Year", "Service_time")
  DTn <- as.data.table(mn, keep.rownames = TRUE)
  DTn <- data.table::melt(DTn, c("rn"))

  assertthat::are_equal(nrow(DT), length(DTn$value))

  # Add the column of service resource counts column to the output data
  # table (DT). Then add a bunch of fixed value columns.
  DT[, ':=' (Num_services = DTn$value)]
  DT[, ':=' (Scenario_ID = scenario)]
  DT[, ':=' (Trial_num = trial)]
  DT[, ':=' (Run_num = run)]
  DT[, ':=' (Month = NA_integer_)]
  DT[, ':=' (Health_benefit = NA)]

  data.table::setcolorder(DT, c("Task_ID",
                                "Scenario_ID",
                                "Trial_num",
                                "Run_num",
                                "Year",
                                "Month",
                                "Num_services",
                                "Service_time",
                                "Health_benefit"))

  return(data.table::setDF(DT))
}
