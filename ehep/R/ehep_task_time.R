#' Calculate Annual Time For A Task
#'
#' @param taskID Index of row in task tables
#' @param year Year (index into \code{demographics} list)
#' @param debug Emit debugging information (default = FALSE)
#'
#' @return List of two vectors:
#' Time - annual task times in minutes
#' N - number of times task was performed
#'
#' @export
#'
TaskTime <- function(taskID, year, debug = FALSE){
  tp <- EXP$taskParameters

  # TODO: Insert check on length of task table

  taskVals <- tp@values[taskID,]

  if (debug){
    .dispTaskInfo(taskID, taskVals)
  }

  # Determine whether this task is covered in the prevalence rates table
  prevalenceRatesTableRow <- which(GPE$stochasticTasks == taskID)
  prevalenceFlag <- (length(prevalenceRatesTableRow) == 1)

  # Look up the population pyramid for this year
  population <- EXP$demographics[[as.character(year)]]

  if (is.null(population)){
    TraceMessage(paste("No demographic info for year ", year, sep = ""))
    return(list(N = 0, Time = 0))
  }

  n = 0L

  # Applicable population
  n <- .computeApplicablePopulation(population, GPE$taskData$RelevantPop[taskID])

  if (debug){
    cat(paste("Applicable pop = ", n, "\n", sep = ""))
  }

  # Correct for prevalence, frequency, test positivity, etc
  m <- EXP$prevalenceRatesMatrix
  prevalenceMultiplier <-
    ifelse(prevalenceFlag, m[prevalenceRatesTableRow, as.character(year)], 1)

  n <- n * prevalenceMultiplier * taskVals["RateMultiplier"]

  if (debug){
    cat(paste("Adj for prevalence -> ", n, "\n", sep = ""))
  }

  # Multiply by number of contacts and time per contact
  n <- n *
    (taskVals["NumContactsPerUnit"] + taskVals["NumContactsAnnual"])

  # Set aside the number of times the service was provided
  numServices <- round(n, 0)
  names(numServices) <- NULL

  if (is.na(taskVals["MinsPerContact"])){
    TraceMessage(paste("MinsPerContact missing for task ", GPE$taskData$Indicator[taskID], sep = ""))
    t = 0
  } else {
    t <- numServices * taskVals["MinsPerContact"]
#    t <- n * taskVals["MinsPerContact"]
  }

  if (debug){
    cat(paste("Multiply by contact number and duration -> ", t, "\n", sep = ""))
  }

  names(t) <- NULL

  return(list(N = numServices, Time = t))
}

.dispTaskInfo <- function(taskId, taskVals) {
  cat(paste("ID:", GPE$taskData$Indicator[taskId], "\n", sep = ""))
  cat(paste("CommonName:", GPE$taskData$CommonName[taskId], "\n", sep = ""))
  cat(paste("RelevantPop:", GPE$taskData$RelevantPop[taskId], "\n", sep = ""))
  cat(paste("StartingRateInPop:", taskVals["StartingRateInPop"], "\n", sep = ""))
  cat(paste("RateMultiplier:", taskVals["RateMultiplier"], "\n", sep = ""))
  cat(paste("AnnualDeltaRatio:", taskVals["AnnualDeltaRatio"], "\n", sep = ""))
  cat(paste("NumContactsPerUnit:", taskVals["NumContactsPerUnit"], "\n", sep = ""))
  cat(paste("NumContactsAnnual:", taskVals["NumContactsAnnual"], "\n", sep = ""))
  cat(paste("MinsPerContact:", taskVals["MinsPerContact"], "\n", sep = ""))
  cat(paste("HoursPerWeek:", taskVals["HoursPerWeek"], "\n", sep = ""))
  cat(paste("FTEratio:", taskVals["FTEratio"], "\n", sep = ""))
}

#' Calculate Task Times For A Group Of Tasks
#'
#' Calculate clinical task times for a group of tasks over a spread of years
#'
#' @param taskIDs Vector of task indices into \code{globalPackageEnvironment$taskData}
#' @param years Vector of years (usually \code{globalPackageEnvironment$years})
#'
#' @return List with two matrices:
#' Time - annual task times in minutes
#' N - number of times task was performed
#'
#' @export
#'
TaskTimesGroup <- function(taskIDs, years){
  m <- length(years)
  n <- length(taskIDs)

  mt <-
    matrix(
      nrow = m,
      ncol = n,
      dimnames = list(years, GPE$taskData$Indicator[taskIDs])
    )

  mn <- mt

  for (i in seq_along(years)) {
    for (j in seq_along(taskIDs)) {
      l <- TaskTime(taskIDs[j], years[i])
      mt[i, j] <- l$Time
      mn[i, j] <- l$N
    }
  }

  return(list(Time = mt, N = mn))
}

.extractPyramid <- function(varName, year){
  df <- eval(parse(text = paste(varName, "$`", as.character(year), "`", sep = "")))
  return(df)
}

.computeApplicablePopulation <- function(pop, label) {
  if (label == "births") {
    return(pop$Female[1] + pop$Male[1])
  }
  if (label == "1-4") {
    return(sum(pop$Female[2:5] + pop$Male[2:5]))
  }
  if (label == "1 yo") {
    return(pop$Female[2] + pop$Male[2])
  }
  if (label == "2 yo") {
    return(pop$Female[3] + pop$Male[3])
  }
  if (label == "15 yo girls") {
    return(pop$Female[16])
  }
  if (label == "-") {
    return(0)
  }
  if (label == "adults 18+") {
    return(sum(pop$Female[19:101] + pop$Male[19:101]))
  }
  if (label == "1-18") {
    return(sum(pop$Female[2:19] + pop$Male[2:19]))
  }
  if (label == "all") {
    return(sum(pop$Female + pop$Male))
  }
  if (label == "50 yo adults") {
    return(pop$Female[51] + pop$Male[51])
  }
  if (label == "adults 50+") {
    return(sum(pop$Female[51:101] + pop$Male[51:101]))
  }
  if (label == "30 yo adults") {
    return(pop$Female[31] + pop$Male[31])
  }
  if (label == "18 yo women") {
    return(pop$Female[19])
  }
  if (label == "women 15-49") {
    return(sum(pop$Female[16:50]))
  }

  TraceMessage(paste("Unknown population group ", label, sep = ""))
  return(0L)
}

AllocationTaskTime <- function(taskID, year, baseTime, debug = FALSE){
  tp <- EXP$taskParameters
  taskVals <- tp@values[taskID, ]

  if (debug) {
    .dispTaskInfo(taskID, taskVals)
  }

  fteRatio <- taskVals["FTEratio"]
  assertthat::assert_that(fteRatio < 1)
  return(baseTime * (fteRatio / (1 - fteRatio)))
}

AllocationTaskTimesGroup <- function(taskIDs, years, baseTimes){
  assertthat::assert_that(length(baseTimes) == length(years))

  df <- data.frame(years)

  nul <- lapply(taskIDs, function(id){
    col <- sapply(seq_along(years), function(i){
      AllocationTaskTime(id, years[i], baseTimes[i])
    })

    df <<- cbind(df,col)
    return(0)
  })

  taskNames <- GPE$taskData$Indicator[taskIDs]
  names(df) <- c("Years", taskNames)

  return(df)
}
