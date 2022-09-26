#' Calculate Annual Time For A Task
#'
#' @param taskID Index of row in task tables
#' @param year Year (index into \code{demographics} list)
#' @param debug Emit debugging information (default = FALSE)
#' @param weeksPerYear Number of weeks per year (default = 48). This value is
#' used to gross up TimeAddedOn tasks from weekly to annual.
#'
#' @return List of two vectors:
#' Time - annual task times in minutes
#' N - number of times task was performed
#'
TaskTime <- function(taskID, year, debug = FALSE, weeksPerYear = 48){
  tp <- EXP$taskParameters

  # TODO: Insert check on length of task table

  taskVals <- tp@values[taskID,]

  # TimeAddedOn tasks are simpler than other tasks; they aren't associated with
  # a population segment, and they aren't dependent on prevalence. But time is
  # returned in a different way: instead of total time required per year to do
  # the tasks, for TimeAddedOn tasks TaskTime returns time per year (in minutes)
  # for the person doing the task.

  if (BVE$taskData$computeMethod[taskID] == "TimeAddedOn"){
    t = taskVals["HoursPerWeek"] * 60 * weeksPerYear
    return(list(N = 1, Time = t))
  }

  # Determine whether this task is covered in the prevalence rates table
  prevalenceRatesTableRow <- which(BVE$stochasticTasks == taskID)
  prevalenceFlag <- (length(prevalenceRatesTableRow) == 1)

  # Look up the population pyramid for this year
  population <- EXP$demographics[[as.character(year)]]

  if (is.null(population)){
    traceMessage(paste("No demographic info for year ", year, sep = ""))
    return(list(N = 0, Time = 0))
  }

  n = 0L

  # Applicable population
  n <- .computeApplicablePopulation(population, BVE$taskData$RelevantPop[taskID])

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

#' Calculate Task Times For A Group Of Tasks
#'
#' Calculate clinical task times for a group of tasks over a spread of years
#'
#' @param taskIDs Vector of task indices into \code{globalPackageEnvironment$taskData}
#' @param years Vector of years (usually \code{globalPackageEnvironment$years})
#' @param weeksPerYear Number of weeks per year (default = 48)
#'
#' @return List with two matrices:
#' Time - annual task times in minutes
#' N - number of times task was performed
#' NULL if the list of task indices is blank
#'
TaskTimesGroup <- function(taskIDs, years, weeksPerYear = 48){
  if (length(taskIDs) == 0){
    return(NULL)
  }

  m <- length(years)
  n <- length(taskIDs)

  mt <-
    matrix(
      nrow = m,
      ncol = n,
      dimnames = list(years, BVE$taskData$Indicator[taskIDs])
    )

  mn <- mt

  for (i in seq_along(years)) {
    for (j in seq_along(taskIDs)) {
      l <- TaskTime(taskIDs[j], years[i], weeksPerYear = weeksPerYear)
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
  # Fail in a big mess if the population labels lookup hasn't been loaded.
  if (is.null(GPE$populationLabels)){
    warning(paste0("Population labels not loaded! Returning 0 for applicable population."))
    return(0)
  }

  l <- GPE$populationLabels
  i <- which(l$Labels == label)

  if (length(i) == 0){
    warning(paste0("Invalid population label: ", label, ". Returning 0 for applicable population."))
    return(0)
  }

  if (length(i) > 1){
    warning(paste0("Duplicate population label: ", label, ". Using first entry."))
    i <- i[1]
  }

  start <- l$Start[i]
  if (is.na(start)){
    start <- GPE$ageMin
  }

  end <- l$End[i]
  if (is.na(end)){
    end <- GPE$ageMax
  }

  total <- 0

  if (l$Female[i]){
    total <- total + sum(pop$Female[(start+1):(end+1)])
  }

  if (l$Male[i]){
    total <- total + sum(pop$Male[(start+1):(end+1)])
  }

  return(total)
}

AllocationTaskTime <- function(taskID, year, baseTime, debug = FALSE){
  tp <- EXP$taskParameters
  taskVals <- tp@values[taskID, ]

  fteRatio <- taskVals["FTEratio"]
  assertthat::assert_that(fteRatio < 1)
  return(baseTime * (fteRatio / (1 - fteRatio)))
}

AllocationTaskTimesGroup <- function(taskIDs, years, baseTimes){
  if (length(taskIDs) == 0){
    return(NULL)
  }

  m <- length(years)
  n <- length(taskIDs)

  mt <-
    matrix(
      nrow = m,
      ncol = n,
      dimnames = list(years, BVE$taskData$Indicator[taskIDs])
    )

  mn <- mt

  for (i in seq_along(years)) {
    for (j in seq_along(taskIDs)) {
      mt[i, j] <- AllocationTaskTime(taskIDs[j], years[i], baseTimes[i])
      mn[i, j] <- 1
    }
  }

  return(list(Time = mt, N = mn))
}
