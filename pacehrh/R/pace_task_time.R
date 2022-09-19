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

  # print(paste0(label," : ",total," ? ",.computeApplicablePopulation_old(pop, label)))
  # assertthat::assert_that(total == .computeApplicablePopulation_old(pop, label))
}

# .computeApplicablePopulation_old <- function(pop, label) {
#   if (label == "births") {
#     return(pop$Female[1] + pop$Male[1])
#   }
#   if (label == "1-4") {
#     return(sum(pop$Female[2:5] + pop$Male[2:5]))
#   }
#   if (label == "1 yo") {
#     return(pop$Female[2] + pop$Male[2])
#   }
#   if (label == "2 yo") {
#     return(pop$Female[3] + pop$Male[3])
#   }
#   if (label == "15 yo girls") {
#     return(pop$Female[16])
#   }
#   if (label == "-") {
#     return(0)
#   }
#   if (label == "adults 18+") {
#     return(sum(pop$Female[19:101] + pop$Male[19:101]))
#   }
#   if (label == "men 18+") {
#     return(sum(pop$Male[19:101]))
#   }
#   if (label == "1-18") {
#     return(sum(pop$Female[2:19] + pop$Male[2:19]))
#   }
#   if (label == "children 0-9") {
#     return(sum(pop$Female[1:10] + pop$Male[1:10]))
#   }
#   if (label == "5-18") {
#     return(sum(pop$Female[6:19] + pop$Male[6:19]))
#   }
#   if (label == "all") {
#     return(sum(pop$Female + pop$Male))
#   }
#   if (label == "50 yo adults") {
#     return(pop$Female[51] + pop$Male[51])
#   }
#   if (label == "adults 50+") {
#     return(sum(pop$Female[51:101] + pop$Male[51:101]))
#   }
#   if (label == "adults 35+") {
#     return(sum(pop$Female[36:101] + pop$Male[36:101]))
#   }
#   if (label == "30 yo adults") {
#     return(pop$Female[31] + pop$Male[31])
#   }
#   if (label == "18 yo women") {
#     return(pop$Female[19])
#   }
#   if (label == "women 15-49") {
#     return(sum(pop$Female[16:50]))
#   }
#   if (label == "women 30-49") {
#     return(sum(pop$Female[31:50]))
#   }
#   if (label == "men 15-49") {
#     return(sum(pop$Male[16:50]))
#   }
#   if (label == "children 5-9") {
#     return(sum(pop$Female[6:10] + pop$Male[6:10]))
#   }
#   if (label == "children 10-14") {
#     return(sum(pop$Female[11:15] + pop$Male[11:15]))
#   }
#   if (label == "adults 15-19") {
#     return(sum(pop$Female[16:20] + pop$Male[16:20]))
#   }
#   if (label == "18 yo adults") {
#     return(sum(pop$Female[19] + pop$Male[19]))
#   }
#   if (label == "15 yo") {
#     return(sum(pop$Female[16] + pop$Male[16]))
#   }
#   if (label == "adults 15-24") {
#     return(sum(pop$Female[16:25] + pop$Male[16:25]))
#   }
#   if (label == "adults 25-34") {
#     return(sum(pop$Female[26:35] + pop$Male[26:35]))
#   }
#   if (label == "adults 35-44") {
#     return(sum(pop$Female[36:45] + pop$Male[36:45]))
#   }
#   if (label == "adults 45-54") {
#     return(sum(pop$Female[46:55] + pop$Male[46:55]))
#   }
#   if (label == "adults 55-64") {
#     return(sum(pop$Female[56:65] + pop$Male[56:65]))
#   }
#   if (label == "adults 65+") {
#     return(sum(pop$Female[66:101] + pop$Male[66:101]))
#   }
#   if (label == "adults 70+") {
#     return(sum(pop$Female[71:101] + pop$Male[71:101]))
#   }
#   if (label == "adults 18-30") {
#     return(sum(pop$Female[19:31] + pop$Male[19:31]))
#   }
#   if (label == "adults 31-44") {
#     return(sum(pop$Female[32:45] + pop$Male[32:45]))
#   }
#   if (label == "women 20-29") {
#     return(sum(pop$Female[21:30]))
#   }
#   if (label == "women 30-39") {
#     return(sum(pop$Female[31:40]))
#   }
#   if (label == "women 40-49") {
#     return(sum(pop$Female[41:50]))
#   }
#   if (label == "women 50-59") {
#     return(sum(pop$Female[51:60]))
#   }
#   if (label == "women 60-69") {
#     return(sum(pop$Female[61:70]))
#   }
#   if (label == "men 20-29") {
#     return(sum(pop$Male[21:30]))
#   }
#   if (label == "men 30-39") {
#     return(sum(pop$Male[31:40]))
#   }
#   if (label == "men 40-49") {
#     return(sum(pop$Male[41:50]))
#   }
#   if (label == "men 50-59") {
#     return(sum(pop$Male[51:60]))
#   }
#   if (label == "men 60-69") {
#     return(sum(pop$Male[61:70]))
#   }
#
#   traceMessage(paste("Unknown population group ", label, sep = ""))
#   return(0L)
# }

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
