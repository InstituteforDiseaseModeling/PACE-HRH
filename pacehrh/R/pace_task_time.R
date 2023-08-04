#' Calculate Task Times Based On BVE and EXP Values
#'
#' @return List of two matrices:
#' Time - annual task times in minutes
#' N - number of times task was performed
#'
TaskTimes <- function() {
  # Grab some values to reduce the flurry of $ signs
  weeksPerYear <- BVE$scenario$WeeksPerYr
  tasks <- BVE$taskData
  taskvals <- EXP$taskParameters
  prm <- EXP$populationRangeMatrices
  pm <- EXP$prevalenceRatesMatrix
  taskIds <- tasks$Indicator

  # Blank minutes-per-contact values should be zero
  mpc <- taskvals[, "MinsPerContact"]
  mpc[is.na(mpc)] <- 0

  l <- lapply(BVE$years, function(year) {
    year <- as.character(year)

    # n = applicable population
    # p = prevalence rate
    n <- as.vector(prm$Total[, year][tasks$popRangeMaskPtr])
    p <- as.vector(pm[, year])

    # Compute the number of executed tasks ("service count")
    tasksN <- n * p * taskvals[, "RateMultiplier"] *
      (taskvals[, "NumContactsPerUnit"] + taskvals[, "NumContactsAnnual"])

    # Round to an integer, unless rounding is turned off
    if (GPE$roundingLaw != "none") {
      tasksN <- round(tasksN, 0)
    }

    # Compute the total time spent executing tasks
    tasksT <- tasksN * mpc

    # Perform a separate calculation for TimeAddedOn tasks
    taoMask <- (tasks$computeMethod == "TimeAddedOn")
    if (sum(taoMask) > 0) {
      tao <- taskvals[, "HoursPerWeek"] * 60 * weeksPerYear
      tasksT[taoMask] <- tao[taoMask]
      tasksN[taoMask] <- 1
    }

    names(tasksN) <- taskIds
    names(tasksT) <- taskIds

    return(list(N = tasksN, Time = tasksT))
  })

  t <- do.call(cbind, lapply(l, function(ll) {
    ll$Time
  }))
  colnames(t) <- BVE$years
  n <- do.call(cbind, lapply(l, function(ll) {
    ll$N
  }))
  colnames(n) <- BVE$years
  return(list(N = n, Time = t))
}

.extractPyramid <- function(varName, year) {
  df <- eval(parse(text = paste(varName, "$`", as.character(year), "`", sep = "")))
  return(df)
}

.getApplicablePopulation <- function(year, label) {
  return(EXP$populationRangeMatrices$Total[label, as.character(year)])
}

.computeApplicablePopulation <- function(pop, label) {
  # Fail in a big mess if the population labels lookup hasn't been loaded.
  if (is.null(BVE$populationLabels)) {
    warning(paste0("Population labels not loaded! Returning 0 for applicable population."))
    return(0)
  }

  l <- BVE$populationLabels
  i <- which(l$Labels == label)

  if (length(i) == 0) {
    warning(paste0("Invalid population label: ", label, ". Returning 0 for applicable population."))
    return(0)
  }

  if (length(i) > 1) {
    warning(paste0("Duplicate population label: ", label, ". Using first entry."))
    i <- i[1]
  }

  start <- l$Start[i]
  if (is.na(start)) {
    start <- GPE$ageMin
  }

  end <- l$End[i]
  if (is.na(end)) {
    end <- GPE$ageMax
  }

  total <- 0

  if (l$Female[i]) {
    total <- total + sum(pop$Female[(start + 1):(end + 1)])
  }

  if (l$Male[i]) {
    total <- total + sum(pop$Male[(start + 1):(end + 1)])
  }

  return(total)
}
