# Technical Note:

# The code to compute per-age task times produces a lot of output. (100 tasks *
# 25 years * 101 ages * 2 genders = 505,000 data points per experiment.) This
# code exploits R environments to reduce how often large data structures are
# passed back and forth between functions. (Environments are passed by
# reference, unlike most other R constructs which are passed by value.)
#
# Computations are done in three stages:
#
# Step 1: Compute per-year task times. Example: affected population = 1000,
# encounters per year = 4, time per encounter = 10 minutes --> N(annual) = 4000,
# T(annual) = 40,000
#
# Step 2: Apply seasonality curves to generate per-month task times.
#
# Step 3: Correct the per-month values generated in the previous step for task
# offsets. Task offsets are time displacements between an event and when
# healthcare tasks associated with the event actually happen. For example,
# births are preceded by several pre-natal visits.
#
# Rounding
# --------
# Per-age statistics are always computed without rounding. We do this because
# per-age stats stratify the population into about 100 much smaller bins,
# increasing the likelihood that small-but-not-zero statistics are rounded to
# zero and disappear from later analysis.
#
# Task Types
# ----------
# Per-age stats only make sense for clinical tasks associated with population
# ranges. Other kinds of per-HCW administrative tasks - computeMethod ==
# "TimeAddedOn" or computeMethod == "TimeRatio" - are excluded from the
# final reported results.
#

ComputePerAgeTaskTimes <- function(e){
  if (GPE$perAgeStats == "off"){
    return(invisible(NULL))
  }
  
  e$AnnualPerAge <- NULL
  .computeAnnualTaskTimes(e)
  
  # Stop if user only wants annual stats (much faster to calculate, but not
  # as accurate since seasonality and seasonality offsets are ignored).
  if (GPE$perAgeStats == "annual"){
    return(invisible(NULL))
  }
  
  # Continue if user wants monthly stats (GPE$perAgeStats == "monthly")
  if (GPE$perAgeStats == "monthly"){
    if (!is.null(e$AnnualPerAge)){
      .computeMonthlyTaskTimes(e)
    }

    remove(AnnualPerAge, pos = e)
  }
  
  return(invisible(NULL))
}

.computeAnnualTaskTimes <- function(e)
{
  tasks <- BVE$taskData
  taskvals <- EXP$taskParameters
  prm <- EXP$populationRangeMatrices
  pm <- EXP$prevalenceRatesMatrix

  taskIds <- tasks$Indicator

  # Blank minutes-per-contact values should be zero
  mpc <- taskvals[,"MinsPerContact"]
  mpc[is.na(mpc)] <- 0

  # Initialize 3-D matrices (Tasks, Ages, Years) to hold the results
  nYears <- length(BVE$years)
  nTasks <- nrow(BVE$taskData)
  nAges <- length(GPE$ages)

  mfT <- array(dim = c(nTasks, nAges, nYears)) # Matrix of female task times
  mmT <- array(dim = c(nTasks, nAges, nYears)) # Matrix of male task times
  mfN <- array(dim = c(nTasks, nAges, nYears)) # Matrix of female task counts
  mmN <- array(dim = c(nTasks, nAges, nYears)) # Matrix of male task counts

  years <- as.character(BVE$years)

  for (i in seq_along(years)){
    year <- years[i]

    # Extract matrices (Tasks x Ages) giving the applicable population for each
    # task broken out by ages.
    fm <- prm$FemaleRanges[[year]][tasks$popRangeMaskPtr,]
    mm <- prm$MaleRanges[[year]][tasks$popRangeMaskPtr,]

    # Get the vector of prevalence rates to apply to each task for this year.
    p <- as.vector(pm[, year])

    # Compute the multiplier factors that will convert applicable population
    # counts to numbers of clinical events.
    mul <- p *
      taskvals[,"RateMultiplier"] *
      (taskvals[,"NumContactsPerUnit"] + taskvals[,"NumContactsAnnual"])

    # Note: this code takes advantage of vector recycling! The mul vector is the
    # same length as a column of fm or mm, so fm * mul has the effect of
    # multiplying each column of the fm matrix by mul.
    fN <- fm * mul
    mN <- mm * mul

    # if (GPE$roundingLaw != "none"){
    #   fN <- round(fN, 0)
    #   mN <- round(mN, 0)
    # }

    mfN[,,i] <- fN
    mmN[,,i] <- mN

    # Compute the total time spent executing tasks
    fT <- fN * mpc
    mT <- mN * mpc

    mfT[,,i] <- fT
    mmT[,,i] <- mT
  }

  dimnames(mfT) <- list(Task = BVE$taskData$Indicator, Age = GPE$ages, Year = BVE$years)
  dimnames(mmT) <- list(Task = BVE$taskData$Indicator, Age = GPE$ages, Year = BVE$years)
  dimnames(mfN) <- list(Task = BVE$taskData$Indicator, Age = GPE$ages, Year = BVE$years)
  dimnames(mmN) <- list(Task = BVE$taskData$Indicator, Age = GPE$ages, Year = BVE$years)
  
  e$AnnualPerAge$Times$Female <- mfT
  e$AnnualPerAge$Times$Male <- mmT
  e$AnnualPerAge$Counts$Female <- mfN
  e$AnnualPerAge$Counts$Male <- mmN

  return(invisible(NULL))
}

.computeMonthlyTaskTimes <- function(e = NULL){
  if (is.null(e)){
    return(invisible(NULL))
  }

  if (is.null(e$AnnualPerAge)){
    return(invisible(NULL))
  }

  .allocateResultsMatrices(e)
  .processTasks(e)
  .labelDimensions(e)
  .filterResultsMatrices(e)
  # .roundResults(e)

  return(invisible(NULL))
}

.allocateResultsMatrices <- function(e){
  nMonths <- length(BVE$years) * 12
  nTasks <- nrow(BVE$taskData)
  nAges <- length(GPE$ages)

  e$Times$Female <- array(dim = c(nTasks, nAges, nMonths))
  e$Times$Male <- array(dim = c(nTasks, nAges, nMonths))
  e$Counts$Female <- array(dim = c(nTasks, nAges, nMonths))
  e$Counts$Male <- array(dim = c(nTasks, nAges, nMonths))
}

.filterResultsMatrices <- function(e){
  # Restrict to TimePerTask tasks (see Technical Note at the top of this file)
  tptMask <- which(BVE$taskData$computeMethod == "TimePerTask")

  # Strip shoulder years off the final reported values
  monthRange <- 1:((length(BVE$years) - GPE$shoulderYears) * 12)

  e$Times$Female <- e$Times$Female[tptMask,,monthRange]
  e$Times$Male <- e$Times$Male[tptMask,,monthRange]
  e$Counts$Female <- e$Counts$Female[tptMask,,monthRange]
  e$Counts$Male <- e$Counts$Male[tptMask,,monthRange]
}

# .roundResults <- function(e){
#   if (GPE$roundingLaw != "none"){
#     e$Times$Female <- round(e$Times$Female, 0)
#     e$Times$Male <- round(e$Times$Male, 0)
#     e$Counts$Female <- round(e$Counts$Female, 0)
#     e$Counts$Male <- round(e$Counts$Male, 0)
#   }
# }

.labelDimensions <- function(e){
  monthDimLabels <- as.vector(sapply(BVE$years, function(year){
    return(paste(year, as.character(1:12),sep = "."))
  }))

  dimnames(e$Times$Female) <-
    list(
      Task = BVE$taskData$Indicator,
      Age = GPE$ages,
      Year.Month = monthDimLabels
    )
  dimnames(e$Times$Male) <-
    list(
      Task = BVE$taskData$Indicator,
      Age = GPE$ages,
      Year.Month = monthDimLabels
    )
  dimnames(e$Counts$Female) <-
    list(
      Task = BVE$taskData$Indicator,
      Age = GPE$ages,
      Year.Month = monthDimLabels
    )
  dimnames(e$Counts$Male) <-
    list(
      Task = BVE$taskData$Indicator,
      Age = GPE$ages,
      Year.Month = monthDimLabels
    )
}

.monthCols <- c("Jan", "Feb", "Mar", "Apr", "May", "June",
                "July", "Aug", "Sept", "Oct", "Nov", "Dec")

.offsetCols <- c("Offset1", "Offset2", "Offset3", "Offset4", "Offset5", "Offset6")

.allowedSexValues <- c("f", "m")
.allowedTypeValues <- c("t", "n")
.defaultSexValue <- .allowedSexValues[1]
.defaultTypeValue <- .allowedTypeValues[1]

.processTasks <- function(e){
  for (sex in .allowedSexValues){
    for (type in .allowedTypeValues){
      .processSeasonalTasks(e, sex, type)
      .processNonSeasonalTasks(e, sex, type)
    }
  }
}

#' Get the Correct Matrix of Annual Task Times or Counts
#'
#' @param e Environment for returned values
#' @param sex Allowed values = "f" | "m"
#' @param type Allowed values = "t" | "n"
#' @param i Index value into Tasks dimension of annual values 3-D matrix
#'
#' @return Ages x Years matrix of time or count values for the task at index i
#'
#' @noRd
.getAnnualTaskMatrix <- function(e = NULL, sex = .defaultSexValue, type = .defaultTypeValue, i){
  if (is.null(e)){
    return(NULL)
  }

  # F = female, M = male
  if (!(tolower(sex) %in% .allowedSexValues)){
    return(NULL)
  }

  # T = time, N = count
  if (!(tolower(type) %in% .allowedTypeValues)){
    return(NULL)
  }

  m <- NULL

  if (tolower(type) == "t"){
    if (tolower(sex) == "f"){
      m <- e$AnnualPerAge$Times$Female[i,,]
    } else {
      m <- e$AnnualPerAge$Times$Male[i,,]
    }
  } else { # type == "n"
    if (tolower(sex) == "f"){
      m <- e$AnnualPerAge$Counts$Female[i,,]
    } else {
      m <- e$AnnualPerAge$Counts$Male[i,,]
    }
  }

  return(m)
}

#' Process Tasks With A Seasonality Component
#'
#' @param e Environment for returned values
#' @param sex Allowed values = "f" | "m"
#' @param type Allowed values = "t" | "n"
#'
#' @return NULL (invisible)
#'
#' @noRd
.processSeasonalTasks <- function(e = NULL, sex = .defaultSexValue, type = .defaultTypeValue){
  if (is.null(e)){
    return(invisible(NULL))
  }

  # Get the list of indices for seasonality-affected tasks
  taskIndexes <- which(BVE$taskData$Indicator %in% BVE$seasonalTasks)
  for(i in taskIndexes){
    # mi is the Ages x Years matrix of task values for the task at index i
    mi <- .getAnnualTaskMatrix(e, sex, type, i)

    id <- BVE$taskData$Indicator[i]

    so <- BVE$seasonalityOffsetsEx
    soIndex <- which(so$Task == id)
    curve <- unlist(so[soIndex, .monthCols])

    # Build an expanded Months x Ages matrix based on the seasonality curve for this task
    mcurve <- matrix(data = curve, ncol = 1)
    xi <- apply(mi, 1, function(y){
      return(as.vector(mcurve %*% matrix(data = y, nrow = 1)))
    })

    # Build a scatter matrix based on the offsets. The scatter matrix redistributes
    # the monthly values generated in the previous step into the months that
    # task activities actually happen.
    nRows <- nCols <- dim(xi)[1]
    A <- matrix(0, nrow = nRows, ncol = nCols)

    offsets <- unlist(so[soIndex, .offsetCols])
    offsets <- offsets[!is.na(offsets)]
    nOffsets <- length(offsets)

    # TODO: capture the case where nOffsets == 1 and offsets[1] == 0 ... nothing
    # needed to adjust the xi matrix

    cellValue <- (1 / nOffsets)

    for (r in 1:nRows){
      for (offset in offsets){
        c <- r - offset
        if ((c >= 1) & (c <= nCols)){
          A[r,c] <- cellValue
        }
      }
    }

    # Apply the scatter matrix
    Xi <- A %*% xi

    if (tolower(type) == "t"){
      if (tolower(sex) == "f"){
        e$Times$Female[i,,] <- t(Xi)
      } else {
        e$Times$Male[i,,] <- t(Xi)
      }
    } else { # type == "n")
      if (tolower(sex) == "f"){
        e$Counts$Female[i,,] <- t(Xi)
      } else {
        e$Counts$Male[i,,] <- t(Xi)
      }
    }
  }

  return(invisible(NULL))
}

#' Compute Counts and Times for Task Without Defined Seasonality Curves
#'
#' @param e Environment for returned values
#' @param sex Allowed values = "f" | "m"
#' @param type Allowed values = "t" | "n"
#'
#' @return NULL (invisible)
#'
#' @noRd
.processNonSeasonalTasks <- function(e = NULL, sex = .defaultSexValue, type = .defaultTypeValue){
  if (is.null(e)){
    return(invisible(NULL))
  }

  # Default seasonality curve - spread annual values evenly across 12 months
  defSeasonalityCurve <- c(1,1,1,1,1,1,1,1,1,1,1,1)/12

  taskIndexes <- which(!(BVE$taskData$Indicator %in% BVE$seasonalTasks))
  for(i in taskIndexes){
    # mi is the Ages x Years matrix of task values for the task at index i
    mi <- .getAnnualTaskMatrix(e, sex, type, i)

    # Build an expanded Months x Ages matrix based on the seasonality curve for this task
    mcurve <- matrix(data = defSeasonalityCurve, ncol = 1)
    xi <- apply(mi, 1, function(y){
      return(as.vector(mcurve %*% matrix(data = y, nrow = 1)))
    })

    if (tolower(type) == "t"){
      if (tolower(sex) == "f"){
        e$Times$Female[i,,] <- t(xi)
      } else {
        e$Times$Male[i,,] <- t(xi)
      }
    } else { # type == "n")
      if (tolower(sex) == "f"){
        e$Counts$Female[i,,] <- t(xi)
      } else {
        e$Counts$Male[i,,] <- t(xi)
      }
    }
  }

  return(invisible(NULL))
}
