#' Calculate Annual Time For Clinical Task
#'
#' @param taskID Index of row in task tables
#' @param year Year (index into \code{demographics} list)
#' @param debug Emit debugging information (default = FALSE)
#'
#' @return Annual time in minutes
#'
#' @export
#'
ClinicalTaskTime <- function(taskID, year, debug = FALSE){
  tp <- experimentValuesEnvironment$taskParameters

  # TODO: Insert check on length of task table

  taskVals <- tp@values[taskID,]

  td <- globalPackageEnvironment$taskData

  taskDesc <- td[taskID,]

  if (debug){
    print(taskVals)
    print(taskDesc)
  }

  population <- .extractPyramid("experimentValuesEnvironment$demographics", year)

  if (is.null(population)){
    TraceMessage(paste("No demographic info for year ", year, sep = ""))
    return(0L)
  }

  if (is.na(taskVals["MinsPerContact"])){
    TraceMessage(paste("MinsPerContact missing for task ", taskDesc$Indicator, sep = ""))
    return(0L)
  }

  n = 0L

  # Applicable population
  n <- .computeApplicablePopulation(population, taskDesc$RelevantPop)

  if (debug){
    print(paste("Applicable pop = ", n, sep = ""))
  }

  # Correct for prevalence, frequency, test positivity, etc
  n <- n * taskVals["StartingRateInPop"] * taskVals["RateMultiplier"]

  if (debug){
    print(paste("Adj for prevalence -> ", n, sep = ""))
  }

  # Correct for annual decrease in prevalence
  if (taskVals["AnnualDeltaRatio"] != 1){
    n <- n * (taskVals["AnnualDeltaRatio"]^(year - globalPackageEnvironment$startYear))
  }

  if (debug){
    print(paste("Adj for prevalence decrease -> ", n, sep = ""))
  }

  # Multiply by number of contacts and time per contact
  n <- n *
    (taskVals["NumContactsPerUnit"] + taskVals["NumContactsAnnual"]) *
    taskVals["MinsPerContact"]

  if (debug){
    print(paste("Multiply by contact number and duration -> ", n, sep = ""))
  }

  names(n) <- NULL

  return(n)
}

#' Calculate Clinical Task Times
#'
#' Calculate clinical task times for a group of tasks over a spread of years
#'
#' @param taskIDs Vector of task indices into \code{globalPackageEnvironment$taskData}
#' @param years Vector of years (usually \code{globalPackageEnvironment$years})
#'
#' @return Dataframe of annual times in minutes
#'
#' @export
#'
ClinicalTaskTimesGroup <- function(taskIDs, years){
  df <- data.frame(years)

  nul <- lapply(taskIDs, function(id){
    col <- sapply(years, function(year){
      ClinicalTaskTime(id,year)
    })

    df <<- cbind(df,col)
    return(0)
  })

  taskNames <- globalPackageEnvironment$taskData$Indicator[taskIDs]
  names(df) <- c("Years", taskNames)

  return(df)
}

.extractPyramid <- function(varName, year){
  df <- eval(parse(text = paste(varName, "$`", as.character(year), "`", sep = "")))
  return(df)
}

.computeApplicablePopulation <- function(pop, label){
  if (label == "births"){return(pop$Female[1] + pop$Male[1])}
  if (label == "1-4"){return(sum(pop$Female[2:5] + pop$Male[2:5]))}
  if (label == "1 yo"){return(pop$Female[2] + pop$Male[2])}
  if (label == "2 yo"){return(pop$Female[3] + pop$Male[3])}
  if (label == "15 yo girls"){return(pop$Female[16])}
  if (label == "-"){return(0)}
  if (label == "adults 18+"){return(sum(pop$Female[19:101] + pop$Male[19:101]))}
  if (label == "1-18"){return(sum(pop$Female[2:19] + pop$Male[2:19]))}
  if (label == "all"){return(sum(pop$Female + pop$Male))}
  if (label == "50 yo adults"){return(pop$Female[51] + pop$Male[51])}
  if (label == "adults 50+"){return(sum(pop$Female[51:101] + pop$Male[51:101]))}
  if (label == "30 yo adults"){return(pop$Female[31] + pop$Male[31])}
  if (label == "18 yo women"){return(pop$Female[19])}
  if (label == "women 15-49"){return(sum(pop$Female[16:50]))}

  TraceMessage(paste("Unknown population group ", label, sep = ""))
  return(0L)
}

# [1] "births"       "1-4"          "1 yo"         "2 yo"         "15 yo girls"  "-"            "adults 18+"
# [8] "1-18"         "all"          "50 yo adults" "adults 50+"   "30 yo adults" "18 yo women"  "women 15-49"
