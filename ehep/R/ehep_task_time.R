#' Calculate Annual Time For Clinical Task
#'
#' @param tasks Dataframe of task parameters (as returned by \code{loadTaskParameters})
#' @param taskID Task ID string
#' @param demographics List of population pyramid dataframes
#' @param year Year (index into \code{demographics} list)
#'
#' @return Annual time in minutes
#'
#' @export
#'
ClinicalTaskTime <- function(tasks, taskID, demographics, year){
  task <- tasks[tasks$Indicator == taskID,]

  if (nrow(task) == 0){
    TraceMessage(paste("Bad task ID ", taskID, sep = ""))
    return(0L)
  }

  if (nrow(task) > 1){
    TraceMessage(paste("Multiple hits for task ID ", taskID, ". Using first hit.", sep = ""))
    task <- task[1,]
  }

  population <- .extractPyramid("demographics", year)

  if (is.null(population)){
    TraceMessage(paste("No demographic info for year ", year, sep = ""))
    return(0L)
  }

  if (is.na(task$MinsPerContact)){
    TraceMessage(paste("MinsPerContact missing for task ", taskID, sep = ""))
    return(0L)
  }

  n = 0L

  # Applicable population
  n <- .computeApplicablePopulation(population, task&RelevantPop)

  # Correct for prevalence, frequency, test positivity, etc
  n <- n * task$StartingRateInPop * task$RateMultiplier

  # Correct for annual decrease in prevalence
  if (task$AnnualDeltaRatio != 1){
    n <- n * (task$AnnualDeltaRatio^(year - globalPackageEnvironment$startYear))
  }

  # Multiply by number of contacts and time per contact
  n <- n * (task$NumContactsPerUnit + task$NumContactsAnnual) * task$MinsPerContact

  return(n)
}

.extractPyramid <- function(varName, year){
  df <- eval(parse(text = paste(varName, "$`", as.character(year), "`", sep = "")))
  return(df)
}

.computeApplicablePopulation <- function(pop, label){
  if (task$RelevantPop == "births"){return(pop$Female[1] + pop$Male[1])}
  if (task$RelevantPop == "1-4"){return(pop$Female[2:5] + pop$Male[2:5])}

  TraceMessage(paste("Unknown population group ", label, sep = ""))
  return(0L)
}
