#' Load Healthcare Task Information
#'
#' Read the healthcare task information from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Data frame of healthcare task parameters
#'
loadTaskParameters <- function(sheetName = "TaskValues"){
  taskData <- NULL

  if (file.exists(GPE$inputExcelFile)){
    out <- tryCatch(
      {
        taskData <-
          readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)
      },
      warning = function(war)
      {
        ehep::TraceMessage(paste("WARNING:", war))
      },
      error = function(err)
      {
        ehep::TraceMessage(paste("ERROR:", err))
      },
      finally =
        {

        }
    )
  } else {
    ehep::TraceMessage(paste("Could not find model input file ",
                             GPE$inputExcelFile,
                             sep = ""))
  }

  if (is.null(taskData)){
    return(NULL)
  }

  # Convert some of the NA values to sensible defaults
  assertthat::has_name(taskData, "StartingRateInPop")
  taskData$StartingRateInPop[is.na(taskData$StartingRateInPop)] <- 0

  assertthat::has_name(taskData, "RateMultiplier")
  taskData$RateMultiplier[is.na(taskData$RateMultiplier)] <- 1

  assertthat::has_name(taskData, "AnnualDeltaRatio")
  taskData$AnnualDeltaRatio[is.na(taskData$AnnualDeltaRatio)] <- 1

  assertthat::has_name(taskData, "NumContactsPerUnit")
  taskData$NumContactsPerUnit[is.na(taskData$NumContactsPerUnit)] <- 0

  assertthat::has_name(taskData, "NumContactsAnnual")
  taskData$NumContactsAnnual[is.na(taskData$NumContactsAnnual)] <- 0

  assertthat::has_name(taskData, "HoursPerWeek")
  taskData$HoursPerWeek[is.na(taskData$HoursPerWeek)] <- 0

  assertthat::has_name(taskData, "FTEratio")
  taskData$FTEratio[is.na(taskData$FTEratio)] <- 0

  computeMethod <- replicate(nrow(taskData), "TimePerTask")
  computeMethod[taskData$FTEratio != 0] <- "TimeRatio"
  computeMethod[taskData$HoursPerWeek != 0] <- "TimeAddedOn"
  taskData$computeMethod <- computeMethod

  # Apply some rules to determine which tasks are affected by stochastically
  # changing prevalence.
  #
  # Stochastically changing prevalence only applies to TimePerTask computations.
  #
  # If StartingRateInPop == 1, do NOT compute prevalence. Stochasticity for
  # these tasks is driven by the population dynamics.

  applyStochasticity <- replicate(nrow(taskData), TRUE)
  applyStochasticity[taskData$StartingRateInPop == 1] <- FALSE
  applyStochasticity[taskData$computeMethod != "TimePerTask"] <- FALSE
  taskData$applyStochasticity <- applyStochasticity

  return(taskData)
}

#' Initialize Healthcare Task Information
#'
#' Read the healthcare task information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @export
#'
#' @return NULL (invisible)
#'
InitializeHealthcareTasks <- function(...){
  .checkAndLoadGlobalConfig()

  args = list(...)

  if (length(args) == 0){
    return(invisible(NULL))
  } else {
    taskData <- loadTaskParameters(...)

    # TODO: Insert error handling

    GPE$taskData <- taskData
    GPE$taskDataDims <- dim(GPE$taskData)
    GPE$stochasticTasks <- which(GPE$taskData$applyStochasticity)
    return(invisible(NULL))
  }
}
