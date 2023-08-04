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
#' @noRd
loadTaskParameters <- function(sheetName = .defaultTaskValuesSheet) {
  traceMessage(paste0("Loading task values sheet ", sheetName))

  taskData <- NULL

  taskData <- readSheet(sheetName = sheetName)

  if (is.null(taskData)) {
    return(NULL)
  }

  taskData <-
    validateTableAgainstSchema(taskData,
      .taskValuesMetaData,
      convertType = TRUE
    )

  if (!is.null(taskData)) {
    # Convert some of the NA values to sensible defaults. Note that
    # validateTableAgainstSchema() guarantees the presence of these columns.
    taskData$StartingRateInPop[is.na(taskData$StartingRateInPop)] <- 0
    taskData$RateMultiplier[is.na(taskData$RateMultiplier)] <- 1
    taskData$AnnualDeltaRatio[is.na(taskData$AnnualDeltaRatio)] <- 1
    taskData$NumContactsPerUnit[is.na(taskData$NumContactsPerUnit)] <- 0
    taskData$NumContactsAnnual[is.na(taskData$NumContactsAnnual)] <- 0
    taskData$HoursPerWeek[is.na(taskData$HoursPerWeek)] <- 0

    computeMethod <- replicate(nrow(taskData), "TimePerTask")
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
  }

  return(taskData)
}
