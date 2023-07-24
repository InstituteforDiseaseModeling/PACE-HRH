#' Load Stochastic Parameters
#'
#' @param stochasticParametersSheetName Sheet name from the model input Excel file
#'
#' @return Dataframe of stochastic parameters
#'
#' @noRd
loadStochasticParameters <- function(stochasticParametersSheetName = .defaultStochasticParmsSheet) {
  traceMessage(paste0("Loading stochastic parameters sheet ", stochasticParametersSheetName))

  stochData <- tryCatch({
    readxl::read_xlsx(GPE$inputExcelFile, sheet = stochasticParametersSheetName)
  },
  error = function(e){
    return(NULL)
  },
  finally = {
  })

  if (is.null(stochData)){
    warning("Could not read stochastic parameters sheet")
    return(NULL)
  }

  stochData <-
    validateTableAgainstSchema(stochData,
                               .stochasticParametersMetaData,
                               convertType = FALSE)

  return(stochData)
}

#' Load Change Rate Limits
#'
#' @param changeRateLimitsSheetName Sheet name from the model input Excel file
#'
#' @return Dataframe of change rate limits
#'
#' @noRd
loadChangeRateLimits <- function(changeRateLimitsSheetName = .defaultChangeRateLimitsSheet) {
  traceMessage(paste0("Loading change rate limits sheet ", changeRateLimitsSheetName))

  limitData <- tryCatch({
    readxl::read_xlsx(GPE$inputExcelFile, sheet = changeRateLimitsSheetName)
  },
  error = function(e){
    return(NULL)
  },
  finally = {
  })

  if (is.null(limitData)){
    warning("Could not read change rate limits sheet")
    return(NULL)
  }

  limitData <-
    validateTableAgainstSchema(limitData,
                               .changeRateLimitsMetaData,
                               convertType = FALSE)

  limitData <- .validateChangeRateLimits(limitData)

  return(limitData)
}

.validateChangeRateLimits <- function(limitData) {
  if (is.null(limitData)) {
    return(NULL)
  }

  # Sanity check data types (should already have been done as part of
  # validateTableAgainstSchema())
  types <- sapply(limitData, typeof)
  names(types) <- NULL

  if (!isTRUE(all.equal(types, .changeRateLimitsColumnTypes))) {
    return(NULL)
  }

  .clearRow <- function(rowNum) {
    limitData[rowNum, "Min"] <<- NA_real_
    limitData[rowNum, "Max"] <<- NA_real_
  }

  .isValidRow <- function(min, max) {
    if (is.na(min) | is.na(max)) {
      return(FALSE)
    }

    # Negative values aren't allowed
    if ((min < 0.0) | (max < 0.0)) {
      return(FALSE)
    }

    # The maximum limit must be greater than or equal to the minimum
    if (min > max) {
      return(FALSE)
    }

    return(TRUE)
  }

  for (i in 1:NROW(limitData)) {
    if (!.isValidRow(limitData[i, "Min"], limitData[i, "Max"])) {
      .clearRow(i)
    }
  }

  return(limitData)
}

#' Initialize Stochastic Parameters
#'
#' Read the stochasticity information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @param stochasticParametersSheetName Name of Excel sheet holding stochastic
#'   parameters (mean, variance, etc) for different computations
#' @param changeRateLimitsSheetName Name of Excel sheet holding max/min ranges
#'   for generated mortality, fertility, and incidence rate
#'
#' @md
#' @export
#'
#' @return NULL (invisible)
#'
#' @examples
#' \dontrun{
#' library(pacehrh)
#'
#' pacehrh::InitializePopulation()
#' pacehrh::InitializeScenarios()
#' pacehrh::InitializeStochasticParameters()
#' pacehrh::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#' }
#'
InitializeStochasticParameters <- function(stochasticParametersSheetName = .defaultStochasticParmsSheet,
                                           changeRateLimitsSheetName = .defaultChangeRateLimitsSheet) {
  .checkAndLoadGlobalConfig()

  BVE$stochasticParams <- loadStochasticParameters(stochasticParametersSheetName)
  BVE$changeRateLimits <- loadChangeRateLimits(changeRateLimitsSheetName)
  return(invisible(NULL))
}
