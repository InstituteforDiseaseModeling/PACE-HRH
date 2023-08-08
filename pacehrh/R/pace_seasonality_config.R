#' Load Seasonality Curves
#'
#' Read the seasonality curves from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetNameCurves Sheet name from the model input Excel file
#' @param ... Throw away other parameters
#'
#' @return Seasonality curves data frame.
#'
#' @noRd
loadSeasonalityCurves <- function(sheetNameCurves = .defaultSeasonalityCurvesSheet, ...) {
  traceMessage(paste0("Loading seasonality curves sheet ", sheetNameCurves))

  seasonalityCurvesData <- NULL

  seasonalityCurvesData <- readSheet(sheetName = sheetNameCurves)

  # TODO: Insert schema validation once the orientation of the curves sheet
  # has been flipped.

  return(seasonalityCurvesData)
}

#' Load Seasonality Offsets
#'
#' Read the seasonality offsets from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetNameOffsets Sheet name from the model input Excel file
#' @param ... Throw away other parameters
#'
#' @return Seasonality offsets data frame.
#'
#' @noRd
loadSeasonalityOffsets <- function(sheetNameOffsets = .defaultSeasonalityOffsetsSheet, ...) {
  traceMessage(paste0("Loading seasonality offsets sheet ", sheetNameOffsets))

  seasonalityOffsetsData <- NULL

  seasonalityOffsetsData <- readSheet(sheetName = sheetNameOffsets)

  if (!is.null(seasonalityOffsetsData)) {
    seasonalityOffsetsData <-
      validateTableAgainstSchema(seasonalityOffsetsData,
        .seasonalityOffsetMetaData,
        convertType = TRUE
      )
  }

  return(seasonalityOffsetsData)
}

#' Initialize Seasonality Information
#'
#' Read the healthcare seasonality information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @md
#' @param ... Parameters passed through to loadSeasonalityCurves() and
#' loadSeasonalityOffsets()
#'
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
#'   pacehrh::RunExperiments(
#'     scenarioName = scenario,
#'     trials = 100
#'   )
#' }
InitializeSeasonality <- function(...) {
  .checkAndLoadGlobalConfig()

  seasonalityCurvesData <- loadSeasonalityCurves(...)
  seasonalityOffsetsData <- loadSeasonalityOffsets(...)

  # TODO: Insert error handling

  BVE$seasonalityCurves <- seasonalityCurvesData
  BVE$seasonalityOffsets <- seasonalityOffsetsData
  return(invisible(NULL))
}
