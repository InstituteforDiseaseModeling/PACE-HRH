#' Load Seasonality Curves
#'
#' Read the seasonality curves from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetNameCurves Sheet name from the model input Excel file
#'
#' @return Seasonality curves data frame.
#'
loadSeasonalityCurves <- function(sheetNameCurves = "SeasonalityCurves"){
  seasonalityCurvesData <- NULL

  if (file.exists(GPE$inputExcelFile)){
    out <- tryCatch(
      {
        seasonalityCurvesData <-
          readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetNameCurves)
      },
      warning = function(war)
      {
        traceMessage(paste("WARNING:", war))
      },
      error = function(err)
      {
        traceMessage(paste("ERROR:", err))
      },
      finally =
        {

        }
    )
  } else {
    traceMessage(paste("Could not find model input file ",
                             GPE$inputExcelFile,
                             sep = ""))
  }

  return(seasonalityCurvesData)
}



#' Load Seasonality Offsets
#'
#' Read the seasonality offsets from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetNameOffsets Sheet name from the model input Excel file
#'
#' @return Seasonality offsets data frame.
#'
loadSeasonalityOffsets <- function(sheetNameOffsets = "SeasonalityOffsets"){
  seasonalityOffsetsData <- NULL

  if (file.exists(GPE$inputExcelFile)){
    out <- tryCatch(
      {
        seasonalityOffsetsData <-
          readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetNameOffsets)
      },
      warning = function(war)
      {
        traceMessage(paste("WARNING:", war))
      },
      error = function(err)
      {
        traceMessage(paste("ERROR:", err))
      },
      finally =
        {

        }
    )
  } else {
    traceMessage(paste("Could not find model input file ",
                             GPE$inputExcelFile,
                             sep = ""))
  }
  
  seasonalityOffsetsData <-
    validateTableAgainstSchema(seasonalityOffsetsData,
                               .seasonalityOffsetMetaData,
                               convertType = TRUE)
  

  return(seasonalityOffsetsData)
}

#' Initialize Seasonality Information
#'
#' Read the healthcare seasonality information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @md
#' @param ... Parameters passed through to [loadSeasonalityCurves()] and
#' [loadSeasonalityOffsets()]
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
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#' }
InitializeSeasonality <- function(...){
  .checkAndLoadGlobalConfig()

  seasonalityCurvesData <- loadSeasonalityCurves(...)
  seasonalityOffsetsData <- loadSeasonalityOffsets(...)

  # TODO: Insert error handling

  BVE$seasonalityCurves <- seasonalityCurvesData
  BVE$seasonalityOffsets <- seasonalityOffsetsData
  return(invisible(NULL))
}
