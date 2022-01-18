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

  if (file.exists(globalPackageEnvironment$inputExcelFile)){
    out <- tryCatch(
      {
        seasonalityCurvesData <-
          readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetNameCurves)
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
                             globalPackageEnvironment$inputExcelFile,
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

  if (file.exists(globalPackageEnvironment$inputExcelFile)){
    out <- tryCatch(
      {
        seasonalityOffsetsData <-
          readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetNameOffsets)
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
                             globalPackageEnvironment$inputExcelFile,
                             sep = ""))
  }

  return(seasonalityOffsetsData)
}

#' Initialize Seasonality Information
#'
#' Read the healthcare seasonality information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @export
#'
#' @return NULL (invisible)
#'
InitializeSeasonality <- function(...){
  .checkAndLoadGlobalConfig()

  seasonalityCurvesData <- loadSeasonalityCurves(...)
  seasonalityOffsetsData <- loadSeasonalityOffsets(...)

  # TODO: Insert error handling

  g <- globalPackageEnvironment

  g$seasonalityCurves <- seasonalityCurvesData
  g$seasonalityOffsets <- seasonalityOffsetsData
  invisible(NULL)
}
