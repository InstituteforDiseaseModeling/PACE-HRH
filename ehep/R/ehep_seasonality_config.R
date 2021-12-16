#' Load Seasonality Curves
#'
#' Read the seasonality curves from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Seasonality curves data frame.
#'
loadSeasonalityCurves <- function(sheetName = "SeasonalityCurves"){
  seasonalityData <- NULL

  if (file.exists(globalPackageEnvironment$inputExcelFile)){
    out <- tryCatch(
      {
        seasonalityData <- readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetName)
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

  return(seasonalityData)
}
