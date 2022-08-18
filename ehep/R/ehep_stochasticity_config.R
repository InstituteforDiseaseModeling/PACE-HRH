#' Load Stochastic Parameters
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Dataframe of stochastic parameters
#'
loadStochasticParameters <- function(sheetName = "StochasticParameters"){
  stochData <- readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)

  # Keep the first three columns
  stochData <- stochData[1:3]
  return(stochData)
}

#' Initialize Stochastic Parameters
#'
#' Read the stochasticity information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @md
#' @param ... Parameters passed through to [loadStochasticParameters()]
#'
#' @export
#'
#' @return NULL (invisible)
#'
InitializeStochasticParameters <- function(...){
  .checkAndLoadGlobalConfig()

  stochData <- loadStochasticParameters(...)

  # TODO: Insert error handling

  GPE$stochasticParams <- stochData
  invisible(NULL)
}
