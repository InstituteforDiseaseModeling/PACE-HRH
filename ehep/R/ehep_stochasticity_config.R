#' Load Stochastic Parameters
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return List with three \code{PopulationPyramid} objects:
#' \code{female}, \code{male} and \code{total}
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
#' @param sheetName Sheet name from the model input Excel file
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
