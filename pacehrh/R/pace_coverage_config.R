#' Load CoverageRates Values
#'
#' Read the annual task coverage values from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Tibble with three population pyramid fields:
#' \code{Female}, \code{Male} and \code{Total}
#'
loadCoverageRates <- function(sheetName = .defaultCoverageRatesSheet){
  traceMessage(paste0("Loading task coverage rates sheet ", sheetName))
  
  coverageRatesData <- NULL
  
  coverageRatesData <- loadTable(sheet = sheetName) # loadTable does validation too
  browser()
  
  hr <- colnames(coverageRatesData)
  
  # First two columns should be indicator and common name:
  if (!identical(hr[1:2], c("Indicator", "CommonName"))) {
    traceMessage(
      paste0(
        "Incorrect coverage rate config header names: (",
        as.character(hr),
        ")"
      )
    )
    return(NULL)
  }
  
  # Strip out "Year" from the header names
  #years <- 
  colnames(coverageRatesData) <- gsub("Year ", "", hr)
  
  # All missing values are assumed to be 1.0
  coverageRatesData[is.na(coverageRatesData)] <- 1.0
  
  # Make the data tidy
  test <- 
    pivot_longer(coverageRatesData, !(c("Indicator", "CommonName")), 
                 names_to="years",
                 values_to="coverage")
  
  return(coverageRatesData)
}

.InitializeCoverageRates <- function(...){
  .checkAndLoadGlobalConfig()
  
  coverageRatesData <- loadCoverageRates(...)
  
  BVE$taskCoverageRates <- coverageRatesData
  return(invisible(NULL))
}