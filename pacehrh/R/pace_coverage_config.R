#' Load CoverageRates Values
#'
#' Read the annual task coverage values from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Data table of per-year task coverage levels
#'
loadCoverageRates <- function(sheetName = .defaultCoverageRatesSheet){
  traceMessage(paste0("Loading task coverage rates sheet ", sheetName))
  
  coverageRatesData <- NULL
  
  coverageRatesData <- loadTable(sheet = sheetName) # loadTable does validation
  
  hr <- colnames(coverageRatesData)
  
  # Sanity check: first two columns should be indicator and common name:
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
  
  
  # total number of years in simulation
  simYears <- length(BVE$years) 
  
  # all cols in coverageRatesData minus the indicator and common name cols
  coverageYears <- length(coverageRatesData)-2 
  
  # Extend table to include all years if sim is longer than provided columns
  if (simYears > coverageYears) {
    coverageRatesData[sprintf("Year %d", seq(coverageYears, simYears-1))] <- NA
  }
  
  # get the new header values
  hr <- colnames(coverageRatesData)
  
  # Strip out "Year" from the header names and shift colnames to index from the 
  # start year
  cleanHeaders <- gsub("Year ", "", hr)
  cleanHeaders <- c(cleanHeaders[1:2], paste(as.integer(cleanHeaders[3:length(cleanHeaders)]) + BVE$years[1]))
  colnames(coverageRatesData) <- cleanHeaders
  
  # All missing values are assumed to be 1.0
  coverageRatesData[is.na(coverageRatesData)] <- 1.0
  
  # Make the data tidy
  tidyCoverageRatesData <-
    pivot_longer(coverageRatesData, !(c("Indicator", "CommonName")), 
                 names_to="years",
                 values_to="coverage")
  
  return(tidyCoverageRatesData)
}

.InitializeCoverageRates <- function(...){
  .checkAndLoadGlobalConfig()
  
  coverageRatesData <- loadCoverageRates(...)
  
  BVE$taskCoverageRates <- coverageRatesData
  return(invisible(NULL))
}