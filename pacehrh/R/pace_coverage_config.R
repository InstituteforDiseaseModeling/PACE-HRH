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
loadCoverageRates <-
  function(sheetName = .defaultCoverageRatesSheet) {
    traceMessage(paste0("Loading task coverage rates sheet ", sheetName))

    coverageRatesData <- loadTable(sheet = sheetName)

    if (is.null(coverageRatesData)) {
      return(NULL)
    }

    hr <- colnames(coverageRatesData)

    # Sanity check: first two columns should be Indicator and CommonName
    if (!identical(hr[1:2], c("Indicator", "CommonName"))) {
      traceMessage(paste0(
        "Incorrect coverage rate config header names: ",
        paste0(hr[1:2], collapse = "; ")
      ))

      return(NULL)
    }

    headerYears <- as.numeric(gsub("Year ", "", hr[-(1:2)])) + BVE$years[1]

    # Sanity check: year offsets should be contiguous
    minYear <- min(headerYears)
    maxYear <- max(headerYears)
    if (!identical(headerYears, seq(minYear, maxYear, 1))) {
      traceMessage("Missing years in coverage rate headers")
      return(NULL)
    }

    # Update columns names with year values
    colnames(coverageRatesData) <- c(hr[1:2], as.character(headerYears))

    # If there are any missing years, add new columns
    missingYears <- setdiff(BVE$years, headerYears)
    if (length(missingYears) > 0) {
      coverageRatesData[sprintf("%d", missingYears)] <- NA

      traceMessage(
        paste0("Mis-match between simulation years and coverage levels. The following years were added at 100% coverage: ",
               paste0(missingYears, collapse = ", "))
      )
    }

    # Add any remaining tasks for this scenario
    coverageRatesData <-
      merge(
        BVE$taskData[, c("Indicator", "CommonName")],
        coverageRatesData,
        by = c("Indicator", "CommonName"),
        all.x = TRUE
      )

    # All missing values are assumed to be 1.0
    coverageRatesData[is.na(coverageRatesData)] <- 1.0

    # Make the data tidy
    tidyCoverageRatesData <-
      pivot_longer(coverageRatesData,
                   !(c("Indicator", "CommonName")),
                   names_to = "Year",
                   values_to = "Coverage")

    # Convert year to numeric from char
    tidyCoverageRatesData$Year <-
      as.numeric(tidyCoverageRatesData$Year)

    return(tidyCoverageRatesData)
  }

.InitializeCoverageRates <- function(...) {
  .checkAndLoadGlobalConfig()

  coverageRatesData <- loadCoverageRates(...)

  BVE$taskCoverageRates <- coverageRatesData
  return(invisible(NULL))
}
