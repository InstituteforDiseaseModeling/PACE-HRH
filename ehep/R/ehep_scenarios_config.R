#' Load Experiment Scenario Information
#'
#' Read the experiment scenario information from the model inputs Excel file.
#' The name and location of the model inputs Excel file is loaded from the
#' global configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Data frame of experiment scenario parameters
#'
loadScenarios <- function(sheetName = "Scenarios") {
  scenarios <- NULL

  if (file.exists(GPE$inputExcelFile)){
    out <- tryCatch(
      {
        scenarios <-
          readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)
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
                             GPE$inputExcelFile,
                             sep = ""))
  }

  if (!is.null(scenarios)){
    # We only care about some of the sheet
    scenarios <- scenarios[.scenarioColumnNames]

    # Asserts test that all the expected columns have shown up
    assertthat::are_equal(length(.scenarioColumnNames), length(.scenarioColumnTypes))
    assertthat::are_equal(names(scenarios), .scenarioColumnNames)
    assertthat::assert_that(all(sapply(scenarios,typeof) == .scenarioColumnTypes))
  }

  return(scenarios)
}

#' Initialize Experiment Scenario Information
#'
#' Read experiment scenario information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @param loadFromExcel If TRUE, initialize the scenarios list from the model
#' inputs Excel file. If FALSE, initialize with a blank scenarios table.
#' @param ... See \code{loadScenarios()}
#'
#' @export
#'
#' @return NULL (invisible)
#'
InitializeScenarios <- function(loadFromExcel = TRUE, ...){
  .checkAndLoadGlobalConfig()

  if (loadFromExcel){
    scenarios <- loadScenarios(...)
  } else {
    scenarios <- CreateScenariosTable()
  }

  # TODO: Insert error handling

  GPE$scenarios <- scenarios
  invisible(NULL)
}
