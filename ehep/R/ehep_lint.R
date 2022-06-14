.Success <- 0L
.errInputFileNotFound <- 1L
.errInputFileNotReadable <- 2L
.errScenarioSheetNotFound <- 4L
.errScenarioSheetNotReadable <- 8L



#' Perform Sanity Checks On Input Excel File
#'
#' @description
#' Do a set of sanity checks on the input Excel file.
#'
#' @param inputFile Excel file to examine. If NULL, check the file defined in the global Configuration.
#' @param outputFile Results output file. If NULL, results are printed to the console.
#' @param scenariosSheet Name of the tab/sheet with scenario details. Default = "Scenarios".
#'
#' @return Error code. 0 = Success, non-zero = Failure
#'
#' @importFrom withr with_output_sink
#'
#' @export
CheckInputExcelFileFormat <- function(inputFile = NULL,
                                      outputFile = NULL,
                                      scenarioSheet = "Scenarios") {
  errcode <- .Success

  if (is.null(inputFile)) {
    inputFile <- GPE$inputExcelFile
  }

  if (is.null(outputFile)){
    errcode <- .checkInputExcelFileFormat(inputFile, scenarioSheet)
  } else {
    withr::with_output_sink(outputFile, code = {
      errcode <- .checkInputExcelFileFormat(inputFile, scenarioSheet)
    })
  }

  return(errcode)
}

.checkInputExcelFileFormat <- function(inputFile, scenarioSheet){
  .catLine()
  .catLine(date())
  .catLine("Input file = ", inputFile)

  e <- new.env(parent = environment())

  # The first set of checks are critical: if these checks fail something is
  # so wrong there's no way to continue.
  errcode <- .criticalChecks(inputFile, scenarioSheet, e)

  if (errcode != .Success){
    return(errcode)
  }

  # The rest of the checks expose serious, but not necessarily fatal, problems

  scenarios <- e$scenarios
  sheets <- e$sheets

  print(scenarios)
  print(sheets)

  # Collect references to other workbook sheets, and check that all the
  # references are to existing sheets
  sheetRefs <- c(scenarios$sheet_PopValues,
                 scenarios$sheet_TaskValues,
                 scenarios$sheet_SeasonalityCurves)

  sheetRefs <- unique(sheetRefs)

  results <- (sheetRefs %in% sheets)

  if (all(results)){
    .catLine("Sheet references from scenarios ... OK")
  } else {
    .catLine("The following sheets referenced by scenarios do not exist ...")
    .catLine("* ", sheetRefs[results == FALSE])
  }



  return(.Success)
}

.criticalChecks <- function(inputFile, scenarioSheet, outEnv){
  outEnv$sheets <- NULL
  outEnv$scenarios <- NULL

  # Check whether the input file exists
  if (!file.exists(inputFile)){
    .catLine(inputFile, " not found")
    return(.errInputFileNotFound)
  }

  # Extract sheet (tab) list, or raise an error if the input file can't be read
  sheets <- tryCatch({
    readxl::excel_sheets(inputFile)
  },
  error = function(c){
    .catLine(inputFile, " could not be read")
    .catLine("Error message: ", c)
    return(NULL)
  })

  if (is.null(sheets)) {
    return(.errInputFileNotReadable)
  }

  # Check whether the scenario sheet is in the sheet list
  if (!(scenarioSheet %in% sheets)){
    .catLine(scenarioSheet, " sheet could not be found")
    return(.errScenarioSheetNotFound)
  }

  # Read the contents of the scenarios sheet
  scenarios <- tryCatch({
    readxl::read_xlsx(inputFile, sheet = scenarioSheet)
  },
  warning = function(c){return(NULL)},
  error = function(c){return(NULL)})

  if (is.null(scenarios)){
    if (!(scenarioSheet %in% tabs)){
      .catLine("Could not read scenario sheet (", scenarioSheet, ")")
      return(.errScenarioSheetNotReadable)
    }
  }

  outEnv$sheets <- sheets
  outEnv$scenarios <- scenarios

  return(.Success)
}

.catLine <- function(...){
  cat(paste0(..., "\n", collapse = ""))
}
