# Completion codes

.Success <- 0L
.errInputFileNotFound <- 1L
.errInputFileNotReadable <- 2L
.errScenarioSheetNotFound <- 4L
.errScenarioSheetNotReadable <- 8L
.errSeasonalityOffsetSheetNotFound <- 16L
.errSeasonalityOffsetSheetNotReadable <- 32L

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
                                      scenarioSheet = "Scenarios",
                                      seasonalityOffsetsSheet = "SeasonalityOffsets") {
  errcode <- .Success

  if (is.null(inputFile)) {
    inputFile <- GPE$inputExcelFile
  }

  if (is.null(outputFile)){
    errcode <- .checkInputExcelFileFormat(inputFile, scenarioSheet, seasonalityOffsetsSheet)
  } else {
    withr::with_output_sink(outputFile, code = {
      errcode <- .checkInputExcelFileFormat(inputFile, scenarioSheet, seasonalityOffsetsSheet)
    })
  }

  return(errcode)
}

.checkInputExcelFileFormat <- function(inputFile, scenarioSheet, seasonalityOffsetsSheet){
  .catLine()
  .catLine(date())
  .catLine("Input file = ", inputFile)

  # Create a place for the criticalChecks() functions to write some return
  # values back to the calling code.
  e <- new.env(parent = environment())

  # The first set of checks are critical: if these checks fail something is
  # so wrong there's no way to continue.
  errcode <- .criticalChecks(inputFile, scenarioSheet, seasonalityOffsetsSheet, e)

  if (errcode != .Success){
    return(errcode)
  }

  # The rest of the checks expose serious, but not necessarily fatal, problems

  # Recover returned values from the by-ref environment
  scenarios <- e$scenarios
  sheets <- e$sheets

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

  # ------------------------------------------

  seasonalityOffsets <- e$seasonalityOffsets

  # Check whether any tasks are duplicated in the seasonality offsets table
  tasks <- seasonalityOffsets$Task
  dups <- duplicated(tasks)

  if (!any(dups)){
    .catLine("No duplicate tasks in seasonality offsets table ... OK")
  } else {
    .catLine("The following tasks are duplicated in the seasonality offsets table ...")
    .catLine("* ", tasks[dups])
  }





  # Generate a list of unique, valid Task Value sheet names
  taskValuesSheets <- unique(scenarios$sheet_TaskValues)
  taskValuesSheets <- taskValuesSheets[taskValuesSheets %in% sheets]

  tasksInSeasonalityOffsetsSheet <- unique(tasks)






  l <- lapply(taskValuesSheets, function(sheet){
    tvs <- tryCatch({
      readxl::read_xlsx(inputFile, sheet = sheet)
    },
    warning = function(c){return(NULL)},
    error = function(c){return(NULL)})

    if (is.null(tvs)){
      return(character(0))
    } else {
      tasks <- tvs$Indicator
      used <- tasksInSeasonalityOffsetsSheet %in% tasks
      return(tasksInSeasonalityOffsetsSheet[!used])
    }
  })

  .catLine("The following tasks in the scenario offsets table are not used in task values sheets ...")

  for (i in seq_along(taskValuesSheets)){
    sheet <- taskValuesSheets[i]
    unusedSeasonalityTasks <- l[[i]]

    if (length(unusedSeasonalityTasks) == 0){
      .catLine("- ", sheet, " : ", "OK")
    } else {
      .catLine("* ", sheet, " : ", unusedSeasonalityTasks)
    }
  }

  # tasksInTvSheets <- unlist(l)
  #
  # used <- tasksInSeasonalityOffsetsSheet %in% tasksInTvSheets
  #
  # if (all(used)){
  #   .catLine("All the tasks in the scenario offsets table are used in task values sheets ... OK")
  # } else {
  #   .catLine("The following tasks in the scenario offsets table are not used in task values sheets ...")
  #   .catLine("* ", tasksInSeasonalityOffsetsSheet[used == FALSE])
  # }

  return(.Success)
}

.criticalChecks <- function(inputFile, scenarioSheet, seasonalityOffsetsSheet, outEnv){
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

  # Check whether the seasonality offsets sheet is in the sheet list
  if (!(seasonalityOffsetsSheet %in% sheets)){
    .catLine(seasonalityOffsetsSheet, " sheet could not be found")
    return(.errSeasonalityOffsetSheetNotFound)
  }

  # Read the contents of the Scenarios sheet
  scenarios <- tryCatch({
    readxl::read_xlsx(inputFile, sheet = scenarioSheet)
  },
  warning = function(c){return(NULL)},
  error = function(c){return(NULL)})

  if (is.null(scenarios)){
    .catLine("Could not read scenario sheet (", scenarioSheet, ")")
    return(.errScenarioSheetNotReadable)
  }

  # Read the contents of the SeasonalityOffsets sheet
  seasonalityOffsets <- tryCatch({
    readxl::read_xlsx(inputFile, sheet = seasonalityOffsetsSheet)
  },
  warning = function(c){return(NULL)},
  error = function(c){return(NULL)})

  if (is.null(seasonalityOffsets)){
    .catLine("Could not read seasonality offsets sheet (", seasonalityOffsetsSheet, ")")
    return(.errSeasonalityOffsetSheetNotReadable)
  }

  outEnv$sheets <- sheets
  outEnv$scenarios <- scenarios
  outEnv$seasonalityOffsets <- seasonalityOffsets

  return(.Success)
}

.catLine <- function(...){
  cat(paste0(..., "\n", collapse = ""))
}
