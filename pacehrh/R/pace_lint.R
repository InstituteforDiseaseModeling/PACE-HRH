# Completion codes

.Success <- 0L
.errInputFileNotFound <- -1L
.errInputFileNotReadable <- -2L
.errScenarioSheetNotFound <- -3L
.errScenarioSheetNotReadable <- -4L
.errSeasonalityOffsetSheetNotFound <- -5L
.errSeasonalityOffsetSheetNotReadable <- -6L
.warnProblemsFound <- 1L

#' Perform Sanity Checks On Input Excel File
#'
#' @description
#' Do a set of sanity checks on the input Excel file.
#'
#' @param inputFile Excel file to examine. If NULL, check the file defined in the global Configuration.
#' @param outputFile Results output file. If NULL, results are printed to the console.
#' @param scenarioSheet Name of the sheet with scenario details. Default = "Scenarios".
#' @param seasonalityOffsetsSheet Name of the sheet with per-task seasonality offset details. Default = "SeasonalityOffsets"
#'
#' @return Error code.
#' 0 = Success
#' < 0 (negative) = Fatal error. Nothing will work right.
#' > 0 (positive) = Warning. Some scenarios might fail, some might succeed.
#'
#' @importFrom withr with_output_sink
#'
#' @export
#'
#' @examples
#' \dontrun{
#' errCode <- ehep::CheckInputExcelFileFormat(outputFile = "config_file_check.txt")
#' }
CheckInputExcelFileFormat <- function(inputFile = NULL,
                                      outputFile = NULL,
                                      scenarioSheet = "Scenarios",
                                      seasonalityOffsetsSheet = "SeasonalityOffsets") {

  # This is a wrapper function that controls whether report output is directed
  # to the console or a file. The real action is in the called function:
  # .checkInputExcelFileFormat()

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

  # Generate a list of unique, valid Task Value sheet names
  e$taskValuesSheets <- unique(e$scenarios$sheet_TaskValues)
  e$taskValuesSheets <- e$taskValuesSheets[e$taskValuesSheets %in% e$sheets]

  # Read all the task value sheets so we aren't repeatedly reading them later
  e$taskValuesSheetData <- .readTaskValuesSheetData(inputFile, e$taskValuesSheets)

  # Generate a list of unique, valid Seasonality Curve sheet names
  e$seasonalityCurveSheets <- unique(e$scenarios$sheet_SeasonalityCurves)
  e$seasonalityCurveSheets <- e$seasonalityCurveSheets[e$seasonalityCurveSheets %in% e$sheets]

  # Read all the Seasonality Curve sheets
  e$seasonalityCurvesSheetData <- .readTaskValuesSheetData(inputFile, e$seasonalityCurveSheets)

  # ------------------------------------------

  testFuncs <- c(
    .test1_sheetRefs,
    .test2_dupSeasonalityTasks,
    .test3_seasonalityTasksUsed,
    .test4_dupTaskIds,
    .test5_invalidNumerics,
    .test6_seasonalityNormalization
  )

  out <- sapply(testFuncs, function(f){do.call(f,list(e))})

  # Pop a line at the bottom of the report output indicating at a glance
  # which tests passed/failed. (1 = failed, 0 = passed.)
  #    > Test results: 1-1-1-0-1-0

  .catLine("Test results: ", paste(as.list(as.character(out)), collapse = "-"))

  if (all(out == .Success)){
    return(.Success)
  } else {
    return(out[which(out != .Success)[1]])
  }
}

# Critical checks are checks for conditions that prevent further progress.
# Some critical checks involve reading information that will be useful later.
# That information is passed back to the caller through a passed environment
# object.

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

.readTaskValuesSheetData <- function(inputFile, taskValuesSheets){
  l <- lapply(taskValuesSheets, function(sheet){
    sheetData <- tryCatch({
      readxl::read_xlsx(inputFile, sheet = sheet)
    },
    warning = function(c){return(NULL)},
    error = function(c){return(NULL)})

    return(sheetData)
  })

  return(l)
}

.catLine <- function(...){
  cat(paste0(..., "\n", collapse = ""))
}

.test1_sheetRefs <- function(e){
  # Collect references to other workbook sheets, and check that all the
  # references are to existing sheets
  errcode <- .Success

  sheetRefs <- c(e$scenarios$sheet_PopValues,
                 e$scenarios$sheet_TaskValues,
                 e$scenarios$sheet_SeasonalityCurves)

  sheetRefs <- unique(sheetRefs)

  results <- (sheetRefs %in% e$sheets)

  if (all(results)){
    .catLine("Sheet references from scenarios ... OK")
  } else {
    errcode <- .warnProblemsFound
    .catLine("The following sheets referenced by scenarios do not exist ...")
    .catLine("* ", sheetRefs[results == FALSE])
  }

  return(errcode)
}

.test2_dupSeasonalityTasks <- function(e){
  # Check for duplicated task IDs in the seasonality offsets table
  errcode <- .Success

  tasks <- e$seasonalityOffsets$Task
  dups <- duplicated(tasks)

  if (!any(dups)){
    .catLine("No duplicate tasks in seasonality offsets table ... OK")
  } else {
    errcode <- .warnProblemsFound
    .catLine("The following tasks are duplicated in the seasonality offsets table ...")
    .catLine("* ", tasks[dups])
  }

  return(errcode)
}

.test3_seasonalityTasksUsed <- function(e){
  # Check whether all the seasonality tasks are being used in each task values sheet
  # (This is to enforce a workaround for a yet-to-be-fixed bug.)
  errcode <- .Success

  tasksInSeasonalityOffsetsSheet <- unique(e$seasonalityOffsets$Task)

  # Gather the task IDs for seasonality tasks that aren't used in task value sheets
  l <- lapply(e$taskValuesSheetData, function(sheetData){
    if (is.null(sheetData)){
      return(character(0))
    } else {
      tasks <- sheetData$Indicator
      used <- tasksInSeasonalityOffsetsSheet %in% tasks
      return(tasksInSeasonalityOffsetsSheet[!used])
    }
  })

  # Output report info
  .catLine("The following tasks in the scenario offsets table are not used in task values sheets ...")

  for (i in seq_along(e$taskValuesSheets)){
    sheet <- e$taskValuesSheets[i]
    unusedSeasonalityTasks <- l[[i]]

    if (length(unusedSeasonalityTasks) == 0){
      .catLine("- ", sheet, " : ", "OK")
    } else {
      errcode <- .warnProblemsFound
      .catLine("* ", sheet, " : ", unusedSeasonalityTasks)
    }
  }

  return(errcode)
}

.test4_dupTaskIds <- function(e){
  # Check that there are no duplicated task-geography combinations in task
  # value sheets.
  errcode <- .Success

  l <- lapply(e$taskValuesSheetData, function(sheetData){
    if (is.null(sheetData)){
      return(character(0))
    } else {
      tasks <- sheetData$Indicator
      geos <- sheetData$Geography
      assertthat::assert_that(length(tasks) == length(geos))
      keys <- paste(tasks, "$$$", geos, sep = "")
      return(tasks[duplicated(keys)])
    }
  })

  .catLine("The following tasks are duplicated in task values sheets ...")

  for (i in seq_along(e$taskValuesSheets)){
    sheet <- e$taskValuesSheets[i]
    duplicateTasks <- l[[i]]

    if (length(duplicateTasks) == 0){
      .catLine("- ", sheet, " : ", "OK")
    } else {
      errcode <- .warnProblemsFound
      .catLine("* ", sheet, " : ", duplicateTasks)
    }
  }

  return(errcode)
}

.test5_invalidNumerics <- function(e){
  # Check for non-numeric characters in task values sheet columns that should
  # be numeric. (R's coercion rules require that the elements of a vector all
  # have the same type, so if R can't translate a vector element as a number,
  # it has to coerce the entire vector into a different, more flexible, type,
  # usually "character".)
  errcode <- .Success

  numericColumns <-
    c(
      "StartingRateInPop",
      "RateMultiplier",
      "AnnualDeltaRatio",
      "NumContactsPerUnit",
      "NumContactsAnnual",
      "MinsPerContact",
      "HoursPerWeek",
      "FTEratio"
    )

  l <- lapply(e$taskValuesSheetData, function(sheetData){
    if (is.null(sheetData)){
      return(character(0))
    } else {
      types <- sapply(numericColumns, function(colName){
        return(typeof(sheetData[[colName]]))
      })
      return(types)
    }
  })

  # If a sheet column is completely blank, readxl imports it as a vector
  # of NA values, which R then interprets as type logical. If there's even one
  # numeric value in the column, the column will be interpreted as type numeric.
  # The following code looks for non-numeric columns (type != "double") that
  # aren't completely blank.

  .catLine("The following numeric columns contain non-numeric values ...")

  for (i in seq_along(e$taskValuesSheets)){
    sheet <- e$taskValuesSheets[i]
    types <- l[[i]]

    sheetData <- e$taskValuesSheetData[[i]]
    blank <- sapply(numericColumns, function(colName){
      return(all(is.na(sheetData[[colName]])))
    })

    if (all(types == "double" | blank == TRUE)){
      .catLine("- ", sheet, " : ", "OK")
    } else {
      errcode <- .warnProblemsFound

      .catLine("* ", sheet, " : ", numericColumns[(types != "double" & !blank)])
    }
  }

  return(errcode)
}

.test6_seasonalityNormalization <- function(e){
  errcode <- .Success

  l <- lapply(e$seasonalityCurvesSheetData, function(sheetData){
    if (is.null(sheetData)){
      return(character(0))
    } else {
      sheetData <- sheetData[-1]
      sapply(sheetData, sum)
    }
  })

  .catLine("The following seasonality curves aren't normalized to 1 ...")

  for (i in seq_along(e$seasonalityCurveSheets)){
    sheet <- e$taskValuesSheets[i]
    normSums <- l[[i]]
    curveNames <- names(normSums)

    normSumDeviation <- abs(normSums - 1.0)

    if (all(normSumDeviation < 1e-9)){
      .catLine("- ", sheet, " : ", "OK")
    } else {
      errcode <- .warnProblemsFound
      .catLine("* ", sheet, " : ",
               curveNames[normSumDeviation >= 1e-9],
               " (", round(normSums[normSumDeviation >= 1e-9], 3), ")")
    }
  }

  return(errcode)
}
