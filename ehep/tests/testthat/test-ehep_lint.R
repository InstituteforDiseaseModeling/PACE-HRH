library(ehep)

withr::local_dir("..")

# Basic operation: use default config file and scenario tab, redirect output to a file
test_that("Lint: basic", {
  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  outFile = "lintout_01_clean.txt"
  errCode <- ehep::CheckInputExcelFileFormat(outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
})

# Basic operation: use default config file and scenario tab, redirect output to a file
test_that("Lint: basic", {
  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./bad_config/Test Inputs - malformed.xlsx"

  outFile = "lintout_01_dirty.txt"
  errCode <- ehep::CheckInputExcelFileFormat(outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
})

# Bad name for the scenarios sheet
test_that("Lint: bad scenario steet name", {
  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  outFile = "lintout_02.txt"
  errCode <- ehep::CheckInputExcelFileFormat(outputFile = outFile, scenarioSheet = "notasheet")
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, ehep:::.errScenarioSheetNotFound)
})

# Bad name for the seasonality offsets sheet
test_that("Lint: bad seasonality offsets sheet name", {
  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  outFile = "lintout_05.txt"
  errCode <- ehep::CheckInputExcelFileFormat(outputFile = outFile, seasonalityOffsetsSheet = "notasheet")
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, ehep:::.errSeasonalityOffsetSheetNotFound)
})

# Bad name for the input file
test_that("Lint: non-existent input file", {
  outFile = "lintout_03.txt"
  errCode <- ehep::CheckInputExcelFileFormat(inputFile = "notafile", outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, ehep:::.errInputFileNotFound)
})

# Valid input file name, but not an Excel file
test_that("Lint: non-existent input file", {
  outFile = "lintout_04.txt"
  errCode <- ehep::CheckInputExcelFileFormat(inputFile = "globalconfig.json", outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, ehep:::.errInputFileNotReadable)
})

# No output redirection ... output goes to the terminal
test_that("Lint: stdout", {
  errCode <- ehep::CheckInputExcelFileFormat(inputFile = "notafile")
  testthat::expect_equal(errCode, ehep:::.errInputFileNotFound)
})

# test validate no rules
test_that("Lint: Validation no rules", {
  # test should fail if no rule is defined for the sheet
  testthat::expect_error(ehep::ValidateInputExcelFileContent(inputFile = "./bad_config/Test_validation.xlsx", sheetNames = c("Total_Pop2")), "rule not found")

})
# test validation capture
test_that("Lint: Validation capture", {
  errCode <- ehep:::.errValidationRuleFailed
  # In this example rules Total_diarrhea does not sum to 1, this is a table-wise error not row-wise error
  # The error is captured and verified in _snaps/ehep_lint.md

  logdir <- tempdir()
  testthat::expect_snapshot(ehep::ValidateInputExcelFileContent(inputFile = "./bad_config/Test_validation.xlsx",
                                                                          outputDir = logdir,
                                                                          sheetNames = c("SeasonalityCurves")),
                            error = TRUE)

})
# test validation success
test_that("Lint: Validation capture Success", {
  errCode <- ehep:::.Success

  logdir <- tempdir()
  testthat::expect_equal(errCode, ehep::ValidateInputExcelFileContent(inputFile = "./bad_config/Test_validation2.xlsx",
                                                                outputDir = logdir,
                                                                sheetNames = c("SeasonalityCurves")))

})

# test validation row-wise
test_that("Lint: Validation capture Success", {
  errCode <- ehep:::.Success
  # In this example, rule is set as warning level so violation is not causing error but output files will be available
  logdir <- tempdir()
  testthat::expect_equal(errCode, ehep::ValidateInputExcelFileContent(inputFile = "./bad_config/Test_validation.xlsx",
                                                                      outputDir = logdir,
                                                                      sheetNames = c("TaskValues_ref")))
  result_file <- paste(logdir, "violation_warning_Missing_parameter_value.csv", sep="/")
  testthat::expect_true(file.exists(result_file))
  violations <- read.csv(result_file)
  # search for the row that fails Indicator = FH.MN.ANC.1XX
  violating_row <- violations %>%
    dplyr::filter(Indicator == "FH.MN.ANC.1XX")
  testthat::expect_equal(nrow(violating_row ), 1)
})

