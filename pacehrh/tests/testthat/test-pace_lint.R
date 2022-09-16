library(pacehrh)

withr::local_dir("..")

# Basic operation: use default config file and scenario tab, redirect output to a file
test_that("Lint: basic", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  outFile = "lintout_01_clean.txt"
  errCode <- pacehrh::CheckInputExcelFileFormat(outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
})

# Basic operation: use default config file and scenario tab, redirect output to a file
test_that("Lint: basic", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./bad_config/Test Inputs - malformed.xlsx"

  outFile = "lintout_01_dirty.txt"
  errCode <- pacehrh::CheckInputExcelFileFormat(outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
})

# Bad name for the scenarios sheet
test_that("Lint: bad scenario steet name", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  outFile = "lintout_02.txt"
  errCode <- pacehrh::CheckInputExcelFileFormat(outputFile = outFile, scenarioSheet = "notasheet")
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, pacehrh:::.errScenarioSheetNotFound)
})

# Bad name for the seasonality offsets sheet
test_that("Lint: bad seasonality offsets sheet name", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  outFile = "lintout_05.txt"
  errCode <- pacehrh::CheckInputExcelFileFormat(outputFile = outFile, seasonalityOffsetsSheet = "notasheet")
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, pacehrh:::.errSeasonalityOffsetSheetNotFound)
})

# Bad name for the input file
test_that("Lint: non-existent input file", {
  outFile = "lintout_03.txt"
  errCode <- pacehrh::CheckInputExcelFileFormat(inputFile = "notafile", outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, pacehrh:::.errInputFileNotFound)
})

# Valid input file name, but not an Excel file
test_that("Lint: non-existent input file", {
  outFile = "lintout_04.txt"
  errCode <- pacehrh::CheckInputExcelFileFormat(inputFile = "globalconfig.json", outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, pacehrh:::.errInputFileNotReadable)
})

# No output redirection ... output goes to the terminal
test_that("Lint: stdout", {
  errCode <- pacehrh::CheckInputExcelFileFormat(inputFile = "notafile")
  testthat::expect_equal(errCode, pacehrh:::.errInputFileNotFound)
})
