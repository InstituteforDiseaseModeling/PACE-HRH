library(ehep)

withr::local_dir("..")

# Basic operation: use default config file and scenario tab, redirect output to a file
test_that("Lint: basic", {
  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  outFile = "lintout_01.txt"
  errCode <- ehep::CheckInputExcelFileFormat(outputFile = outFile)
  testthat::expect_true(file.exists(outFile))
})

# Bad name for the scenario tab
test_that("Lint: bad scenario tab name", {
  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  outFile = "lintout_02.txt"
  errCode <- ehep::CheckInputExcelFileFormat(outputFile = outFile, scenarioSheet = "notasheet")
  testthat::expect_true(file.exists(outFile))
  testthat::expect_equal(errCode, ehep:::.errScenarioSheetNotFound)
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
