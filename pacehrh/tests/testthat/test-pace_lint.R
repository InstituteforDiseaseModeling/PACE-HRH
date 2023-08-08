library(pacehrh)

withr::local_dir("..")

# Basic operation: use default config file and scenario tab, redirect output to a file
test_that("Lint: basic", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./config/model_inputs.xlsx"
  outFile = "lintout_01_clean.txt"

  # Check that linting produces an output file
  errCode <- pacehrh::CheckInputExcelFileFormat(outputFile = outFile)
  testthat::expect_true(file.exists(outFile))

  if (file.exists(outFile)){
    file.remove(outFile)
  }

  # Check that linting output is stable
  testthat::expect_snapshot({
    errCode <- pacehrh::CheckInputExcelFileFormat(noDate = TRUE)
  })
})

# Basic operation: use default config file and scenario tab, redirect output to a file
test_that("Lint: basic", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./bad_config/model_inputs-malformed.xlsx"
  testthat::expect_snapshot({
    errCode <- pacehrh::CheckInputExcelFileFormat(noDate = TRUE)
  })
})

# Bad name for the scenarios sheet
test_that("Lint: bad scenario sheet name", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./config/model_inputs.xlsx"
  testthat::expect_snapshot({
    errCode <- pacehrh::CheckInputExcelFileFormat(scenarioSheet = "notasheet", noDate = TRUE)
  })
  testthat::expect_equal(errCode, pacehrh:::.errScenarioSheetNotFound)
})

# Bad name for the seasonality offsets sheet
test_that("Lint: bad seasonality offsets sheet name", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  e$inputExcelFile <- "./config/model_inputs.xlsx"
  testthat::expect_snapshot({
    errCode <- pacehrh::CheckInputExcelFileFormat(seasonalityOffsetsSheet = "notasheet", noDate = TRUE)
  })
  testthat::expect_equal(errCode, pacehrh:::.errSeasonalityOffsetSheetNotFound)
})

# Bad name for the input file
test_that("Lint: non-existent input file", {
  testthat::expect_snapshot({
    errCode <- pacehrh::CheckInputExcelFileFormat(inputFile = "notafile", noDate = TRUE)
  })
  testthat::expect_equal(errCode, pacehrh:::.errInputFileNotFound)
})

# Valid input file name, but not an Excel file
test_that("Lint: non-existent input file", {
  testthat::expect_snapshot({
    errCode <- pacehrh::CheckInputExcelFileFormat(inputFile = "globalconfig.json", noDate = TRUE)
  })
  testthat::expect_equal(errCode, pacehrh:::.errInputFileNotReadable)
})

# No output redirection ... output goes to the terminal
test_that("Lint: stdout", {
  errCode <- pacehrh::CheckInputExcelFileFormat(inputFile = "notafile")
  testthat::expect_equal(errCode, pacehrh:::.errInputFileNotFound)
})
