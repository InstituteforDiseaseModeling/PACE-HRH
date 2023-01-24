context("Test ValidateInputExcelFileContent")
local_edition(3)
setwd("../..")
source("ValidateInput.R")

# test validate no rules
test_that("Validation no rules", {
  # test should fail if no rule is defined for the sheet
  logdir <- tempdir()
  testthat::expect_error(ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation.xlsx",
                                                       outputDir = logdir,
                                                       sheetNames = c("Total_Pop2")), "Unable to check sheet")
  
})
# test validation capture
test_that("Validation capture", {
  errCode <- .errValidationRuleFailed
  # In this example rules Total_diarrhea does not sum to 1, this is a table-wise error not row-wise error
  # The error is captured and verified in _snaps/pacehrh_lint.md
  
  logdir <- tempdir()
  # skip_if(.Platform$OS.type != "windows")
  testthat::expect_equal(errCode, ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation.xlsx",
                                                                outputDir = logdir,
                                                                sheetNames = c("SeasonalityCurves")), error=TRUE)
  
})
# test validation success
test_that("Validation capture Success", {
  errCode <- .Success
  
  logdir <- tempdir()
  testthat::expect_equal(errCode, ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation2.xlsx",
                                                                      outputDir = logdir,
                                                                      sheetNames = c("SeasonalityCurves")))
  
})

# test validation row-wise
test_that("Validation capture Success", {
  errCode <- .Success
  # In this example, rule is set as warning level so violation is not causing error but output files will be available
  logdir <- tempdir()
  testthat::expect_equal(errCode, ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation.xlsx",
                                                                      outputDir = logdir,
                                                                      sheetNames = c("StochasticParameters")))
  result_file <- paste(logdir, "StochasticParameters_info_violation_Large_SD_value.csv", sep="/")
  testthat::expect_true(file.exists(result_file))
  violations <- read.csv(result_file)
  # search for the row that fails as expected
  violating_row <- violations %>%
    dplyr::filter(Value == "Seasonality ratio to mean")
  testthat::expect_equal(nrow(violating_row ), 1)
})

# test validation success
test_that("Validation Scenarios custom name", {
  errCode <- .Success
  logdir <- tempdir()
  testthat::expect_equal(errCode, ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation_custom.xlsx",
                                                                outputDir = logdir))
})

# test validation missing columns specified in the rule (sheet:SeasonalityCurves)
test_that("Validation Scenarios custom name missing col", {
 
  logdir <- tempdir()
  testthat::expect_output(ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation_custom_missing_col.xlsx",
                                                                outputDir = logdir), regexp = "some rules cannot be applied to sheet: SeasonalityCurves")
  
})

# test validation missing sheet specified in scenarios
test_that("Validation Scenarios custom name missing col", {
  
  logdir <- tempdir()
  testthat::expect_error(ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation_custom_missing_sheet.xlsx",
                                                        outputDir = logdir), regexp = 'sheet: "Task2" does not exist')
  
})

