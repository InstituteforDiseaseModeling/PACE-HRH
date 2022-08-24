context("Test ValidateInputExcelFileContent")
local_edition(3)
setwd("../..")
source("src/ValidateInput.R")

# test validate no rules
test_that("Validation no rules", {
  # test should fail if no rule is defined for the sheet
  testthat::expect_error(ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation.xlsx", sheetNames = c("Total_Pop2")), "rule not found")
  
})
# test validation capture
test_that("Validation capture", {
  errCode <- .errValidationRuleFailed
  # In this example rules Total_diarrhea does not sum to 1, this is a table-wise error not row-wise error
  # The error is captured and verified in _snaps/ehep_lint.md
  
  logdir <- tempdir()
  # skip_if(.Platform$OS.type != "windows")
  testthat::expect_snapshot(ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation.xlsx",
                                                                outputDir = logdir,
                                                                sheetNames = c("SeasonalityCurves")))
  
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
                                                                      sheetNames = c("TaskValues_ref")))
  result_file <- paste(logdir, "info_violation_Missing_parameter_value.csv", sep="/")
  testthat::expect_true(file.exists(result_file))
  violations <- read.csv(result_file)
  # search for the row that fails Indicator = FH.MN.ANC.1XX
  violating_row <- violations %>%
    dplyr::filter(Indicator == "FH.MN.ANC.1XX")
  testthat::expect_equal(nrow(violating_row ), 1)
})