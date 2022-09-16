library(ehep)

withr::local_dir("..")

test_that("Scenario configuration: basic read from Excel", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  ehep:::setGlobalConfig(inputExcelFilePath = "./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL

  testthat::expect_true(is.null(e$scenarios))
  ehep::InitializeScenarios()
  testthat::expect_false(is.null(e$scenarios))
  testthat::expect_equal(class(e$scenarios), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(names(e$scenarios[ehep:::.scenarioColumnNames]), ehep:::.scenarioColumnNames)
})

test_that("Scenario configuration: bad sheet name", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  ehep:::setGlobalConfig(inputExcelFilePath = "./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL

  testthat::expect_true(is.null(e$scenarios))
  ehep::InitializeScenarios(sheetName = "notasheet")
  testthat::expect_true(is.null(e$scenarios))
})

test_that("Scenario configuration: bad Excel file", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  ehep:::setGlobalConfig(inputExcelFilePath = "./simple_config/notafile.xlsx")
  e$scenarios <- NULL

  # This should fail (ie not return a scenario table) because the filename is bogus
  testthat::expect_true(is.null(e$scenarios))
  ehep::InitializeScenarios()
  testthat::expect_true(is.null(e$scenarios))
})

test_that("Scenario configuration: non-Excel configuration", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  ehep:::setGlobalConfig(inputExcelFilePath = "./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL

  testthat::expect_true(is.null(e$scenarios))

  # Create an empty scenarios table
  ehep::InitializeScenarios(loadFromExcel = FALSE)

  testthat::expect_false(is.null(e$scenarios))
  testthat::expect_equal(class(e$scenarios), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(names(e$scenarios[ehep:::.scenarioColumnNames]), ehep:::.scenarioColumnNames)
  testthat::expect_equal(nrow(e$scenarios), 0)
})
