library(ehep)

withr::local_dir("..")

test_that("Experiment control: bad scenarios", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  ehep:::setGlobalConfig(inputExcelFilePath = "./simple_config/Test Inputs.xlsx")

  ehep::InitializeScenarios()
  testthat::expect_true(!is.null(e$scenarios))

  out <- SaveBaseSettings(scenarioName = "")
  testthat::expect_true(is.null(out))

  out <- SaveBaseSettings(scenarioName = NULL)
  testthat::expect_true(is.null(out))

  out <- SaveBaseSettings(scenarioName = "not-a-scenario")
  testthat::expect_true(is.null(out))
})

# Test that the correct sheets are read, based on the sheet names in the
# scenarios record.

test_that("Experiment control: basic read from Excel", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)

  local_vars("initialPopulation", envir = e)
  local_vars("populationLabels", envir = e)
  local_vars("scenarios", envir = e)
  local_vars("seasonalityCurves", envir = e)
  local_vars("seasonalityOffsets", envir = e)

  ehep:::setGlobalConfig(inputExcelFilePath = "./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL

  ehep::InitializePopulation()
  ehep::InitializeScenarios()

  testthat::expect_true(!is.null(e$scenarios))

  scenarioName <- "TEST_CustomSheets_1"
  assertthat::assert_that(scenarioName %in% e$scenarios$UniqueID)

  result <- ehep::SaveBaseSettings(scenarioName)

  testthat::expect_true(!is.null(result))
  testthat::expect_true(result$UniqueID == scenarioName)

#  print("TBD TBD TBD")

  # print(ehep:::GPE$taskData)
  # print(ehep:::BVE$taskParameters)
  # print(ehep:::GPE$seasonalityCurves)
})
