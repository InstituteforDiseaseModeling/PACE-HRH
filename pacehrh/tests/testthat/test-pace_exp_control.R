library(pacehrh)

withr::local_dir("..")

test_that("Experiment control: bad scenarios", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  # Set input file, and cheat the system into thinking the global configuration
  # is already loaded
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$globalConfigLoaded <- TRUE

  pacehrh::InitializeScenarios()
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
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)

  local_vars("traceState", envir = e)
  pacehrh::Trace(TRUE)  
  
  local_vars("initialPopulation", envir = bve)
  local_vars("populationLabels", envir = bve)
  local_vars("scenarios", envir = e)
  local_vars("seasonalityCurves", envir = bve)
  local_vars("seasonalityOffsets", envir = bve)

  # Set input file, and cheat the system into thinking the global configuration
  # is already loaded
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$globalConfigLoaded <- TRUE
  e$scenarios <- NULL

  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeSeasonality()
  pacehrh::InitializeStochasticParameters()
  
  testthat::expect_true(!is.null(e$scenarios))

  scenarioName <- "TEST_CustomSheets_1"
  assertthat::assert_that(scenarioName %in% e$scenarios$UniqueID)

  result <- pacehrh::SaveBaseSettings(scenarioName)

  testthat::expect_true(!is.null(result))
  testthat::expect_true(result$UniqueID == scenarioName)
})
