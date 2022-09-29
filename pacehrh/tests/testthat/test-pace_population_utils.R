library(pacehrh)

withr::local_dir("..")



test_that("Applicable population matrices: stuff", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)

  inputFile <- "./simple_config/super_simple_inputs.xlsx"
  pacehrh:::setGlobalConfig(inputExcelFilePath = inputFile)

  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, inputFile)
})





test_that("Applicable population matrices: basic", {
  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()

  # Make sure to use a scenario that has seasonality results!
  scenario <- "MergedModel"
  nTrials <- 5
  startYear <- 2025
  endYear <- 2050
  shoulderYears <- 1

  pacehrh::SetGlobalStartEndYears(startYear, endYear, shoulderYears)

  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                            trials = nTrials)

  testthat::expect_true(!is.null(results))

  testthat::expect_null(pacehrh::ComputeApplicablePopulationMatrices(results, NULL))
  testthat::expect_null(pacehrh::ComputeApplicablePopulationMatrix(results[[1]], NULL))

  p <- pacehrh::ComputeApplicablePopulationMatrix(results[[1]])
  testthat::expect_equal(class(p), c("matrix", "array"))
  testthat::expect_type(p, "double")

  pm <- pacehrh::ComputeApplicablePopulationMatrices(results)
  testthat::expect_true(!is.null(pm))
  testthat::expect_equal(length(pm), nTrials)

  p <- pm[[1]]
  testthat::expect_equal(class(p), c("matrix", "array"))
  testthat::expect_type(p, "double")
})

test_that("Applicable population matrices: bad calls", {
  testthat::expect_null(pacehrh::ComputeApplicablePopulationMatrices(NULL))
  testthat::expect_null(pacehrh::ComputeApplicablePopulationMatrix(NULL))
})
