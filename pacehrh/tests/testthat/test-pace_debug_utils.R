library(pacehrh)

withr::local_dir("..")

test_that("Debugging tools: basic", {
  e <- pacehrh:::GPE
  local_vars("globalConfigLoaded", envir = e)
  e$globalConfigLoaded <- FALSE

  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()

  # Make sure to use a scenario that has seasonality results!
  scenario <- "MergedModel"
  nTrials <- 5
  startYear <- 2025
  endYear <- 2050
  nMonths <- 12 * length(startYear:endYear)

  shoulderYears <- pacehrh:::GPE$shoulderYears

  pacehrh::SetGlobalStartEndYears(startYear, endYear)

  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                            trials = nTrials)

  testthat::expect_true(!is.null(results))

  outFileName <- "wide_demographics.csv"
  writeCurrentExpDemographics_wide(filename = outFileName)
  testthat::expect_true(file.exists(outFileName))
  testthat::expect_snapshot_file(outFileName)
})
