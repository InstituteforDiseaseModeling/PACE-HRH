library(pacehrh)

withr::local_dir("..")

.scenario <- "MergedModel"
.nTrials <- 5
.startYear <- 2025
.endYear <- 2050
.nMonths <- 12 * length(.startYear:.endYear)

.results <- NULL

test_that("Plotting: setup", {
  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()

  # Make sure to use a scenario that has seasonality results!


  pacehrh::SetGlobalStartEndYears(.startYear, .endYear)

  .results <<-
    pacehrh::RunExperiments(scenarioName = .scenario,
                            trials = .nTrials)

  testthat::expect_true(!is.null(.results))
})

test_that("Plotting: confirm setup", {
  testthat::expect_true(!is.null(.results))
  testthat::expect_equal(length(.results), .nTrials)


  pacehrh::PlotPopulationCurve(.results[[2]]$Population[["2030"]]$Male)
})
