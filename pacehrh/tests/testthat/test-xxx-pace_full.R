library(pacehrh)

withr::local_dir("..")

test_that("Full experiment: basic", {
  skip("Temporarily skipped")
  return


  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()

  scenario <- "ScenarioA"
  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                         trials = 2,
                         debug = FALSE)

  testthat::expect_true(TRUE)
})

