library(ehep)

withr::local_dir("..")

test_that("explodeMortalityRates: basic", {
  ehep::InitializePopulation()
  ehep::InitializeHealthcareTasks()
  ehep::InitializeScenarios()
  ehep::InitializeStochasticParameters()
  ehep::InitializeSeasonality()

  scenario <- "ScenarioA"
  results <-
    ehep::RunExperiments(scenarioName = scenario,
                         trials = 2,
                         debug = FALSE)

  testthat::expect_true(TRUE)
})

