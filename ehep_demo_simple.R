library(ehep)

ehep::InitializePopulation()
ehep::InitializeHealthcareTasks()
ehep::InitializeScenarios()
ehep::InitializeStochasticParameters()
ehep::InitializeSeasonality()

scenario <- "ScenarioA"
results <-
  ehep::RunExperiments(scenarioName = scenario,
                       trials = 5,
                       debug = FALSE)

ehep::SaveSuiteResults(results, "results.csv", scenario, 1)
