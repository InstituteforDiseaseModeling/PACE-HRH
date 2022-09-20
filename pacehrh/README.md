# The PACE-HRH Package

This README describes the features and functions of the PACE-HRH package.

## Sample

```
# Load the pacehrh library
library(pacehrh)

# [OPTIONAL] Turn on tracing if you want the package to tell you more
# about what it's doing and any problems it encounters

# pacehrh::Trace(TRUE)

# Initialize the package
pacehrh::InitializePopulation()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()

pacehrh::SetGlobalStartEndYears(2020, 2040)

# Set the name of a scenario to run. The scenario must be in the list defined
# in the input data spreadsheet.
scenario <- "ScenarioA"
num_trials <- 10
run_id <- 1

# Run the scenario
results <-
  pacehrh::RunExperiments(scenarioName = scenario,
                       trials = num_trials)

# Save the results to a CSV file
pacehrh::SaveSuiteResults(results, "results.csv", scenario, run_number)
```

