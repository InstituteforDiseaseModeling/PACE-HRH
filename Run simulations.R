library(ehep)
library(readxl)
library(devtools)
library(beepr)

ehep::Trace(FALSE)
ehep::InitializePopulation()
ehep::InitializeHealthcareTasks()
ehep::InitializeScenarios()
ehep::InitializeStochasticParameters()
ehep::InitializeSeasonality()

numtrials <- 500
date <- "March23_ECOruns"

scenarios <- read_xlsx("config/R Model Inputs.xlsx",sheet="Scenarios")

# Run through the full scenario list.
for (i in 1:nrow(scenarios)){
  print(paste("Starting scenario",i))

  scenario <- scenarios$UniqueID[i]
  results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
  ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,".csv",sep=""), scenario, 1)

}

beep()