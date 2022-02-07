library(ehep)
library(readxl)

ehep::Trace(FALSE)
ehep::InitializePopulation()
ehep::InitializeHealthcareTasks()
ehep::InitializeScenarios()
ehep::InitializeStochasticParameters()
ehep::InitializeSeasonality()

numtrials <- 300
date <- "Feb7"

scenarios <- read_xlsx("config/R Model Inputs.xlsx",sheet="Scenarios")

# Run through the full scenario list.
# for (i in 1:nrow(scenarios)){
#   print(paste("Startin scenario",i)) 
#   
#   scenario <- scenarios$UniqueID[i]
#   results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
#   ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,".csv",sep=""), scenario, 1)
#    
# }

ehep::InitializeStochasticParameters()
scenario <- "PopTest_NtlTotal"
append <- "_15_1_15_1_125_2005" #fertility p, mortality p, delta fertility p, delta mortality p, fert/mort delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,"_",append,".csv",sep=""), scenario, 1)


ehep::InitializeStochasticParameters()
scenario <- "PopTest_NtlTotal"
append <- "_12_12_15_15_125_2005" #fertility p, mortality p, delta fertility p, delta mortality p, fert/mort delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,"_",append,".csv",sep=""), scenario, 1)


ehep::InitializeStochasticParameters()
scenario <- "PopTest_NtlTotal"
append <- "_12_12_12_20_125_15_2005" #fertility p, mortality p, delta fertility p, delta mortality p, fertility delta q, mortality delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,"_",append,".csv",sep=""), scenario, 1)


ehep::InitializeStochasticParameters()
scenario <- "PopTest_NtlTotal"
append <- "_12_12_15_20_15_15_2005" #fertility p, mortality p, delta fertility p, delta mortality p, fertility delta q, mortality delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,"_",append,".csv",sep=""), scenario, 1)


ehep::InitializeStochasticParameters()
scenario <- "PopTest_NtlTotal"
append <- "_12_12_135_20_135_15_2005" #fertility p, mortality p, delta fertility p, delta mortality p, fertility delta q, mortality delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,"_",append,".csv",sep=""), scenario, 1)

ehep::InitializeStochasticParameters()
scenario <- "PopTest_NtlTotal"
append <- "_12_12_135_20_135_15_2005 MortEstimated2020" #fertility p, mortality p, delta fertility p, delta mortality p, fertility delta q, mortality delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,"_",append,".csv",sep=""), scenario, 1)

ehep::InitializeStochasticParameters()
scenario <- "PopTest_NtlTotal"
append <- "_12_12_135_20_135_15_2005 MortReported2019" #fertility p, mortality p, delta fertility p, delta mortality p, fertility delta q, mortality delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,"_",append,".csv",sep=""), scenario, 1)

ehep::InitializeStochasticParameters()
scenario <- "PopTest_NtlTotal"
append <- "_12_12_135_22_135_16_2005 MortReported2019" #fertility p, mortality p, delta fertility p, delta mortality p, fertility delta q, mortality delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,"_",append,".csv",sep=""), scenario, 1)
