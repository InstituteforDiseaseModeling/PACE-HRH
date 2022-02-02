library(ehep)
library(readxl)

ehep::Trace(FALSE)
ehep::InitializePopulation()
ehep::InitializeHealthcareTasks()
ehep::InitializeScenarios()
ehep::InitializeStochasticParameters()
ehep::InitializeSeasonality()

#ehep::SetGlobalStartEndYears(2020, 2035)
numtrials <- 500

#gpe <- ehep:::globalPackageEnvironment

scenarios <- read_xlsx("config/R Model Inputs.xlsx",sheet="Scenarios")

# Run through the full scenario list. Only use this once scenario manager is working.
for (i in 1:nrow(scenarios)){
  print(paste("Startin scenario",i)) 
  
  scenario <- scenarios$UniqueID[i]
  date <- "Jan26"
  results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
  ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",date,".csv",sep=""), scenario, 1)
   
}

# Baseline National - original guess on p/q values
scenario <- "PopCheck_NationalTotal"
append <- "_2_1_15_1_2" #fertility p, mortality p, delta fertility p, delta mortality p, delta q
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",append,".csv",sep=""), scenario, 1)

# # National - reduced delta p/q values
# scenario <- "PopCheck_NationalTotal"
# append <- "_2_1_15_1_125" #fertility p, mortality p, delta fertility p, delta mortality p, delta q
# results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
# ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",append,".csv",sep=""), scenario, 1)
# 
# # National - reduced fertility p values
# scenario <- "PopCheck_NationalTotal"
# append <- "_1_1_15_1_125" #fertility p, mortality p, delta fertility p, delta mortality p, delta q
# results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
# ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",append,".csv",sep=""), scenario, 1)
# 
# # Baseline National - original guess on p/q values + fertility deltas 2019/11
# scenario <- "PopCheck_NationalTotal"
# append <- "_2_1_15_1_2_2011" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertility calc year
# results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
# ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",append,".csv",sep=""), scenario, 1)

# Baseline National - original guess on p/q values + fertility deltas 2019/05
### Choosing the 2005-->2019 delta fertility rates because they best match birth numbers to UN Population projections through 2035. Did a comparison across all of the possible DHS deltas to 2019 and 2005 had the best match. 
###This is reasonable because it represents recent time but has enough years to be less sensitive to short-term fluctuations.
scenario <- "PopCheck_NationalTotal"
append <- "_2_1_15_1_2_2005" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",append,".csv",sep=""), scenario, 1)

# # Baseline National - original guess on p/q values + fertility deltas 2019/2000
# scenario <- "PopCheck_NationalTotal"
# append <- "_2_1_15_1_2_2000" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertility calc year
# results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
# ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",append,".csv",sep=""), scenario, 1)

ehep::InitializeStochasticParameters()
scenario <- "PopCheck_NationalTotal"
append <- "_15_1_15_1_125_2005" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertility calc year
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,"_",append,".csv",sep=""), scenario, 1)



########################################### 
# First Draft GR deck

# Baseline Rural expected value
scenario <- "ExpectedValue_RuralRatio"
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,".csv",sep=""), scenario, 1)

# No fertility decrease, rural expected value
# o_Fertility should be false, but set it to true.
# Set delta rate to 1.0 in PopValues fertility rows 5,17-22.
scenario <- "NoFertilityChange_RuralRatio"
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,".csv",sep=""), scenario, 1)

# No gains in HIV, malaria, or TB. 
# o_MHIVTB_decr should be false, doesn't matter currently bc/ doesn't function.
# Set Annual Deltas in TaskValues sheet for M, TB, HIV to 1.0.
scenario <- "p_ChildHealthImp_RR"
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,".csv",sep=""), scenario, 1)

# No gains in child health. 
# o_ChildDis_decr should be false, doesn't matter currently bc/ doesn't function.
# Set Annual Deltas in TaskValues sheet for sick child care rows to 1.0.
scenario <- "p_Demographics_RR"
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,".csv",sep=""), scenario, 1)

# No seasonality.
# Set o_Seasonality to FALSE in row 4. This is not strictly the right way to do this (there should be an intermediate step of population growth, but that doesn't work yet.)
# Will need to rerun this later.
scenario <- "p_Seasonality_RR"
results <-   ehep::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
ehep::SaveSuiteResults(results, paste("results/results_",scenario,".csv",sep=""), scenario, 1)


