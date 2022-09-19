library(pacehrh)
library(readxl)
library(devtools)
library(beepr)
library(tidyverse)

#############################################################
#Run the lines below after changing model_inputs.xlsx file#
#############################################################
rm(list = ls())

rmarkdown::render(input = "validation_report.Rmd",
                  output_format = "html_document",
                  output_dir = "log",
                  params=list(inputFile="config/model_inputs.xlsx", outputDir="log"))
shell.exec(normalizePath("log/validation_report.html"))
print("Please check validation results in \"log\" folder", quote=FALSE)

pacehrh::Trace(TRUE)
pacehrh::InitializePopulation()
pacehrh::InitializeHealthcareTasks()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()

scenarios <- read_xlsx("config/model_inputs.xlsx",sheet="Scenarios")

numtrials <- 50
date <- Sys.Date()
usefuldescription <- scenarios$Geography_dontedit[1]

# Run through the full scenario list.
for (i in 1:nrow(scenarios)){
  print(paste("Starting scenario",i))
  scenario <- scenarios$UniqueID[i]
  geoname <- scenarios$Geography_dontedit[i]
  results <-   pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
  pacehrh::SaveSuiteResults(results, paste("results/results_",usefuldescription,"_",scenario,"_",date,".csv",sep=""), scenario, 1)
}

beep()


#Post-processing steps: read and collate simulation results into summary statistics tables to be used in analyses
resultsFiles <- vector(mode='list', 0)
for (i in 1:nrow(scenarios)){
  resultsFiles <- c(resultsFiles, paste("results/results_",usefuldescription,"_",scenarios$UniqueID[i],"_",date,".csv",sep=""))
}

print("Read and collate results")
DR_test <- ehep::ReadAndCollateSuiteResults(files = resultsFiles)
print("Compute Cadre allocation")
CA <- ehep:::ComputeCadreAllocations(DR_test)
print("Compute summary stats")
SS <- ehep:::ComputeSummaryStats(DR_test, CA)
print("Attach scenario details")
Mean_ServiceCat <- SS$Mean_ServiceCat %>%
  merge(scenarios, by.x = "Scenario_ID", by.y = "UniqueID")
Stats_TotClin <- SS$Stats_TotClin %>%
  merge(scenarios, by.x = "Scenario_ID", by.y = "UniqueID")
Mean_ClinCat <- SS$Mean_ClinCat %>%
  merge(scenarios, by.x = "Scenario_ID", by.y = "UniqueID")
Mean_Total <- SS$Mean_Total %>%
  merge(scenarios, by.x = "Scenario_ID", by.y = "UniqueID")
Stats_ClinMonth <- SS$Stats_ClinMonth %>%
  merge(scenarios, by.x = "Scenario_ID", by.y = "UniqueID")
Mean_Alloc <- SS$Mean_Alloc %>%
  merge(scenarios, by.x = "Scenario_ID", by.y = "UniqueID")

write.csv(Mean_ServiceCat,paste("results/Mean_ServiceCat_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Stats_TotClin,paste("results/Stats_TotClin_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_ClinCat,paste("results/Mean_ClinCat_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_Total,paste("results/Mean_Total_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Stats_ClinMonth,paste("results/Stats_ClinMonth_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_Alloc,paste("results/Mean_Alloc_",usefuldescription,"_",date,".csv",sep=""))
