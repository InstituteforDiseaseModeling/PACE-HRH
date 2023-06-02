library(plyr)
library(dplyr)
library(pacehrh)
library(readxl)
library(devtools)
library(beepr)
library(tidyverse)
library(tidyr)

#############################################################
#Run the lines below after changing model_inputs.xlsx file#
#############################################################
# rmarkdown::render(input = "validation_report.Rmd",
#                   output_format = "html_document",
#                   output_dir = "log",
#                   params=list(inputFile="config/model_inputs.xlsx", outputDir="log"))
# shell.exec(normalizePath("log/validation_report.html"))
# print("Please check validation results in \"log\" folder", quote=FALSE)

rm(list = ls())

pacehrh::Trace(TRUE)
SetInputExcelFile(inputExcelFilePath = "./config/model_inputs_demo.xlsx")
pacehrh::InitializePopulation()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()
pacehrh::InitializeCadreRoles()
pacehrh::SetGlobalStartEndYears(2020, 2040)
pacehrh::SetRoundingLaw("Late")

scenarios <- read_xlsx("config/model_inputs_demo.xlsx",sheet="Scenarios")
cadreroles <- read_xlsx("config/model_inputs_demo.xlsx",sheet="CadreRoles")

numtrials <- 50
date <- Sys.Date()
usefuldescription <- "test"

SS_list <- list()
OH_list <- list()

# Run through the full scenario list.
for (i in 1:nrow(scenarios)){
  print(paste("Starting scenario",i))
  scenario <- scenarios$UniqueID[i]
  results <- pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
  # extract task-level simulation results
  SR <- pacehrh::SaveExtendedSuiteResults(results)
  # extract task-level service time allocation to cadre
  CA <- pacehrh::SaveCadreAllocations(SR)
  # generate summary stats tables
  print("Compute summary stats")
  summarystats <- pacehrh::ComputeSummaryStats(SR, CA)
  assign(paste0("SS_", scenario), summarystats)
  SS_list = append(SS_list, paste0("SS_", scenario))
  # extract health staff overhead times
  cadreOverheadHrs <- pacehrh::SaveCadreOverheadData(run = paste("Run", i , sep = "_"))
  assign(paste0("OH_",scenario), cadreOverheadHrs)
  OH_list = append(OH_list, paste0("OH_", scenario))
}

beep()

# Post-processing steps: 
# collate summary statistics tables across scenarios to be used in analyses
remove(cadreOverheadTime, Mean_Alloc, Mean_ClinCat, Mean_ServiceCat, Mean_Total, Stats_ClinMonth, ByRun_ClinMonth, Stats_TotClin )
StartYear = min(SR$Year)
EndYear = max(SR$Year)

for (each in OH_list){
  if(!exists('cadreOverheadTime')){
    cadreOverheadTime <- get(each)
  }else{
    cadreOverheadTime <- rbind(cadreOverheadTime, get(each))
  }
}
cadreOverheadTime$Year = as.integer(cadreOverheadTime$Year)

AnnualOverheadTime <- cadreOverheadTime %>% 
  group_by(Scenario_ID, Year) %>% 
  dplyr::summarise(CI05 = sum(OverheadTime), 
            CI25 = sum(OverheadTime), 
            MeanHrs = sum(OverheadTime),
            CI75 = sum(OverheadTime), 
            CI95 = sum(OverheadTime)) %>% 
  dplyr::mutate(ClinicalOrNon = "Overhead", ClinicalCat = "-") %>% 
  filter(Year>=StartYear & Year<=EndYear)

for (each in SS_list){
  SS_temp <-  get(each)
  
  if(!exists('Mean_Total')){
    Mean_Total <- SS_temp$Mean_Total
  }else{
    Mean_Total <- rbind(Mean_Total, SS_temp$Mean_Total)
  }
  
  if(!exists('Mean_ClinCat')){
    Mean_ClinCat <- SS_temp$Mean_ClinCat
  }else{
    Mean_ClinCat <- rbind(Mean_ClinCat, SS_temp$Mean_ClinCat)
  }
  
  if(!exists('Mean_ServiceCat')){
    Mean_ServiceCat <- SS_temp$Mean_ServiceCat
  }else{
    Mean_ServiceCat <- rbind(Mean_ServiceCat, SS_temp$Mean_ServiceCat)
  }
  
  if(!exists('Mean_Alloc')){
    Mean_Alloc <- SS_temp$Mean_Alloc
  }else{
    Mean_Alloc <- rbind(Mean_Alloc, SS_temp$Mean_Alloc)
  }
  
  if(!exists('Stats_ClinMonth')){
    Stats_ClinMonth <- SS_temp$Stats_ClinMonth
  }else{
    Stats_ClinMonth <- rbind(Stats_ClinMonth, SS_temp$Stats_ClinMonth)
  }
  
  if(!exists('ByRun_ClinMonth')){
    ByRun_ClinMonth <- SS_temp$ByRun_ClinMonth
  }else{
    ByRun_ClinMonth <- rbind(ByRun_ClinMonth, SS_temp$ByRun_ClinMonth)
  }
  
  if(!exists('Stats_TotClin')){
    Stats_TotClin <- SS_temp$Stats_TotClin
  }else{
    Stats_TotClin <- rbind(Stats_TotClin, SS_temp$Stats_TotClin)
  }
}

# Add in overhead hours for summary stats tables that include both clinical and non-clinical tasks
print("Add in overhead hours to relevant tables")
Mean_ClinCat <- Mean_ClinCat %>% 
  select(-WeeksPerYr) %>% 
  rbind(AnnualOverheadTime)

Mean_Total <- Mean_Total %>%
  select(-WeeksPerYr, -HrsPerWeek) %>% 
  rbind(AnnualOverheadTime[,1:7]) %>% 
  group_by(Scenario_ID, Year) %>% 
  dplyr::summarise(CI05 = sum(CI05),
            CI25 = sum(CI25),
            MeanHrs = sum(MeanHrs),
            CI75 = sum(CI75),
            CI95 = sum(CI95))

Mean_Alloc <- Mean_Alloc %>% 
  separate(col = Cadre,into =  c("Role_ID", "suffix"), sep = "_", remove = FALSE) %>% 
  left_join(cadreOverheadTime, by = c("Scenario_ID","Role_ID", "Year")) %>% 
  left_join(cadreroles, by = c("Scenario_ID"="ScenarioID", "Role_ID"="RoleID")) %>% 
  group_by(Scenario_ID, Year, RoleDescription) %>% 
  dplyr::summarise(CI05 = sum(CI05+OverheadTime), 
            CI25 = sum(CI25+OverheadTime), 
            CI50 = sum(CI50+OverheadTime), 
            CI75 = sum(CI75+OverheadTime), 
            CI95 = sum(CI95+OverheadTime))

# Attach scenario details 
print("Attach scenario details")

Stats_TotClin <- Stats_TotClin %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))

Stats_ClinMonth <- Stats_ClinMonth %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))

ByRun_ClinMonth <- ByRun_ClinMonth %>% 
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))

Mean_ServiceCat <- Mean_ServiceCat %>%
  inner_join(scenarios, by= c("Scenario_ID" = "UniqueID")) %>% 
  filter(ClinicalOrNon == "Clinical")

Mean_ClinCat <- Mean_ClinCat %>%
  inner_join(scenarios, by=c("Scenario_ID"="UniqueID"))

Mean_Total <- Mean_Total %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID"))

Mean_Alloc <- Mean_Alloc %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID"))

write.csv(Mean_ServiceCat,paste("results/Mean_ServiceCat_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Stats_TotClin,paste("results/Stats_TotClin_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_ClinCat,paste("results/Mean_ClinCat_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_Total,paste("results/Mean_Total_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Stats_ClinMonth,paste("results/Stats_ClinMonth_",usefuldescription,"_",date,".csv",sep=""))
write.csv(ByRun_ClinMonth,paste("results/ByRun_ClinMonth_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_Alloc,paste("results/Mean_Alloc_",usefuldescription,"_",date,".csv",sep=""))
