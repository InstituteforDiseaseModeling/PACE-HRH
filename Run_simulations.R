library(plyr)
library(dplyr)
library(pacehrh)
library(readxl)
library(devtools)
library(beepr)
library(tidyverse)

#############################################################
#Run the lines below after changing model_inputs.xlsx file#
#############################################################
rmarkdown::render(input = "validation_report.Rmd",
                  output_format = "html_document",
                  output_dir = "log",
                  params=list(inputFile="config/model_inputs.xlsx", outputDir="log"))
shell.exec(normalizePath("log/validation_report.html"))
print("Please check validation results in \"log\" folder", quote=FALSE)

rm(list = ls())

pacehrh::Trace(TRUE)
pacehrh::InitializePopulation()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()
pacehrh::SetRoundingLaw("Late")

scenarios <- read_xlsx("config/model_inputs.xlsx",sheet="Scenarios")

numtrials <- 50
date <- Sys.Date()
usefuldescription <- scenarios$Geography_dontedit[1]

# Run through the full scenario list.
for (i in 1:nrow(scenarios)){
  print(paste("Starting scenario",i))
  scenario <- scenarios$UniqueID[i]
  geoname <- scenarios$Geography_dontedit[i]
  results <- pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
  # create demographic plots and save them to a pdf file for visual inspection
  # destination = paste("results/population", geoname, scenario, date, ".pdf", sep="_")
  # pdf(file=destination)
  # visually inspect population predictions of the model
  resultspop <- SaveSuiteDemographics(results) %>%
    pivot_longer(c("Female", "Male"), names_to ="Gender", values_to = "Population")
  popsummary <- resultspop %>%
    group_by(Year, Gender, Age) %>%
    summarize(Population=mean(Population))
  pop2020 <- subset(popsummary, Year==2020)
  pop2020M <- sum(pop2020$Population[pop2020$Gender=="Male"])
  print(paste("2020 Male population is: ", round(pop2020M,0)))
  pop2020F <- sum(pop2020$Population[pop2020$Gender=="Female"])
  print(paste("2020 Female population is: ", round(pop2020F,0)))
  pop2035 <- subset(popsummary, Year==2035)
  pop2035M <- sum(pop2035$Population[pop2035$Gender=="Male"])
  print(paste("2035 Male population is: ", round(pop2035M,0)))
  pop2035F <- sum(pop2035$Population[pop2035$Gender=="Female"])
  print(paste("2035 Female population is: ", round(pop2035F,0)))
  plot_checkpop <- ggplot()+
    theme_bw()+
    geom_point(data = pop2020, aes(x=Age, y=Population, color=Gender), shape=1, position = "jitter")+
    geom_point(data = pop2035, aes(x=Age, y=Population, color=Gender), shape=4, position = "jitter")+
    labs(title=paste(scenario,"Start Pop M:", round(pop2020M,0), "F:", round(pop2020F,0), "; End Pop M:", round(pop2035M,0), "F:", round(pop2035F,0)))
  print(plot_checkpop)
  # visually inspect fertility rates predictions of the model
  plot_checkfertility <- pacehrh::PlotFertilityRatesStats(results, type = "boxplot", log = FALSE)
  print(plot_checkfertility)
  # visually inspect mortality rates predictions of the model, by male and female
  mortalityrates_male <- GetSuiteRates(results, "maleMortality")
  plot_checkmortality_m <- ggplot(mortalityrates_male, aes(x = Year, y = Rate, color = Label, group = Year)) +
    geom_boxplot() +
    theme(legend.position = "none") +
    facet_wrap(vars(Label), scales = "free_y", ncol=2)+
    labs(title="Male mortality rates predictions")
  print(plot_checkmortality_m)
  mortalityrates_female <- GetSuiteRates(results, "femaleMortality")
  plot_checkmortality_f <- ggplot(mortalityrates_female, aes(x = Year, y = Rate, color = Label, group = Year)) +
    geom_boxplot() +
    theme(legend.position = "none") +
    facet_wrap(vars(Label), scales = "free_y", ncol=2)+
    labs(title="Female mortality rates predictions")
  print(plot_checkmortality_f)
  #dev.off()
  #save simulation results to csv files, by scenario
  pacehrh::SaveSuiteResults(results, paste("results/results_",usefuldescription,"_",scenario,"_",date,".csv",sep=""), scenario, 1)
}

beep()

#Post-processing steps: read and collate simulation results into summary statistics tables to be used in analyses
resultsFiles <- vector(mode='list', 0)
for (i in 1:nrow(scenarios)){
  resultsFiles <- c(resultsFiles, paste("results/results_",usefuldescription,"_",scenarios$UniqueID[i],"_",date,".csv",sep=""))
}

print("Read and collate results")
DR_test <- pacehrh::ReadAndCollateSuiteResults(files = resultsFiles)
print("Compute Cadre allocation")
CA <- pacehrh:::ComputeCadreAllocations(DR_test)
print("Compute summary stats")
SS <- pacehrh:::ComputeSummaryStats(DR_test, CA)
print("Attach scenario details")
Mean_ServiceCat <- SS$Mean_ServiceCat %>%
  inner_join(scenarios, by= c("Scenario_ID" = "UniqueID"))
Mean_MonthlyTask <- SS$Mean_AnnualTask %>% 
  inner_join(scenarios, by=c("Scenario_ID" = "UniqueID"))
Stats_TotClin <- SS$Stats_TotClin %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))
Mean_ClinCat <- SS$Mean_ClinCat %>%
  inner_join(scenarios, by=c("Scenario_ID"="UniqueID", "WeeksPerYr"))
Mean_Total <- SS$Mean_Total %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))
Stats_ClinMonth <- SS$Stats_ClinMonth %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))
ByRun_ClinMonth <- SS$ByRun_ClinMonth %>% 
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))
Mean_Alloc <- SS$Mean_Alloc %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr"))

write.csv(Mean_ServiceCat,paste("results/Mean_ServiceCat_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_MonthlyTask,paste("results/Mean_MonthlyTask_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Stats_TotClin,paste("results/Stats_TotClin_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_ClinCat,paste("results/Mean_ClinCat_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_Total,paste("results/Mean_Total_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Stats_ClinMonth,paste("results/Stats_ClinMonth_",usefuldescription,"_",date,".csv",sep=""))
write.csv(ByRun_ClinMonth,paste("results/ByRun_ClinMonth_",usefuldescription,"_",date,".csv",sep=""))
write.csv(Mean_Alloc,paste("results/Mean_Alloc_",usefuldescription,"_",date,".csv",sep=""))
