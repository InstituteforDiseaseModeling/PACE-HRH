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
rm(list = ls())

get_mortality <- function(results, categoryName, agebands){
  # Get mortality matrix to data frame
  # categoryName can be maleMortality, femaleMortality etc
  # ageband is a dataframe with Label column that matched results' mortality labels and AgeBand column that is custom defined
  
  # Access the rateMatrix
  all_trials_matrix <- 
    lapply(
      lapply(
        lapply(
          lapply(results, "[[", "PopulationRates"),
          "[[", categoryName),
        "[[", "ratesMatrix"),
      data.frame)
  
  # Pivot the rateMatrix year columns to a single column named "Year"
  all_trials_matrix <- lapply(all_trials_matrix, rownames_to_column, "Label")
  all_trials_matrix <- lapply(all_trials_matrix, 
                              function(x){
                                pivot_longer(x, colnames(x)[grepl("X", colnames(x))], 
                                             names_prefix = "X", 
                                             names_to = "Year", 
                                             values_to = "MortalityRate")
                              }
  )
  # Combine all trials and join to the custom AgeBand
  all_trials_matrix <- do.call(data.table::rbindlist, list(all_trials_matrix, idcol="Trial"))
  mortalityrates <- all_trials_matrix %>% 
    inner_join(agebands)
  return(mortalityrates)
}

rmarkdown::render(input = "validation_report.Rmd",
                  output_format = "html_document",
                  output_dir = "log",
                  params=list(inputFile="config/model_inputs.xlsx", outputDir="log"))
shell.exec(normalizePath("log/validation_report.html"))
print("Please check validation results in \"log\" folder", quote=FALSE)

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
  results <-   pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
  destination = paste("results/mortality",scenario,date,".pdf",sep="")
  pdf(file=destination)
  # remove(resultspop)
  
  df1 <- SaveSuiteDemographics(results)
  resultspop <- pivot_longer(df1, c("Female", "Male"), names_to ="Gender", values_to = "Population")
  resultspop %>% 
    dplyr::rename("Run" ="Trial") %>%
    dplyr::select(-AgeBucket)
  resultspop$Gender <- substr(resultspop$Gender,1,1)
 
  popsummary <- resultspop %>%
    dplyr::group_by(Year, Gender, Age) %>%
    dplyr::summarize(Population=mean(Population))
  pop2020 <- popsummary %>%
    subset(Year==2020)
  pop2020M <- sum(pop2020$Population[pop2020$Gender=="M"])
  pop2020F <- sum(pop2020$Population[pop2020$Gender=="F"])
  pop2035 <- popsummary %>%
    subset(Year==2035)
  pop2035M <- sum(pop2035$Population[pop2035$Gender=="M"])
  pop2035F <- sum(pop2035$Population[pop2035$Gender=="F"])
  g0 <- ggplot()+
    theme_bw()+
    geom_point(data = pop2020, aes(x=Age, y=Population, color=Gender), shape=1, position = "jitter")+
    geom_point(data = pop2035, aes(x=Age, y=Population, color=Gender), shape=4, position = "jitter")+
    labs(title=paste(scenario,"Start Pop M:", pop2020M, "F:", pop2020F, "; End Pop M:", pop2035M, "F:", pop2035F ))
  print(g0)
  
  g1 <- pacehrh::PlotFertilityRatesStats(results, type = "boxplot", log = FALSE)
  print(g1)
  
  agebands <- data.frame(Label=results[[1]]$PopulationRates$maleMortality$bandedRates$labels, 
                         AgeBand=c("Infants", "Y1_4", "Y5_9", "Y10_14", "Y15_19", "Y20-34", "Y35-49", "Y50-59", "Y60_74", "Y75+"))
  # remove(mortalityrates)
  
  mortalityrates <- get_mortality(results, "maleMortality", agebands)
  
  g2 <- ggplot(mortalityrates, aes(x = Year, y = MortalityRate, color = AgeBand, group = Year)) +
    geom_boxplot() +
    theme(legend.position = "none") +
    facet_wrap(vars(AgeBand), scales = "free_y", ncol=2)+
    labs(title="Male mortality rates predictions")
  print(g2)
  
  remove(mortalityrates)
  agebands <- data.frame(Label=results[[1]]$PopulationRates$femaleMortality$bandedRates$labels, 
                         AgeBand=c("Infants", "Y1_4", "Y5_9", "Y10_14", "Y15_19", "Y20-34", "Y35-49", "Y50-59", "Y60_74", "Y75+"))
  mortalityrates <- get_mortality(results, "femaleMortality", agebands)
 
  g3 <- ggplot(mortalityrates, aes(x = Year, y = MortalityRate, color = AgeBand, group = Year)) +
    geom_boxplot() +
    theme(legend.position = "none") +
    facet_wrap(vars(AgeBand), scales = "free_y", ncol=2)+
    labs(title="Female mortality rates predictions")
  print(g3)
  
  dev.off()
  pacehrh::SaveSuiteResults(results, paste("results/results_",usefuldescription,"_",scenario,"_",date,".csv",sep=""), scenario, 1)
}

beep()