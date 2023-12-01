library(plyr)
library(dplyr)
library(pacehrh)
library(readxl)
library(devtools)
library(beepr)
library(tidyverse)
library(tidyr)


rm(list = ls())
input_file = "./config/model_inputs.xlsx"
pacehrh::Trace(TRUE)
SetInputExcelFile(inputExcelFilePath = input_file)
pacehrh::InitializePopulation()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()
pacehrh::InitializeCadreRoles()
pacehrh::SetGlobalStartEndYears(2020, 2040)
pacehrh::SetRoundingLaw("Late")


scenarios <- read_xlsx(input_file,sheet="Scenarios")$UniqueID

first_year = "2020"

df_trials <- data.frame(scenario = character(0),
                        seed = numeric(0),
                        trial= numeric(0), 
                        expect_mean = numeric(0), 
                        mean= numeric(0), 
                        diff =numeric(0), 
                        diff_percent = numeric(0),
                        duration=numeric(0))

ran_seeds = sample(1:10000, 3)
for (scenario in scenarios){
  print(paste0("Checking ", scenario))
  pacehrh::SetStochasticity(FALSE)
  results_deterministic <- pacehrh::RunExperiments(scenarioName = scenario, trials = 2, debug = FALSE)
  mean_deterministic = mean(as.matrix(data.frame(lapply(results_deterministic, function(x){x$AnnualTimes[,first_year]}))))
  pacehrh::SetStochasticity(TRUE)
  # Run through the full scenario list.
  for (i in  c(10, 50, 100, 200, 300, 400, 500, 800, 1000)){
    for (seed in ran_seeds){
      start_time <- Sys.time()
      results <- pacehrh::RunExperiments(scenarioName = scenario, trials = i, debug = FALSE, seed = seed)
      end_time <- Sys.time()
      # duration = end_time - start_time
      duration = as.numeric(difftime(end_time, start_time, units="secs"))
      print(paste0("seed:", seed, " iteration:", i, " duration:", duration))
      mean_stochastic = mean(as.matrix(data.frame(lapply(results, function(x){x$AnnualTimes[,first_year]}))))
      diff = abs(mean_deterministic - mean_stochastic)
      diff_percent  = round(diff / mean_deterministic * 100.0, digits=2)
      df_trials[nrow(df_trials)+1, ] <- c(as.character(scenario), 
                                          as.numeric(seed),
                                          as.numeric(i), 
                                          as.numeric(mean_deterministic), 
                                          as.numeric(mean_stochastic),
                                          as.numeric(diff), 
                                          as.numeric(diff_percent), 
                                          as.numeric(duration)
      )
      
    }
   
  }
}

beep()

# Columns to change data type except 'ide'
numeric_cols <- setdiff(names(df_trials), "scenario")
df_trials[, numeric_cols] <- lapply(df_trials[, numeric_cols], function(x) as.numeric(x))

df_avg <- df_trials %>%
  group_by(scenario, seed, trial) %>%
  summarise(duration = mean(duration), diff_percent = mean(diff_percent))


plot <- ggplot(df_avg, aes(x = trial)) +
  geom_line(aes(y = duration), color = "blue") +
  geom_line(aes(y = diff_percent * 100), color = "red") +
  scale_y_continuous(
    name = "Run Duration (second)",
    sec.axis = sec_axis(~./100, name = "Diff Percent %")
  ) +
  labs(title = paste0("Trial Experiments Plots for first year average AnnualTimes (Scenario ~ Seed)")) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  facet_grid(seed ~ scenario)

output_file <- paste0(gsub("\\.xlsx$", "", input_file),"_iteration_check.png")
ggsave(output_file, plot)