library(pacehrh)
setwd("../..")
source("tests/testthat/test_utils.R")

plot_pop <- function(results, scenario,  startyear=2020, endyear=2035){
  resultspop <- SaveSuiteDemographics(results) %>%
    pivot_longer(c("Female", "Male"), names_to ="Gender", values_to = "Population")
  popsummary <- resultspop %>%
    group_by(Year, Gender, Age) %>%
    summarize(Population=mean(Population))
  popStart <- subset(popsummary, Year=startyear)
  popStartM <- sum(popStart$Population[popStart$Gender=="Male"])
  print(paste(str(startyear), " Male population is: ", round(popStartM,0)))
  popStartF <- sum(popStart$Population[popStart$Gender=="Female"])
  print(paste("2020 Female population is: ", round(popStartF,0)))
  popEnd <- subset(popsummary, Year==endyear)
  popEndM <- sum(popEnd$Population[popEnd$Gender=="Male"])
  print(paste("2035 Male population is: ", round(popEndM,0)))
  popEndF <- sum(popEnd$Population[popEnd$Gender=="Female"])
  print(paste("2035 Female population is: ", round(popEndF,0)))
  plot_checkpop <- ggplot()+
    theme_bw()+
    geom_point(data = popStart, aes(x=Age, y=Population, color=Gender), shape=1, position = "jitter")+
    geom_point(data = popEnd, aes(x=Age, y=Population, color=Gender), shape=4, position = "jitter")+
    labs(title=paste(scenario,"Start Pop M:", round(popStartM,0), "F:", round(popStartF,0), "; End Pop M:", round(popEndM,0), "F:", round(popEndF,0)))
  print(plot_checkpop)
}

test_that("demo model population",{
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    start = 2020
    end = 2040
    test_template(input_file, start=start, end=end)
    numtrials <- 10
    scenario <- "BasicServices"
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    expect_doppelganger(glue::glue("{scenario}_population"), plot_pop(results, scenario, start, end))
    
  })
})

test_that("demo model fertility", {
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    test_template(input_file)
    numtrials <- 10
    start = 2020
    end = 2040
    scenario <- "BasicServices"
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    expect_doppelganger(glue::glue("{scenario}_fertility"), pacehrh::PlotFertilityRatesStats(results, type = "boxplot", log = FALSE))
  })
})

