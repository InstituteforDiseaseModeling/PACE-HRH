library(pacehrh)
setwd("../..")
source("tests/testthat/test_utils.R")

plot_pop <- function(results, scenario,  startyear=2020, endyear=2035){
  resultspop <- SaveSuiteDemographics(results) %>%
    pivot_longer(c("Female", "Male"), names_to ="Gender", values_to = "Population")
  popsummary <- resultspop %>%
    dplyr::group_by(Year, Gender, Age) %>%
    dplyr::summarize(Population=mean(Population))
  popStart <- subset(popsummary, Year==startyear)
  popStartM <- sum(popStart$Population[popStart$Gender=="Male"])
  print(paste(str(startyear), " Male population is: ", round(popStartM,0)))
  popStartF <- sum(popStart$Population[popStart$Gender=="Female"])
  print(paste(str(startyear), " Female population is: ", round(popStartF,0)))
  popEnd <- subset(popsummary, Year==endyear)
  popEndM <- sum(popEnd$Population[popEnd$Gender=="Male"])
  print(paste(str(endyear), " Male population is: ", round(popEndM,0)))
  popEndF <- sum(popEnd$Population[popEnd$Gender=="Female"])
  print(paste(str(endyear), " Female population is: ", round(popEndF,0)))
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

test_that("demo model population flat",{
  # if rate change is capped at 1.0 with stochasticity turned off, fertility rate will look flat
  input_file <- "config/model_inputs_demo.xlsx"
  new_input_file <- "tests/flat_fertility.xlsx"
  wb <- openxlsx::loadWorkbook(input_file)
  flat_cap <- tibble(RateCategory = c("Mortality", "Fertility", "Incidence"),  Min = 1.0, Max=1.0)
  openxlsx::writeData(wb, sheet = "ChangeRateLimits", flat_cap, colNames = TRUE, rowNames=FALSE)
  openxlsx::saveWorkbook(wb, new_input_file, overwrite = TRUE)
  on.exit(file.remove(new_input_file))
  local({
    start = 2020
    end = 2040
    test_template(new_input_file, start=start, end=end, stochasticity=FALSE)
    numtrials <- 2
    scenario <- "BasicServices"
    # Sys.setenv( TESTTHAT_PKG="pacehrh")
    # m <- local_mock(`pacehrh:::loadChangeRateLimits` = tibble::tibble(RateCategory = c("Mortality", "Fertility", "Incidence"),  Min = 1, Max=1),
    #                 mock_env = as.environment("package:pacehrh"),
    #                 eval_env = as.environment("package:pacehrh"))
    
    print(pacehrh:::BVE$changeRateLimits)
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    expect_doppelganger(glue::glue("{scenario}_population_flat"), pacehrh::PlotFertilityRatesStats(results))
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

