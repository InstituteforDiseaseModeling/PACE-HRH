library(pacehrh)
setwd("../..")
source("tests/testthat/test_utils.R")

with_parameters_test_that("demo model seasonality",{
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    test_template(input_file)
    numtrials <- 2
    start = 2020
    end = 2040
    # scenario <- "BasicServices"
    # task <- "Malaria_test"
    # type <- "Malaria"
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    pacehrh::SaveSuiteResults(results, paste("tests/results/regression_demo/test_seasonality_", scenario,".csv",sep=""), scenario, 1)
    pattern = glue::glue("test_seasonality_{scenario}.*.csv")
    DR_test <- do.call(rbind,
                       lapply(paste("tests/results/regression_demo", list.files(path = "tests/results/regression_demo", pattern = pattern), sep="/"), read.csv))
   
    DR_test <- DR_test %>% 
      filter(Task_ID == task) %>%
      select(Task_ID, Scenario_ID, Year, Month, Service_time, Num_services, Trial_num, Run_num)
    
    DR_test <- DR_test %>% 
      filter(Year > start) %>%
      filter(Year < end)
    
    expect_doppelganger(glue::glue("{task}_seasonality"), calculate_decomposition(DR_test, scenario, task, type))
    
  })
},  
cases(
  malaria = list(scenario = "BasicServices", task = "Malaria_test", type="Malaria"),
  ANC = list(scenario = "BasicServices", task = "ANC", type="Births"),
  Malnutrition_U5 = list(scenario = "BasicServices", task = "Malnutrition_U5", type ="Malnutrition")
))



