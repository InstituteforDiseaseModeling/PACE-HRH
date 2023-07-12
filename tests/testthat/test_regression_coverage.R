testthat::local_edition(3)
library(pacehrh)
library(dplyr)
setwd("../..")
source("tests/testthat/test_utils.R")

plot_service_time<- function(ER, task){
  result <- ER %>% filter (Task_ID == task) %>% 
    dplyr::group_by(Year, Month) %>% 
    dplyr::summarise(Service_time = mean(Service_time), Coverage_service_time = mean(Coverage_service_time)) %>%
    ungroup() %>% 
    group_by(Year) %>% 
    dplyr::summarise(Service_time = sum(Service_time), Coverage_service_time = sum(Coverage_service_time))
 ggplot(data= result) +
   geom_bar(aes(x = Year, y=Service_time), stat = "identity", fill = "red", alpha=0.5) +
   geom_bar(aes(x = Year, y=Coverage_service_time), stat = "identity", fill ="blue", alpha=0.5)
   
}

test_that("demo model step coverage",{
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    start = 2020
    end = 2040
    test_template(input_file, popSheet="TotalPop", start=start, end=end, rounding="Late")
    numtrials <- 10
    scenario <- "StepCoverage"
    scenarios <- read_xlsx(input_file,sheet="Scenarios")
    results <- pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    ER <-  pacehrh::SaveExtendedSuiteResults(results)
    # Check ANC step coverage
    result_ANC <- ER %>% dplyr::filter (Task_ID == "ANC") %>% 
      select(Year, Coverage) %>% 
      distinct() %>% 
      mutate(expected_coverage = ifelse(((Year-2020) %/% 5 + 1) *0.25 > 1,1, ((Year-2020) %/% 5 + 1) *0.25))
    expect_true(all(result_ANC$Coverage == result_ANC$expected_coverage))
    result_Newborn <- ER %>% dplyr::filter (Task_ID == "Newborn") %>% 
      select(Year, Coverage) %>% 
      distinct() %>% 
      mutate(expected_coverage = ifelse(((Year-2020) %/% 10) *1 > 1,1, ((Year-2020) %/% 10) *1))
    expect_true(all(result_Newborn$Coverage == result_Newborn$expected_coverage))

    expect_doppelganger(glue::glue("{scenario}_coverage_ANC"), plot_service_time(ER, "ANC"))
    expect_doppelganger(glue::glue("{scenario}_coverage_Newborn"), plot_service_time(ER, "Newborn"))
  })
})