testthat::local_edition(3)
library(pacehrh)
library(dplyr)
setwd("../..")
source("tests/testthat/test_utils.R")

plot_cadre <- function(Mean_Alloc, startyear, endyear){
  Cadre_labelled <- Mean_Alloc %>% 
    dplyr::filter(CI50!=0 ) %>% 
    dplyr::filter(Year > startyear & Year <= endyear) %>%
    dplyr::group_by(Scenario_ID, Year) %>% 
    dplyr::mutate(sum_CI50 = sum(CI50), sum_CI05 = sum(CI05), sum_CI95 = sum(CI95))
  # print(Cadre_labelled)
  ggplot(data=Cadre_labelled) +
    geom_bar(aes(x=Year,y=CI50/WeeksPerYr,fill=RoleDescription),stat="identity",alpha=.9)
}

test_that("demo model cadre allocation",{
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    start = 2020
    end = 2035
    test_template(input_file, popSheet="TotalPop", start=start, end=end)
    numtrials <- 50
    scenario <- "Expanded"
    scenarios <- read_xlsx(input_file,sheet="Scenarios")
    cadreroles <- read_xlsx(input_file,sheet="CadreRoles")
    results <- pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    ER <-  pacehrh::SaveExtendedSuiteResults(results)
    CA <- ER %>% pacehrh::SaveCadreAllocations()
    SS <- pacehrh::ComputeSummaryStats(ER, CA)
    cadreOverheadTime <- pacehrh::SaveCadreOverheadData()
    cadreOverheadTime$Year = as.integer(cadreOverheadTime$Year)
    AnnualOverheadTime <- cadreOverheadTime %>% 
      group_by(Scenario_ID, Year) %>% 
      dplyr::summarise(CI05 = sum(OverheadTime), 
                       CI25 = sum(OverheadTime), 
                       MeanHrs = sum(OverheadTime),
                       CI75 = sum(OverheadTime), 
                       CI95 = sum(OverheadTime)) %>% 
      dplyr::mutate(ClinicalOrNon = "Overhead", ClinicalCat = "-") %>% 
      dplyr::filter(Year>=start & Year<=end)
    Mean_Alloc <- SS$Mean_Alloc %>% 
      # inner_join(scenarios, by= c("Scenario_ID"="UniqueID")) %>%
      separate(col = Cadre,into =  c("Role_ID", "suffix"), sep = "_", remove = FALSE) %>% 
      dplyr::left_join(cadreOverheadTime, by = c("Scenario_ID","Role_ID", "Year")) %>% 
      dplyr::left_join(cadreroles, by = c("Scenario_ID"="ScenarioID", "Role_ID"="RoleID")) %>% 
      dplyr::group_by(Scenario_ID, Year, RoleDescription, WeeksPerYr) %>% 
      dplyr::summarise(CI05 = sum(CI05+OverheadTime), 
                       CI25 = sum(CI25+OverheadTime), 
                       CI50 = sum(CI50+OverheadTime), 
                       CI75 = sum(CI75+OverheadTime), 
                       CI95 = sum(CI95+OverheadTime))
    summary_HPW <- Mean_Alloc %>% group_by(RoleDescription, Year) %>% summarise(CI50/WeeksPerYr) %>% arrange(RoleDescription, Year)
    filename = "tests/results/regression/extended_allocation.csv"
    write.csv(summary_HPW, "tests/results/regression/extended_allocation.csv")
    expect_snapshot_file(filename, compare=testthat::compare_file_text)
    expect_doppelganger(glue::glue("{scenario}_cadre_allocation"), plot_cadre(Mean_Alloc, start, end))
  })
})