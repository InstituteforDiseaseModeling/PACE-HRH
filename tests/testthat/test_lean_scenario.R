testthat::local_edition(3)
library(pacehrh)
setwd("../..")
source("tests/testthat/test_utils.R")
source("tests/testthat/test_per_age.R")


test_that("check_demo_template", {
  # TODO:
  # There is currently warning about
  # tasks in the scenario offsets table are not used in task values sheets
  # We need to define the validate behavior in lint
  input_file <- "config/model_inputs_demo.xlsx"
  expect_true(pacehrh::CheckInputExcelFileFormat(input_file) %in% c(pacehrh:::.Success, pacehrh:::.warnProblemsFound))
  
})

test_that("model regression annual",{
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    setting = "annual"
    test_template(input_file, setting = setting)
    numtrials <- 5
    scenario <- "BasicServices"
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    # save results for selected tasks that are representative of seasonality
    expect_doppelganger("ANC",  draw_comparison(results, "ANC", numtrials , setting, 1))
    expect_doppelganger("Diarrhea_U5",  draw_comparison(results, "Diarrhea_U5", numtrials , setting, 1))
    expect_doppelganger("Malaria_test",  draw_comparison(results, "Malaria_test", numtrials , setting, 1))
  })
})

test_that("model regression monthly",{
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    setting = "monthly"
    test_template(input_file, setting = setting)
    numtrials <- 5
    scenario <- "BasicServices"
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    # save results for selected tasks that are representative of seasonality
    expect_doppelganger("ANC_monthly",  draw_comparison(results, "ANC", numtrials , setting, 1))
    expect_doppelganger("Diarrhea_U5_monthly",  draw_comparison(results, "Diarrhea_U5", numtrials , setting, 1))
    expect_doppelganger("Malaria_test_monthly",  draw_comparison(results, "Malaria_test", numtrials , setting, 1))
  })
})

test_that("simple regression", {
  input_file <- "config/model_inputs_demo.xlsx"
  local({
    test_template(input_file)
    numtrials <- 5
    
    scenarios <-xlsx::read.xlsx(input_file, sheetName = "Scenarios")%>% 
      filter(!is.na(UniqueID)) %>%
      filter(sheet_TaskValues == "TaskValues_basic")
    # Run through the full scenario list.
    for (i in 1:nrow(scenarios)){
      cat(paste("Starting scenario",i))
      scenario <- scenarios$UniqueID[i]
      results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
      expect_true(all(!is.na(results)))
      filename <- paste("tests/results/regression/results_Ethiopia", scenario,".csv",sep="")
      filename_d <- paste("tests/results/regression/demog_Ethiopia", scenario,".csv",sep="")
      cat(paste("output file should be created as:", filename, sep=""))
      SaveSuiteResults(results, filename, scenario, 1)
      expect_true(file.exists(filename))
      result <- read.csv(filename)
      result <- result %>% 
        dplyr::arrange("Task_ID", "Scenario_ID", "Trial_num", "Run_num", "Year", "Month") %>%
        dplyr::filter(Trial_num <= 2) # reduce comparison (legacy results only have 2 trials)
      
      sorted_filename <- gsub("results_", "results_sorted_", filename)
      write.csv(result, sorted_filename)
      # assuming no data change the stochastic randomness, result should be the same using the default seed
      expect_snapshot_file(sorted_filename, compare=testthat::compare_file_text)
      # Check population 
      df <- SaveSuiteDemographics(results)
      df <- df %>%
        dplyr::group_by(Trial, Year, Age) %>%
        dplyr::summarize(Female = sum(Female), Male = sum(Male)) %>%
        dplyr::arrange(Trial, Year, Age)  %>%
        dplyr::filter(Trial <= 2) # reduce comparison (legacy results only have 2 trials)
      write.csv(df, filename_d, row.names = FALSE)
      expect_snapshot_file(filename_d, compare=testthat::compare_file_text)
    }
  })
  
})

test_that("pop matrix number match", {
  local(
    {
      input_file = "config/model_inputs_demo.xlsx"
      test_template(input_file)
      results <- RunExperiments(scenarioName = "BasicServices", trials = 2, debug = TRUE)
      computed_m <- pacehrh::ComputeApplicablePopulationMatrices(results)
      for (y in as.character(seq(2020, 2040))){
        for (trial in seq(1,2)){
          # Check Female only
          expect_equal(computed_m[[trial]]["women 30-39",y],
                       sum(results[[trial]][["Population"]][[y]]$Female[31:40]),
                       label= glue::glue("failed in y {y} and trial: {trial}"))
          # Check Male Only
          l <- length(results[[trial]][["Population"]][[y]]$Male)
          expect_equal(computed_m[[trial]]["men 18+",y],
                       sum(results[[trial]][["Population"]][[y]]$Male[19:l]),
                       label= glue::glue("failed in y {y} and trial: {trial}"))
          #Check Both
          expect_equal(computed_m[[trial]][["1-18",y]],
                       sum(results[[trial]][["Population"]][[y]]$Male[2:19] + results[[trial]][["Population"]][[y]]$Female[2:19]),
                       label= glue::glue("failed in y {y} and trial: {trial}"))
          
        }
      }
    }
  )
})
