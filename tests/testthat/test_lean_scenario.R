local_edition(3)
packages = c("dplyr","testthat", "vdiffr")
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}

library(pacehrh)
setwd("../..")
minimum_input_file <- "pacehrh/inst/extdata/model_inputs_template_lean.xlsx"
source("tests/testthat/test_per_age.R")

# Set up necessary steps for default minimum template
test_template <- function(input_file, rounding="", setting="annual"){
    dir.create("tests/results", showWarnings = FALSE)
    dir.create("tests/results/regression", showWarnings = FALSE)
    pacehrh::SetInputExcelFile(inputExcelFilePath = input_file)
    Trace(TRUE)
    InitializePopulation()
    InitializeScenarios()
    InitializeStochasticParameters()
    InitializeSeasonality()
    SetGlobalStartEndYears(start = 2020, end = 2040)
    if (rounding!=""){pacehrh::SetRoundingLaw(rounding)}
    pacehrh::SetPerAgeStats(setting)
    withr::defer_parent(unlink("tests/results", recursive = TRUE,  force = TRUE))
}

test_that("check_package_template", {
  # TODO:
  # There is currently warning about
  # tasks in the scenario offsets table are not used in task values sheets
  # We need to define the validate behavior in lint
  expect_true(pacehrh::CheckInputExcelFileFormat(minimum_input_file) %in% c(pacehrh:::.Success, pacehrh:::.warnProblemsFound))

})

test_that("check_user_template", {
  # TODO:
  # There is currently warning about
  # tasks in the scenario offsets table are not used in task values sheets
  # We need to define the validate behavior in lint
  input_file <- "config/model_inputs.xlsx"
  expect_true(pacehrh::CheckInputExcelFileFormat(input_file) %in% c(pacehrh:::.Success, pacehrh:::.warnProblemsFound))
  
})

test_that("model regression annual",{
  input_file <- "config/model_inputs.xlsx"
  local({
    setting = "annual"
    test_template(input_file, setting = setting)
    numtrials <- 2
    scenario <- "ComprehensiveModel"
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    # save results for selected tasks that are representative of seasonality
    expect_doppelganger("FH.MN.ANC.1",  draw_comparison(results, "FH.MN.ANC.1", numtrials , setting, 1))
    expect_doppelganger("FH.MN.D.3",  draw_comparison(results, "FH.MN.D.3", numtrials , setting, 1))
    expect_doppelganger("Record keeping",  draw_comparison(results, "Record keeping", numtrials , setting, 1))
  })
})

test_that("model regression monthly",{
  input_file <- "config/model_inputs.xlsx"
  local({
    setting = "monthly"
    test_template(input_file, setting = setting)
    numtrials <- 2
    scenario <- "ComprehensiveModel"
    results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
    expect_true(all(!is.na(results)))
    # save results for selected tasks that are representative of seasonality
    expect_doppelganger("FH.MN.ANC.1_monthly",  draw_comparison(results, "FH.MN.ANC.1", numtrials , setting, 1))
    expect_doppelganger("FH.MN.D.3_monthly",  draw_comparison(results, "FH.MN.D.3", numtrials , setting, 1))
    expect_doppelganger("Record keeping_monthly",  draw_comparison(results, "Record keeping", numtrials , setting, 1))
  })
})

test_that("simple regression", {
  local({
    test_template(minimum_input_file)
    numtrials <- 2
    
    # Run through the full scenario list.
    for (i in 1:nrow(pacehrh:::loadScenarios())){
      cat(paste("Starting scenario",i))
      scenario <- pacehrh:::loadScenarios()$UniqueID[i]
      geoname <- pacehrh:::loadScenarios()$Geography_dontedit[i]
      results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
      expect_true(all(!is.na(results)))
      filename <- paste("tests/results/regression/results_", geoname, scenario,".csv",sep="")
      filename_d <- paste("tests/results/regression/demog_", geoname, scenario,".csv",sep="")
      cat(paste("output file should be created as:", filename, sep=""))
      SaveSuiteResults(results, filename, scenario, 1)
      expect_true(file.exists(filename))
      result <- read.csv(filename)
      result <- result %>% dplyr::arrange("Task_ID", "Scenario_ID", "Trial_num", "Run_num", "Year", "Month")
      sorted_filename <- gsub("results_", "results_sorted_", filename)
      write.csv(result, sorted_filename)
      # assuming no data change the stochastic randomness, result should be the same using the default seed
      expect_snapshot_file(sorted_filename, compare=testthat::compare_file_text)
      # Check population 
      df <- SaveSuiteDemographics(results)
      df <- df %>%
        dplyr::group_by(Trial, Year, Age) %>%
        dplyr::summarize(Female = sum(Female), Male = sum(Male)) %>%
        dplyr::arrange(Trial, Year, Age)
      write.csv(df, filename_d, row.names = FALSE)
      expect_snapshot_file(filename_d, compare=testthat::compare_file_text)
    }
  })

})

test_that("pop matrix number match", {
  local(
    {
    results <- RunExperiments(scenarioName = "BasicModel", trials = 2, debug = TRUE)
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
