context("Test minimum template")
local_edition(3)
packages = c("dplyr","testthat")
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}

library(pacehrh)
setwd("../..")
minimum_input_file <- "pacehrh/inst/extdata/model_inputs_template_lean.xlsx"

# Set up necessary steps for default minimum template
test_template <- function(){
    dir.create("tests/results", showWarnings = FALSE)
    pacehrh:::setGlobalConfig(inputExcelFilePath = minimum_input_file)
    Trace(TRUE)
    InitializePopulation()
    InitializeScenarios()
    InitializeStochasticParameters()
    InitializeSeasonality()
    SetGlobalStartEndYears(start = 2020, end = 2040)
    withr::defer_parent(unlink("tests/results", recursive = TRUE,  force = TRUE))
}

test_that("check_template", {
  # TODO:
  # There is currently warning about
  # tasks in the scenario offsets table are not used in task values sheets
  # We need to define the validate behavior in lint
  expect_true(pacehrh::CheckInputExcelFileFormat(minimum_input_file) %in% c(pacehrh:::.Success, pacehrh:::.warnProblemsFound))

})

test_that("simple regression", {
  local({
    test_template()
    numtrials <- 2
    dir.create("tests/results/regression", showWarnings = FALSE)
    # Run through the full scenario list.
    for (i in 1:nrow(pacehrh:::GPE$scenarios)){
      cat(paste("Starting scenario",i))
      scenario <- pacehrh:::GPE$scenarios$UniqueID[i]
      geoname <- pacehrh:::GPE$scenarios$Geography_dontedit[i]
      results <- RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
      expect_true(all(!is.na(results)))
      filename <- paste("tests/results/regression/results_", geoname, scenario,".csv",sep="")
      cat(paste("output file should be created as:", filename, sep=""))
      SaveSuiteResults(results, filename, scenario, 1)
      expect_true(file.exists(filename))
      result <- read.csv(filename)
      result <- result %>% arrange("Task_ID", "Scenario_ID", "Trial_num", "Run_num", "Year", "Month")
      sorted_filename <- gsub("results_", "results_sorted_", filename)
      write.csv(result, sorted_filename)
      # assuming no data change the stochastic randomness, result should be the same using the default seed
      expect_snapshot_file(sorted_filename)
    }
  })

})
