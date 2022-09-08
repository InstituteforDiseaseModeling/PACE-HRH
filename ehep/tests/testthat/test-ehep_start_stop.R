library(ehep)

withr::local_dir("..")

test_that("Full suite: basic", {
  ehep::InitializePopulation()
  ehep::InitializeHealthcareTasks()
  ehep::InitializeScenarios()
  ehep::InitializeStochasticParameters()
  ehep::InitializeSeasonality()

  # Make sure to use a scenario that has seasonality results!
  scenario <- "MergedModel"
  nTrials <- 5
  startYear <- 2025
  endYear <- 2050
  nMonths <- 12 * length(startYear:endYear)

  ehep::SetGlobalStartEndYears(startYear, endYear)

  results <-
    ehep::RunExperiments(scenarioName = scenario,
                         trials = nTrials)

  # Check that the results structure elements are the correct size and number
  testthat::expect_true(!is.null(results))
  testthat::expect_true("SeasonalityResults" %in% names(results[[1]]))
  testthat::expect_equal(length(results), nTrials)
  testthat::expect_true(setequal(names(results[[1]]$Population),as.character(startYear:endYear)))
  testthat::expect_equal(length(results[[1]]$SeasonalityResults[[1]]$Time), nMonths)

  # ---------------------------------
  ehep::SaveSuiteResults(results, "results.csv", scenario, 1)
  csvResults <- data.table::fread(file = "results.csv")

  # Test that the saved CSV has the correct number of rows and columns
  nTasks <- length(results[[1]]$SeasonalityResults)
  testthat::expect_equal(nrow(csvResults), (nTasks * nMonths * nTrials))

  csvCols <- c("Task_ID",
               "Scenario_ID",
               "Trial_num",
               "Run_num",
               "Year",
               "Month",
               "Num_services",
               "Service_time",
               "Health_benefit")

  testthat::expect_true(setequal(names(csvResults), csvCols))
})

