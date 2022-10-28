library(pacehrh)

withr::local_dir("..")

test_that("Full suite: basic", {
  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()

  # Make sure to use a scenario that has seasonality results!
  scenario <- "MergedModel"
  nTrials <- 5
  startYear <- 2025
  endYear <- 2050
  nMonths <- 12 * length(startYear:endYear)

  shoulderYears <- pacehrh:::GPE$shoulderYears

  pacehrh::SetGlobalStartEndYears(startYear, endYear)

  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                         trials = nTrials)

  # Check that the results structure elements are the correct size and number
  testthat::expect_true(!is.null(results))
  testthat::expect_true("SeasonalityResults" %in% names(results[[1]]))
  testthat::expect_equal(length(results), nTrials)
  testthat::expect_true(setequal(names(results[[1]]$Population),as.character(startYear:(endYear+shoulderYears))))
  testthat::expect_equal(length(results[[1]]$SeasonalityResults[[1]]$Time), nMonths)

  # ---------------------------------
  pacehrh::SaveSuiteResults(results, "results.csv", scenario, 1)
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

  # ---------------------------------
  # Strip "SeasonalityResults" from the results structure to provoke short
  # format report

  results2 <- lapply(results, function(r){
    r$SeasonalityResults <- NULL
    return(r)
  })

  pacehrh::SaveSuiteResults(results2, "results2.csv", scenario, "Run-1")
  csvResults <- data.table::fread(file = "results2.csv")

  # Test that the saved CSV has the correct number of rows and columns
  nTasks <- NROW(results[[1]]$AnnualTimes)
  testthat::expect_equal(nrow(csvResults), (nTasks * (length(startYear:endYear) + shoulderYears) * nTrials))
  testthat::expect_true(setequal(names(csvResults), csvCols))

  # ---------------------------------
  # Save single result

  outdf <- pacehrh::SaveResults(results, "one_result.csv", scenario, trial = 2, "Run-1")

  testthat::expect_equal(nrow(outdf), (nTasks * (length(startYear:endYear) + shoulderYears)))
  testthat::expect_true(setequal(names(outdf), csvCols))

  # ---------------------------------
  # Test population info

  outdf <- pacehrh::SaveSuiteDemographics(results)

  csvCols <- c("Trial",
               "Year",
               "AgeBucket",
               "Age",
               "Female",
               "Male")

  testthat::expect_equal(nrow(outdf), (length(pacehrh:::GPE$ages) * (length(startYear:endYear) + shoulderYears) * nTrials))
  testthat::expect_true(setequal(names(outdf), csvCols))
})

