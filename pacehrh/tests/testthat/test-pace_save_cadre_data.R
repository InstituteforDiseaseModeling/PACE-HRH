library(pacehrh)
library(data.table)

withr::local_dir("..")

test_that("Save cadre data", {
  gpe <- pacehrh:::GPE
  local_vars("globalConfigLoaded", envir = gpe)

  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()
  pacehrh::InitializeCadreRoles()

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

  testthat::expect_true(!is.null(results))

  filename <- "cadre_data.csv"
  run <- "the_run"

  # SaveCadreOverheadData without a filename just returns a data table
  testthat::expect_false(file.exists(filename))
  cadreData <- pacehrh::SaveCadreOverheadData()
  testthat::expect_true(!is.null(cadreData))
  testthat::expect_false(file.exists(filename))

  # SaveCadreOverheadData with a filename returns a data table and writes the output to disk
  withr::defer(file.remove(filename))
  cadreData <- pacehrh::SaveCadreOverheadData(filepath = filename, run = run)
  testthat::expect_true(file.exists(filename))

  # Check that the column names on the data table are correct
  columns <- c("Scenario_ID", "Role_ID", "Year", "Run", "OverheadTime")
  testthat::expect_equal(names(cadreData), columns)

  # Read the saved data from disk
  cadreDataRecalled <- data.table::fread(file = filename)
  testthat::expect_true(!is.null(cadreDataRecalled))
  testthat::expect_equal(names(cadreDataRecalled), columns)
})
