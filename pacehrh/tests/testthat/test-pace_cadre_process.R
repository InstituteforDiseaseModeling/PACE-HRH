library(pacehrh)
library(data.table)

withr::local_dir("..")

test_that("Cadre computations", {
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

  srFileName <- "_SR.csv"
  caFileName <- "_CA.scv"
  runName <- "THX-1138"

  # Generate and save extended suite results. Check that the results that get
  # saved to disk can be read back into a form (almost) equal to the original
  # data.
  testthat::expect_false(file.exists(srFileName))
  withr::defer(unlink(srFileName))
  srData <- pacehrh::SaveExtendedSuiteResults(results, filepath = srFileName, run = runName)

  testthat::expect_true(!is.null(srData))
  testthat::expect_true(file.exists(srFileName))

  srDataFromCsv <- data.table::fread(srFileName)
  testthat::expect_true(isTRUE(all.equal(srData, srDataFromCsv)))

  # Generate and save cadre allocation results. Check that the results that get
  # saved to disk can be read back into a form (almost) equal to the original
  # data.
  testthat::expect_false(file.exists(caFileName))
  withr::defer(unlink(caFileName))
  caData <- pacehrh::SaveCadreAllocations(srData, filepath = caFileName)

  testthat::expect_true(!is.null(caData))
  testthat::expect_true(file.exists(caFileName))

  caDataFromCsv <- data.table::fread(caFileName)
  testthat::expect_true(isTRUE(all.equal(caData, caDataFromCsv)))

  # Call SaveCadreAllocations() using the data read back from CSV. We've already
  # established that the data read back from CSV is (almost) the same as the
  # in-memory data. This test confirms that calling SaveCadreAllocations() with
  # the data read from CSV doesn't crash SaveCadreAllocations() and returns the
  # same results.
  caData_2 <- pacehrh::SaveCadreAllocations(srDataFromCsv)
  testthat::expect_true(!is.null(caData_2))
  testthat::expect_true(isTRUE(all.equal(caData, caData_2)))

  # Compute summary stats using both the in-memory and read-from-CSV data.
  summaryStats <- pacehrh::ComputeSummaryStats(srData, caData)
  testthat::expect_true(!is.null(summaryStats))
  summaryStatsFromCsv <- pacehrh::ComputeSummaryStats(srDataFromCsv, caDataFromCsv)
  testthat::expect_true(!is.null(summaryStatsFromCsv))
  testthat::expect_true(isTRUE(all.equal(summaryStats, summaryStatsFromCsv)))
})

test_that("Cadre computations - bad cadre task sheet", {
  gpe <- pacehrh:::GPE
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  local_vars("inputExcelFile", envir = GPE)
  local_vars("globalConfigLoaded", envir = GPE)
  local_vars("scenarios", envir = GPE)

  pacehrh::SetInputExcelFile("./bad_config/model_inputs_bad_cadre.xlsx")

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

  withr::defer(pacehrh::Trace(originalTraceState))
  originalTraceState <- pacehrh::Trace(TRUE)

  # The code should generate a message because the cadre task sheet can't be
  # loaded. This error doesn't stop the experiments from running, but it will
  # prevent later computation of cadre allocations.
  testthat::expect_snapshot({
    results <-
      pacehrh::RunExperiments(scenarioName = scenario,
                              trials = nTrials)
  })

  testthat::expect_true(!is.null(results))

  srData <- pacehrh::SaveExtendedSuiteResults(results, run = "runName")
  testthat::expect_true(!is.null(srData))

  testthat::expect_message({
    caData <- pacehrh::SaveCadreAllocations(srData)
  },
  regexp = "No task-to-cadre allocation data")

  testthat::expect_true(is.null(caData))
})

test_that("check cadre compute", {
  original_years <- BVE$years# Store the original value
  on.exit(BVE$years <- original_years)  # Ensure original value is restored after the test
  BVE$years <- seq(2020, 2030)  # Set the mock value
  scenario <- list(UniqueID = 1, WeeksPerYr=52)
  roles <- data.frame(ScenarioID = c(1,1), RoleID = c("FH1", "FH2"), StartYear = c(2020, 2035), EndYear = c(2030, NA), OverheadHoursPerWeek = c(40,40))
  expect_warning(output <- computeCadreData(scenario, roles), "FH2 has start year after the simulation end year")
  overhead <- data.frame(output$annualOverheads)
  included_role <- overhead[overhead$Role == "FH1", ]
  excluded_role <- overhead[overhead$Role == "FH2", ]
  testthat::expect_true(all(excluded_role == 0, na.rm = TRUE))
  testthat::expect_true(all(included_role == 40*52*60, na.rm = TRUE))
})
