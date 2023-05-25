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
