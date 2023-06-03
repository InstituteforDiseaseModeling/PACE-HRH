library(pacehrh)

withr::local_dir("..")

test_that("Cadre processing: basic", {
  e <- pacehrh:::GPE
  local_vars("globalConfigLoaded", envir = e)
  e$globalConfigLoaded <- FALSE

  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()
  pacehrh::InitializeCadreRoles()

  scenario <- "MergedModel"

  pacehrh::SetGlobalStartEndYears(2025, 2055)

  pacehrh::SetRoundingLaw("none")

  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                            trials = 10)

  resultsDir <- "new_results"
  resultsFile <- file.path(resultsDir, "results.csv")

  if (!dir.exists(resultsDir)){
    dir.create(resultsDir)
  }

  pacehrh::SaveSuiteResults(results, resultsFile, scenario, "Run-1")

  testthat::expect_true(file.exists(resultsFile))
  DR <- pacehrh::SaveExtendedSuiteResults(results)

  testthat::expect_true(!is.null(DR))

  CA <- pacehrh::SaveCadreAllocations(DR)

  testthat::expect_true(!is.null(CA))

  stats <- pacehrh::ComputeSummaryStats(DR, CA)

  testthat::expect_true(!is.null(stats))
})

test_that("Post-processing: file list initialization", {
  e <- pacehrh:::GPE
  local_vars("globalConfigLoaded", envir = e)
  local_vars("traceState", envir = e)
  e$globalConfigLoaded <- FALSE

  testthat::expect_null(pacehrh:::.getFilesList())
  testthat::expect_null(pacehrh:::.getFilesList(dir = "notadirectory"))
  testthat::expect_null(pacehrh:::.getFilesList(files = vector(mode = "character", length = 0L)))

  resultsDir <- "one_result" # Directory should only contain one CSV file
  resultsFile <- file.path(resultsDir, "results.csv")

  testthat::expect_equal(length(dir(resultsDir)), 1)
  testthat::expect_true(setequal(
    pacehrh:::.getFilesList(files = resultsFile),
    pacehrh:::.getFilesList(dir = resultsDir)
  ))

  testthat::expect_true(TRUE)
})
