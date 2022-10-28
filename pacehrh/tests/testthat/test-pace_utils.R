library(pacehrh)

withr::local_dir("..")

test_that("Utilities: is.blank", {
  testthat::expect_true(pacehrh:::is.blank(""))
  testthat::expect_true(pacehrh:::is.blank(NULL))
  testthat::expect_true(pacehrh:::is.blank(NA))
  testthat::expect_false(pacehrh:::is.blank("notablank"))
  testthat::expect_false(pacehrh:::is.blank(0))
  testthat::expect_false(pacehrh:::is.blank(TRUE))
})

test_that("Utilities: GetSuiteRates", {
  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()

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


  testthat::expect_warning(pacehrh::GetSuiteRates(results, rateCategory = "notacategory"))

  df <- pacehrh::GetSuiteRates(results, rateCategory = "femaleFertility")
  testthat::expect_true(!is.null(df))

  cols <- c("Label",
            "Trial",
            "Year",
            "Rate")

  testthat::expect_equal(names(df), cols)
})
