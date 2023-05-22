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
  pacehrh::InitializeCadreRoles()

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
  testthat::expect_warning(pacehrh::GetSuiteRates(results = NULL))

  rateCategory <- "femaleFertility"

  df <- pacehrh::GetSuiteRates(results, rateCategory = rateCategory)
  testthat::expect_true(!is.null(df))

  cols <- c("Label",
            "Trial",
            "Year",
            "Rate")

  testthat::expect_equal(names(df), cols)

  # Numeric comparison test
  trial <- 1

  for (label in unique(df$Label[!is.na(df$Label)])) {
    source <- results[[trial]][["PopulationRates"]][[rateCategory]][["ratesMatrix"]][label, ]
    dest <- df[df$Label == label & df$Trial == trial,]

    testthat::expect_equal(sum(source), sum(dest$Rate))
  }
})
