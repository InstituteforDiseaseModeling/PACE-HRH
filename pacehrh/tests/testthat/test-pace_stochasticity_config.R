library(pacehrh)

withr::local_dir("..")

# This test loads and validates stochasticity parameters
test_that("Stochasticity configuration: basic", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  gpe <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = gpe)

  gpe$inputExcelFile <- "./simple_config/model_inputs.xlsx"
  pars <- pacehrh:::loadStochasticParameters(stochasticParametersSheetName = "TEST_StochasticParms")

  testthat::expect_named(pars, c("Value", "p", "q"))

  # Tests for loading the change rate limit values
  gpe$inputExcelFile <- "./config/model_inputs.xlsx"

  testthat::expect_warning(
    {
      limits <- pacehrh:::loadChangeRateLimits(changeRateLimitsSheetName = "notasheet")
      testthat::expect_true(is.null(limits))
    },
    regexp = "Could not read change rate limits sheet"
  )

  limits <- pacehrh:::loadChangeRateLimits()
  testthat::expect_true(!is.null(limits))

  gpe$inputExcelFile <- "./simple_config/model_inputs.xlsx"
  limits <- pacehrh:::loadChangeRateLimits(changeRateLimitsSheetName = "TEST_RateLimits_1")
  testthat::expect_true(!is.null(limits))

  # Should generate a "missing required columns" warning
  testthat::expect_snapshot(
    limits <- pacehrh:::loadChangeRateLimits(changeRateLimitsSheetName = "TEST_RateLimits_2")
  )
  testthat::expect_true(is.null(limits))
})

test_that("Stochasticity configuration: confirm cleanup 1", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
})
