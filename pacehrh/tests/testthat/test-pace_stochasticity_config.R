library(pacehrh)

withr::local_dir("..")

# This test loads and validates stochasticity parameters
test_that("Stochasticity configuration: basic population", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/model_inputs.xlsx"
  pars <- pacehrh:::loadStochasticParameters(sheetName = "TEST_StochasticParms")

  testthat::expect_named(pars, c("Value", "p", "q"))
})

test_that("Stochasticity configuration: confirm cleanup 1", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
})
