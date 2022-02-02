library(ehep)

withr::local_dir("..")

# This test loads and validates stochasticity parameters
test_that("Stochasticity configuration: basic population", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pars <- ehep:::loadStochasticParameters(sheetName = "TEST_StochasticParms")

  testthat::expect_named(pars, c("Value", "p", "q"))
})

test_that("Stochasticity configuration: confirm cleanup 1", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
})

# This test loads and validates a simplified version of the input population data.
# test_that("Population configuration: basic population parameters", {
#   testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
#
#   e <- ehep:::GPE
#   local_vars("inputExcelFile", envir = e)
#
#   e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
#   popParms <- ehep:::loadPopulationChangeParameters(sheetName = "TEST_PopValues")
#
#   testthat::expect_true(!is.null(popParms))
#   testthat::expect_equal(length(popParms$initValues@values), length(popParms$changeRates@values))
#
#   testDeltas <- replicate(length(popParms$changeRates@values), 0.98)
#   testDeltas[2] <- 30
#   testthat::expect_equal(popParms$changeRates@values, testDeltas)
# })
#
# test_that("Population configuration: confirm cleanup 2", {
#   testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
# })
#
# .validInitPopulation <- function(pop) {
#   return(TRUE)
# }
#
# test_that("Population configuration: InitializePopulation()", {
#   e <- ehep:::GPE
#
#   testthat::expect_equal(e$inputExcelFile, "./config/R Model Inputs.xlsx")
#   testthat::expect_true(file.exists("globalconfig.json"))
#
#   local_vars("inputExcelFile", envir = e)
#   local_vars("initialPopulation", envir = e)
#   local_vars("populationChangeParameters", envir = e)
#   local_vars("globalConfigLoaded", envir = e)
#
#   testthat::expect_false(e$globalConfigLoaded)
#   testthat::expect_null(e$initialPopulation)
#   testthat::expect_null(e$populationChangeParameters)
#
#   testthat::expect_invisible(ehep::InitializePopulation())
#
#   testthat::expect_true(e$globalConfigLoaded)
#   testthat::expect_true(!is.null(e$initialPopulation))
#   testthat::expect_true(!is.null(e$populationChangeParameters))
#
#   testthat::expect_true(.validInitPopulation(e$initialPopulation))
# })

