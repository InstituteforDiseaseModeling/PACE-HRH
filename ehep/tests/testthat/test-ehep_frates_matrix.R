library(ehep)

withr::local_dir("..")

test_that("Fertility rates matrix: basic", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  pars <- ehep:::loadStochasticParameters(sheetName = "TEST_StochasticParms")
  pcp <- ehep:::loadPopulationChangeParameters(sheetName = "TEST_PopValues")
  years <- ehep:::GPE$years

  testthat::expect_named(pcp, c("initValues", "changeRates"))



  retval <- ehep:::.generateFertilityRatesMatrix(pars, years, pcp)

  print(retval)
})
