library(pacehrh)

withr::local_dir("..")

test_that("Population rates configuration: Load and compute", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Population Rates Test Data.xlsx"

  withr::defer(pacehrh::Trace(originalTraceState))
  originalTraceState <- pacehrh::Trace(TRUE)

  popRates <- pacehrh:::loadPopulationChangeRates(sheetName = "newPopValues")
  testthat::expect_true(!is.null(popRates))

  # Check that the banded rates and full rates agree
  results <- sapply(popRates, function(r){
    if (!is.null(r$prt)){
      A <- pacehrh:::.generateExpansionMatrix(breaks = r$bandedRates$breaks)
      b <- r$bandedRates$changeRates
      x <- as.vector(A %*% b)
      ref <- r$fullRates$changeRates
      return(all(x == ref))
    } else {
      return(TRUE)
    }
  })

  testthat::expect_true(all(results))

  popRates <- pacehrh:::loadPopulationChangeRates(sheetName = "badPopValues")
  testthat::expect_true(!is.null(popRates))

  # Check that the banded rates and full rates agree
  results <- sapply(popRates, function(r){
    if (!is.null(r$prt)){
      A <- pacehrh:::.generateExpansionMatrix(breaks = r$bandedRates$breaks)
      b <- r$bandedRates$changeRates
      x <- as.vector(A %*% b)
      ref <- r$fullRates$changeRates
      return(all(x == ref))
    } else {
      return(TRUE)
    }
  })

  testthat::expect_true(all(results))

  # Sheet unusable - missing columns
  popRates <- pacehrh:::loadPopulationChangeRates(sheetName = "badPopValues_2")
  testthat::expect_true(is.null(popRates))

  # Sheet incomplete - no femaleFertility values
  popRates <- pacehrh:::loadPopulationChangeRates(sheetName = "badPopValues_3")
  testthat::expect_true(!is.null(popRates))

  n <- sapply(popRates, function(r){
    if (!is.null(r$prt)){
      return(1)
    } else {
      return(0)
    }
  })

  # Four slots in the structure, but only three of them have good data
  testthat::expect_true(length(n) == 4)
  testthat::expect_true(sum(n) == 3)
})
