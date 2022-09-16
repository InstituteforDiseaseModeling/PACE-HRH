library(ehep)

withr::local_dir("..")

test_that("Population rates configuration: Load and compute", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Population Rates Test Data.xlsx"

  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  popRates <- ehep:::loadPopulationChangeRates(sheetName = "newPopValues")
  testthat::expect_true(!is.null(popRates))

  # Check that the banded rates and full rates agree
  results <- sapply(popRates, function(r){
    if (!is.null(r$prt)){
      A <- ehep:::.generateExpansionMatrix(breaks = r$bandedRates$breaks)
      b <- r$bandedRates$changeRates
      x <- as.vector(A %*% b)
      ref <- r$fullRates$changeRates
      return(all(x == ref))
    } else {
      return(TRUE)
    }
  })

  testthat::expect_true(all(results))

  popRates <- ehep:::loadPopulationChangeRates(sheetName = "badPopValues")
  testthat::expect_true(!is.null(popRates))

  # Check that the banded rates and full rates agree
  results <- sapply(popRates, function(r){
    if (!is.null(r$prt)){
      A <- ehep:::.generateExpansionMatrix(breaks = r$bandedRates$breaks)
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
  popRates <- ehep:::loadPopulationChangeRates(sheetName = "badPopValues_2")
  testthat::expect_true(is.null(popRates))

  # Sheet incomplete - no femaleFertility values
  popRates <- ehep:::loadPopulationChangeRates(sheetName = "badPopValues_3")
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
