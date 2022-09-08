library(ehep)

withr::local_dir("..")

# This test loads and validates a simplified version of the input population data.
test_that("Population configuration: basic population", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- ehep:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  pseq <- seq(10000, 0, -100)
  testthat::expect_equal(pop$female@values, pseq)
  testthat::expect_equal(pop$male@values, pseq)
})

test_that("Population configuration: confirm cleanup 1", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
})

.validInitPopulation <- function(pop) {
  return(TRUE)
}

test_that("Population configuration: InitializePopulation()", {
  e <- ehep:::GPE

  testthat::expect_equal(e$inputExcelFile, "./config/R Model Inputs.xlsx")
  testthat::expect_true(file.exists("globalconfig.json"))

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = e)

  testthat::expect_false(e$globalConfigLoaded)
  testthat::expect_null(e$initialPopulation)

  testthat::expect_invisible(ehep::InitializePopulation())

  testthat::expect_true(e$globalConfigLoaded)
  testthat::expect_true(!is.null(e$initialPopulation))
  testthat::expect_true(!is.null(e$populationLabels))

  testthat::expect_true(.validInitPopulation(e$initialPopulation))
})

test_that("Population configuration: check labels", {
  e <- ehep:::GPE

  testthat::expect_equal(e$inputExcelFile, "./config/R Model Inputs.xlsx")
  testthat::expect_true(file.exists("globalconfig.json"))

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = e)

  testthat::expect_false(e$globalConfigLoaded)
  testthat::expect_null(e$initialPopulation)

  testthat::expect_invisible(ehep::InitializePopulation())

  testthat::expect_true(e$globalConfigLoaded)
  testthat::expect_true(!is.null(e$initialPopulation))
  testthat::expect_true(!is.null(e$populationLabels))

  if (!is.null(e$populationLabels)){
    df <- e$populationLabels
    cols <- names(df)

    testthat::expect_equal(length(cols), 5)
    testthat::expect_true(all(cols %in% c("Labels", "Male", "Female", "Start", "End")))
  }
})

