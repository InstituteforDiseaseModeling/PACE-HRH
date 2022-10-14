library(pacehrh)

withr::local_dir("..")

# This test loads and validates a simplified version of the input population data.
test_that("Population configuration: basic population", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- pacehrh:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  pseq <- seq(10000, 0, -100)
  testthat::expect_equal(pop$female@values, pseq)
  testthat::expect_equal(pop$male@values, pseq)
})

test_that("Population configuration: confirm cleanup 1", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
})

.validInitPopulation <- function(pop) {
  return(TRUE)
}

test_that("Population configuration: InitializePopulation()", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  testthat::expect_equal(e$inputExcelFile, "./config/model_inputs.xlsx")
  testthat::expect_true(file.exists("globalconfig.json"))

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = bve)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = e)

  testthat::expect_false(e$globalConfigLoaded)
  testthat::expect_null(e$initialPopulation)

  testthat::expect_invisible(pacehrh::InitializePopulation())

  testthat::expect_true(e$globalConfigLoaded)
  testthat::expect_true(!is.null(bve$initialPopulation))
  testthat::expect_true(!is.null(e$populationLabels))

  testthat::expect_true(.validInitPopulation(e$initialPopulation))
})

test_that("Population configuration: check labels", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  testthat::expect_equal(e$inputExcelFile, "./config/model_inputs.xlsx")
  testthat::expect_true(file.exists("globalconfig.json"))

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = bve)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = e)

  testthat::expect_false(e$globalConfigLoaded)
  testthat::expect_null(e$initialPopulation)

  testthat::expect_invisible(pacehrh::InitializePopulation())

  testthat::expect_true(e$globalConfigLoaded)
  testthat::expect_true(!is.null(bve$initialPopulation))
  testthat::expect_true(!is.null(e$populationLabels))

  if (!is.null(e$populationLabels)){
    df <- e$populationLabels
    cols <- names(df)

    testthat::expect_equal(length(cols), 5)
    testthat::expect_true(all(cols %in% c("Labels", "Male", "Female", "Start", "End")))
  }
})

