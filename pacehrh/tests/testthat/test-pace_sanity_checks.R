library(pacehrh)

withr::local_dir("..")

test_that("Sanity checks: scenario check", {

  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("traceState", envir = e)
  local_vars("scenarios", envir = e)

  # Set input file, and null out the previous scenarios table
  pacehrh::SetInputExcelFile("./bad_config/model_inputs - bad scenarios sheet.xlsx")
  e$scenarios <- NULL

  pacehrh::Trace(TRUE)

  testthat::expect_null(e$scenarios)
  result <- pacehrh:::.checkScenarios(autoCorrect = FALSE)
  testthat::expect_false(result)
  testthat::expect_null(e$scenarios)

  result <- pacehrh:::.checkScenarios(autoCorrect = TRUE)


})
