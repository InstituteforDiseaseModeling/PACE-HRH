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
  pacehrh::SetInputExcelFile("./bad_config/model_inputs-bad_scenarios_sheet.xlsx")
  e$scenarios <- NULL
  e$globalConfigLoaded <- TRUE

  pacehrh::Trace(TRUE)

  testthat::expect_null(e$scenarios)
  testthat::expect_true(e$traceState)

  testthat::expect_snapshot(pacehrh:::.checkScenarios(autoCorrect = FALSE))
  testthat::expect_null(e$scenarios)

  testthat::expect_snapshot(pacehrh:::.checkScenarios(autoCorrect = TRUE))
  testthat::expect_null(e$scenarios)

  pacehrh::SetInputExcelFile("./bad_config/model_inputs-good_scenarios_sheet.xlsx")
  e$scenarios <- NULL

  testthat::expect_snapshot(pacehrh:::.checkScenarios(autoCorrect = TRUE))
  testthat::expect_true(!is.null(e$scenarios))

  testthat::expect_true(pacehrh:::.checkScenarios(autoCorrect = TRUE))
})
