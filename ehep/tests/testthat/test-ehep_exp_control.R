library(ehep)

withr::local_dir("..")

test_that("Experiment control: basic read from Excel", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)

  if (exists("scenarios", envir = e)){
    if (!is.null(e$scenarios)){
      local_vars("scenarios", envir = e)
    }
  }

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  e$globalConfigLoaded <- TRUE # Fool the system into believing it has already processed the globalconfig.json file
  e$scenarios <- NULL

  ehep::InitializeScenarios()
  testthat::expect_true(!is.null(e$scenarios))

  scenarioName <- "NoChangeBaseline_NR"
  testthat::expect_true(scenarioName %in% e$scenarios$UniqueID)

  ehep::SaveBaseSettings(scenarioName)





  e$scenarios <- NULL
})
