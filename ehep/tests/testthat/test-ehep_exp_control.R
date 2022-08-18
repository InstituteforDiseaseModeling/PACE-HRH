library(ehep)

withr::local_dir("..")

# Test that the correct sheets are read, based on the sheet names in the
# scenarios record.

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

  if (exists("populationChangeParameters", envir = e)){
    if (!is.null(e$populationChangeParameters)){
      local_vars("populationChangeParameters", envir = e)
    }
  }

  if (exists("seasonalityCurves", envir = e)){
    if (!is.null(e$seasonalityCurves)){
      local_vars("seasonalityCurves", envir = e)
    }
  }


  ehep:::setGlobalConfig(inputExcelFilePath = "./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL

  ehep::InitializeScenarios()
  testthat::expect_true(!is.null(e$scenarios))

  scenarioName <- "TEST_CustomSheets_1"
  assertthat::assert_that(scenarioName %in% e$scenarios$UniqueID)

  result <- ehep::SaveBaseSettings(scenarioName)

  testthat::expect_true(!is.null(result))
  testthat::expect_true(result$UniqueID == scenarioName)

  print("TBD TBD TBD")

  # print(ehep:::GPE$taskData)
  # print(ehep:::BVE$taskParameters)
  #
  # print(ehep:::GPE$populationChangeParameters)
  # print(ehep:::BVE$populationChangeParameters)
  #
  # print(ehep:::GPE$seasonalityCurves)

  e$scenarios <- NULL
  e$populationChangeParameters <- NULL
  e$seasonalityCurves <- NULL
})
