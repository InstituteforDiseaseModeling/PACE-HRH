library(pacehrh)

withr::local_dir("..")

test_that("Scenario configuration: basic read from Excel", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  # Set input file, and cheat the system into thinking the global configuration
  # is already loaded
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$globalConfigLoaded <- TRUE
  e$scenarios <- NULL

  testthat::expect_true(is.null(e$scenarios))
  pacehrh::InitializeScenarios()
  testthat::expect_false(is.null(e$scenarios))
  testthat::expect_equal(class(e$scenarios), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(names(e$scenarios[pacehrh:::.scenarioColumnNames]), pacehrh:::.scenarioColumnNames)
})

test_that("Scenario configuration: bad sheet name", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  # Set input file, and cheat the system into thinking the global configuration
  # is already loaded
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$globalConfigLoaded <- TRUE
  e$scenarios <- NULL

  testthat::expect_true(is.null(e$scenarios))
  pacehrh::InitializeScenarios(sheetName = "notasheet")
  testthat::expect_true(is.null(e$scenarios))
})

test_that("Scenario configuration: bad Excel file", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  # Set input file, and cheat the system into thinking the global configuration
  # is already loaded
  e$inputExcelFile <- "./simple_config/notafile.xlsx"
  e$globalConfigLoaded <- TRUE
  e$scenarios <- NULL

  # This should fail (ie not return a scenario table) because the filename is bogus
  testthat::expect_true(is.null(e$scenarios))
  pacehrh::InitializeScenarios()
  testthat::expect_true(is.null(e$scenarios))
})

test_that("Scenario configuration: non-Excel configuration", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)

  # Set input file, and cheat the system into thinking the global configuration
  # is already loaded
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$globalConfigLoaded <- TRUE
  e$scenarios <- NULL

  testthat::expect_true(is.null(e$scenarios))

  # Create an empty scenarios table
  pacehrh::InitializeScenarios(loadFromExcel = FALSE)

  testthat::expect_false(is.null(e$scenarios))
  testthat::expect_equal(class(e$scenarios), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(names(e$scenarios[pacehrh:::.scenarioColumnNames]), pacehrh:::.scenarioColumnNames)
  testthat::expect_equal(nrow(e$scenarios), 0)
})

test_that("Scenario configuration: missing columns", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
  
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)
  local_vars("traceState", envir = e)
  
  # Set input file
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL
  
  # The TEST_Scenarios_1 sheet is missing two required columns
  testthat::expect_true(is.null(e$scenarios))
  testthat::expect_warning(pacehrh::InitializeScenarios(sheetName = "TEST_Scenarios_1"),
                           regexp = "Missing required columns")
  testthat::expect_true(is.null(e$scenarios))
})

test_that("Scenario configuration: mis-typed columns", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
  
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)
  local_vars("traceState", envir = e)
  
  # Set input file
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL
  
  # The TEST_Scenarios_2 sheet has two required columns with incorrect types
  testthat::expect_true(is.null(e$scenarios))
  testthat::expect_warning(pacehrh::InitializeScenarios(sheetName = "TEST_Scenarios_2"),
                           regexp = "Columns with incorrect types")
  testthat::expect_true(is.null(e$scenarios))
})

test_that("Scenario configuration: no optional columns", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
  
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)
  local_vars("traceState", envir = e)
  
  # Set input file
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL
  
  # The TEST_Scenarios_3 sheet has no optional columns
  testthat::expect_true(is.null(e$scenarios))
  pacehrh::InitializeScenarios(sheetName = "TEST_Scenarios_3")
  testthat::expect_true(!is.null(e$scenarios))
  
  testthat::expect_true(setequal(pacehrh:::.scenarioMetaData$cols, names(e$scenarios)))
})

test_that("Scenario configuration: optional columns, wrong type", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
  
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)
  local_vars("traceState", envir = e)
  
  # Set input file
  pacehrh::SetInputExcelFile("./simple_config/Test Inputs.xlsx")
  e$scenarios <- NULL
  
  # The TEST_Scenarios_4 sheet has an optional column, but the type is wrong
  testthat::expect_true(is.null(e$scenarios))
  testthat::expect_warning(pacehrh::InitializeScenarios(sheetName = "TEST_Scenarios_4"),
                           regexp = "Columns with incorrect types")
  testthat::expect_true(is.null(e$scenarios))
})
