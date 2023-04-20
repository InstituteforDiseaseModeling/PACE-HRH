testthat::local_edition(3)
setwd("../..")
source("ValidateInput.R")

# test validate no rules
test_that("Validation no rules", {
  # test should fail if no rule is defined for the sheet
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    logdir <- tempdir()
    testthat::expect_error(ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation.xlsx",
                                                         logFile = lf,
                                                         outputDir = logdir,
                                                         optional_sheets = list(Total_Pop="rules_TotalPop2.yaml")), "Unable to check sheet")
  })
})
# test validation capture
test_that("Validation capture", {
  local({
    lf <-log_open(file_name = "tests/log.txt", show_notes = FALSE)
    errCode <- .errValidationRuleFailed
    # In this example rules Total_diarrhea does not sum to 1, this is a table-wise error not row-wise error
    # The error is captured and verified in _snaps/pacehrh_lint.md
    
    logdir <- tempdir()
    # skip_if(.Platform$OS.type != "windows")
    testthat::expect_equal(errCode, ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation.xlsx",
                                                                  logFile = lf,
                                                                  outputDir = logdir,
                                                               optional_sheets = list(SeasonalityCurves="rules_SeasonalityCurves.yaml")))
  })
})
# test validation success
test_that("Validation capture Success", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    errCode <- .Success
  
    logdir <- tempdir()
    testthat::expect_equal(errCode, ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation2.xlsx",
                                                                  logFile = lf,
                                                                  outputDir = logdir,
                                                                optional_sheets = list(SeasonalityCurves="rules_SeasonalityCurves.yaml")))
  })
})

# test validation row-wise
test_that("Validation capture Success", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    errCode <- .Success
    # In this example, rule is set as warning level so violation is not causing error but output files will be available
    logdir <- tempdir()
    testthat::expect_equal(errCode, ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation3.xlsx",
                                                                  logFile = lf,
                                                                  outputDir = logdir,
                                                                optional_sheets = list(StochasticParameters="rules_StochasticParameters.yaml")))
    result_file <- paste(logdir, "StochasticParameters_info_violation_Large_SD_value.csv", sep="/")
    testthat::expect_true(file.exists(result_file))
    violations <- read.csv(result_file)
    # search for the row that fails as expected
    violating_row <- violations %>%
      dplyr::filter(Value == "Seasonality ratio to mean")
    testthat::expect_equal(nrow(violating_row ), 1)
  })
})

# test validation success
test_that("Validation Scenarios custom name", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    errCode <- .Success
    logdir <- tempdir()
    testthat::expect_equal(errCode, ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation_custom.xlsx",
                                                                  logFile =lf,
                                                                  outputDir = logdir))
  })
})

# test validation missing columns specified in the rule (sheet:SeasonalityCurves)
test_that("Validation Scenarios custom name missing col", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    logdir <- tempdir()
    testthat::expect_output(ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation_custom_missing_col.xlsx",
                                                          logFile = lf,
                                                          outputDir = logdir), regexp = "some rules cannot be applied to sheet: SeasonalityCurves")
  })
})

# test validation missing sheet specified in scenarios
test_that("Validation Scenarios custom name missing col", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    logdir <- tempdir()
    testthat::expect_error(ValidateInputExcelFileContent(inputFile = "tests/testthat/sample_config/Test_validation_custom_missing_sheet.xlsx",
                                                         logFile = lf,
                                                          outputDir = logdir), regexp = 'sheet: "Task2" does not exist')
  })
})

### Setup some cadre data for testing 
get_cadre_roles_default<- function(){
  inputFile = "tests/testthat/sample_config/cadre_inputs.xlsx"
  scenarios <- read_xlsx(inputFile, sheet="Scenarios")
  df <-  read_xlsx(inputFile, sheet="CadreRoles") %>% 
    filter(ScenarioID == "ComprehensiveModel") %>%
    inner_join(scenarios, by=c('ScenarioID'='UniqueID')) %>%
    select(ScenarioID, RoleID, StartYear, EndYear, sheet_Cadre) %>%
    mutate(StartYear = as.numeric(StartYear), EndYear = as.numeric(EndYear))
  return (list(df=df, inputFile=inputFile))
}

# test .cadre_check_scenarios

test_that("Check rule2 execution", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    logdir <- tempdir()
    testdata <- get_cadre_roles_default()
    # create some invalid endYear
    df <- testdata$df %>% mutate (EndYear = EndYear +3)
    .cadre_check_scenarios(df, testdata$inputFile, logdir)
    testthat::expect_true(file.exists(file.path(logdir, "violation_endyear_not_in_startyear_Cadres_Comprehensive.csv")))
    
  })
})

test_that("Check rule3 execution", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    logdir <- tempdir()
    testdata <- get_cadre_roles_default()
    df <- testdata$df
    # create some invalid startYear that does not exist
    df[nrow(df), "StartYear"] = 2023
    .cadre_check_scenarios(df, testdata$inputFile, logdir)
    testthat::expect_true(file.exists(file.path(logdir, "violation_startyear_not_in_header_Cadres_Comprehensive.csv")))
    violation <- read.csv(file.path(logdir, "violation_startyear_not_in_header_Cadres_Comprehensive.csv"))
    testthat::expect_true("2023" %in% violation$StartYear)
  })
})

test_that("Check rule4 execution", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    logdir <- tempdir()
    testdata <- get_cadre_roles_default()
    df <- testdata$df
    # We know that The min EndYear does not appear in the last Bracket so purposely use this to test
    df[which(df$EndYear==min(df$EndYear, na.rm=TRUE)), "EndYear"] = NA
    .cadre_check_scenarios(df, testdata$inputFile, logdir)
    testthat::expect_true(file.exists(file.path(logdir, "violation_startyear_max_missing_Cadres_Comprehensive.csv")))
  })
})

test_that("Check rule5 execution", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    logdir <- tempdir()
    testdata <- get_cadre_roles_default()
    df <- testdata$df
    #  We know that The min EndYear does not appear in the last Bracket so purposely use this to test
    df[which(df$EndYear==min(df$EndYear, na.rm=TRUE)), "EndYear"] = 2034
    .cadre_check_scenarios(df, testdata$inputFile, logdir)
    testthat::expect_true(file.exists(file.path(logdir, "roles_missing_years_Cadres_Comprehensive.csv")))
  })
})

test_that("Check rule6 execution", {
  local({
    lf <- log_open(file_name = "tests/log.txt", show_notes = FALSE)
    logdir <- tempdir()
    testdata <- get_cadre_roles_default()
    df <- testdata$df
    #  Change the RoleID to make some definition non-existent
    df[which(df$RoleID=="HEW1"), "RoleID"] = "HEW3"
    .cadre_check_scenarios(df, testdata$inputFile, logdir)
    testthat::expect_true(file.exists(file.path(logdir, "no_definition_in_cadreroles_Cadres_Comprehensive.csv")))
    violation <- read.csv(file.path(logdir, "no_definition_in_cadreroles_Cadres_Comprehensive.csv"))
    testthat::expect_true("HEW1" %in% violation$RoleID)
  })
})

