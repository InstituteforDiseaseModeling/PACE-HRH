library(pacehrh)

withr::local_dir("..")

# This test loads and validates a simplified version of the task data.
test_that("Task configuration: basic data", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  tasks <- pacehrh:::loadTaskParameters(sheetName = "TEST_TaskValues")

  testthat::expect_true(!is.null(tasks))
  testthat::expect_equal(class(tasks), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(names(tasks[pacehrh:::.taskColumnNames]), pacehrh:::.taskColumnNames)
  testthat::expect_error(tasks["notacolumn"])

  # taskData$StartingRateInPop[is.na(taskData$StartingRateInPop)] <- 0
  # taskData$RateMultiplier[is.na(taskData$RateMultiplier)] <- 1
  # taskData$AnnualDeltaRatio[is.na(taskData$AnnualDeltaRatio)] <- 1
  # taskData$NumContactsPerUnit[is.na(taskData$NumContactsPerUnit)] <- 0
  # taskData$NumContactsAnnual[is.na(taskData$NumContactsAnnual)] <- 0
  # taskData$HoursPerWeek[is.na(taskData$HoursPerWeek)] <- 0
  # taskData$FTEratio[is.na(taskData$FTEratio)] <- 0
  #
  # computeMethod <- replicate(nrow(taskData), "TimePerTask")
  # computeMethod[taskData$FTEratio != 0] <- "TimeRatio"
  # computeMethod[taskData$HoursPerWeek != 0] <- "TimeAddedOn"
  # taskData$computeMethod <- computeMethod
  #
  # applyStochasticity <- replicate(nrow(taskData), TRUE)
  # applyStochasticity[taskData$StartingRateInPop == 1] <- FALSE
  # applyStochasticity[taskData$computeMethod != "TimePerTask"] <- FALSE
  # taskData$applyStochasticity <- applyStochasticity
})

test_that("Task configuration: confirm cleanup 1", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
})

test_that("Task configuration: invalid sheet name", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  tasks <- pacehrh:::loadTaskParameters(sheetName = "notasheet")

  testthat::expect_null(tasks)
})

test_that("Task configuration: invalid file name", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/notafile.xlsx"
  tasks <- pacehrh:::loadTaskParameters(sheetName = "TEST_TaskValues")

  testthat::expect_null(tasks)
})
