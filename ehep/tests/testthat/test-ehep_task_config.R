library(ehep)

withr::local_dir("..")

# This test loads and validates a simplified version of the task data.
test_that("Task configuration: basic data", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  tasks <- ehep:::loadTaskParameters(sheetName = "TEST_TaskValues")

  testthat::expect_true(!is.null(tasks))
  testthat::expect_equal(class(tasks), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(names(tasks[ehep:::.taskColumnNames]), ehep:::.taskColumnNames)
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

# test_that("Population configuration: confirm cleanup 1", {
#   testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
# })
#
# # This test loads and validates a simplified version of the input population data.
# test_that("Population configuration: basic population parameters", {
#   testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
#
#   e <- ehep:::GPE
#   local_vars("inputExcelFile", envir = e)
#
#   e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
#   popParms <- ehep:::loadPopulationChangeParameters(sheetName = "TEST_PopValues")
#
#   testthat::expect_true(!is.null(popParms))
#   testthat::expect_equal(length(popParms$initValues@values), length(popParms$changeRates@values))
#
#   testDeltas <- replicate(length(popParms$changeRates@values), 0.98)
#   testDeltas[2] <- 30
#   testthat::expect_equal(popParms$changeRates@values, testDeltas)
# })
#
# test_that("Population configuration: confirm cleanup 2", {
#   testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
# })
#
# .validInitPopulation <- function(pop) {
#   return(TRUE)
# }
#
# test_that("Population configuration: InitializePopulation()", {
#   e <- ehep:::GPE
#
#   testthat::expect_equal(e$inputExcelFile, "./config/R Model Inputs.xlsx")
#   testthat::expect_true(file.exists("globalconfig.json"))
#
#   local_vars("inputExcelFile", envir = e)
#   local_vars("initialPopulation", envir = e)
#   local_vars("populationChangeParameters", envir = e)
#   local_vars("globalConfigLoaded", envir = e)
#
#   testthat::expect_false(e$globalConfigLoaded)
#   testthat::expect_null(e$initialPopulation)
#   testthat::expect_null(e$populationChangeParameters)
#
#   testthat::expect_invisible(ehep::InitializePopulation())
#
#   testthat::expect_true(e$globalConfigLoaded)
#   testthat::expect_true(!is.null(e$initialPopulation))
#   testthat::expect_true(!is.null(e$populationChangeParameters))
#
#   testthat::expect_true(.validInitPopulation(e$initialPopulation))
# })
#
