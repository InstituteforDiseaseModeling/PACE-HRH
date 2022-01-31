library(ehep)

withr::local_dir("..")
e <- ehep:::GPE
local_vars("globalDebug", envir = e)
GPE$globalDebug <- TRUE

test_that("explodeMortalityRates: basic", {
  refFemale <- c(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5,
                 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7)

  refMale <- c(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5,
               6, 6, 6, 6, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
               8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
               8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
               8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8)

  results <- ehep:::explodeMortalityRates(c(1,2,3,4,5,6,7,8))

  testthat::expect_true(!is.null(results))
  testthat::expect_named(results, c("Female", "Male"))
  testthat::expect_equal(results$Female, refFemale)
  testthat::expect_equal(results$Male, refMale)
})

test_that("explodeFertilityRates: basic", {
  refFemale <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
                 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5,
                 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  results <- ehep:::explodeFertilityRates(c(1,2,3,4,5,6,7))

  testthat::expect_true(!is.null(results))
  testthat::expect_named(results, c("Female", "Male"))
  testthat::expect_equal(results$Female, refFemale)
  testthat::expect_true(all(results$Male == 0))
})

test_that("computeBirths: basic", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- ehep:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  rates <- vector(mode = "double", length = length(ehep:::GPE$ages))
  rates[] <- 1

  testthat::expect_equal(ehep:::computeBirths(pop$female@values, rates),
                         sum(pop$female@values))

  rates[] <- 0
  testthat::expect_equal(ehep:::computeBirths(pop$female@values, rates), 0)
})

test_that("computeDeaths: basic", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- ehep:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  rates <- vector(mode = "double", length = length(ehep:::GPE$ages))
  rates[] <- 1000
  ratesList <- list(Female = rates, Male = rates)

  print(pop)
  print(ratesList$Female)

  results <- ehep:::computeDeaths(pop, ratesList)

  print(results)

  testthat::expect_true(TRUE)
})
