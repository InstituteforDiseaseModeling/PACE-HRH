library(ehep)

withr::local_dir("..")
e <- ehep:::GPE
local_vars("globalDebug", envir = e)
GPE$globalDebug <- TRUE

# test_that("explodeMortalityRates: basic", {
#   refFemale <- c(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5,
#                  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7,
#                  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
#                  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10,
#                  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
#
#   refMale <- c(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5,
#                6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 11, 11, 11, 11, 11,
#                11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
#                13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14,
#                14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14)
#
#   results <- ehep:::explodeMortalityRates(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
#
#   testthat::expect_true(!is.null(results))
#   testthat::expect_named(results, c("Female", "Male"))
#   testthat::expect_equal(results$Female, refFemale)
#   testthat::expect_equal(results$Male, refMale)
# })

# test_that("explodeFertilityRates: basic", {
#   refFemale <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
#                  2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5,
#                  6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#
#   results <- ehep:::explodeFertilityRates(c(1,2,3,4,5,6,7))
#
#   testthat::expect_true(!is.null(results))
#   testthat::expect_named(results, c("Female", "Male"))
#   testthat::expect_equal(results$Female, refFemale)
#   testthat::expect_true(all(results$Male == 0))
# })

test_that("computeBirths: basic", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- ehep:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  rates <- vector(mode = "double", length = length(ehep:::GPE$ages))

  rates[] <- 1
  r <- list(femaleFertility = rates)
  testthat::expect_equal(ehep:::.computeBirths(pop$female@values, r),
                         sum(pop$female@values))

  rates[] <- 0
  r <- list(femaleFertility = rates)
  testthat::expect_equal(ehep:::.computeBirths(pop$female@values, r), 0)
})

test_that("computeDeaths: basic", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- ehep:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  rates <- vector(mode = "double", length = length(ehep:::GPE$ages))

  # Rates are expressed as "deaths per 1000", so rates = 1000 sets
  # the actual per-person multiplier to unity.
  rates[] <- 1
  r <- list(femaleMortality = rates, maleMortality = rates/2)

  results <- ehep:::.computeDeaths(data.frame(
    Female = pop$female@values,
    Male = pop$male@values
  ), r)

  compSum = sum(seq(10000,0,-100))
  testthat::expect_equal(sum(results$Female), compSum)
  testthat::expect_equal(sum(results$Male), compSum/2)
})

# test_that("ComputeDemographicsProjection: simple", {
#   testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
#
#   e <- ehep:::GPE
#   local_vars("inputExcelFile", envir = e)
#
#   e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
#
#   pop <- ehep:::loadInitialPopulation(sheetName = "TEST_TotalPop")
#   pars <- ehep:::loadStochasticParameters(sheetName = "TEST_StochasticParms")
#   pcp <- ehep:::loadPopulationChangeParameters(sheetName = "TEST_PopValues")
# #  pcp <- ehep:::loadPopulationChangeParameters(sheetName = "TEST_PopValues_Const")
#   ages <- ehep:::GPE$ages
#   years <- ehep:::GPE$years
#
#   # This conversion needs to go away. History: ComputeDemographicsProjection()
#   # was one of the first functions written for EHEP, before we started to carry
#   # PopulationPyramid objects around. It was initially easier to just convert
#   # between formats when ComputeDemographicsProjection() is called rather than
#   # risk breaking ComputeDemographicsProjection() itself.
#   initialPopulationDf <- data.frame(
#     Age = pop$age,
#     Female = pop$female@values,
#     Male = pop$male@values,
#     Total = pop$total@values
#   )
#
#   mf <- .generateFertilityRatesMatrix(pars, years, pcp, stochasticity = FALSE)
#   mm <- .generateMortalityRatesMatrix(pars, years, pcp, stochasticity = FALSE)
#
#   demographics <- ComputeDemographicsProjection(
#     initialPopulationDf,
#     mf,
#     mm,
#     years,
#     normalize = NULL,
#     growthFlag = TRUE,
#     debug = TRUE
#   )
#
#   testthat::expect_true(!is.null(demographics))
#
#   # Save a graph to eyeball for general shape, etc
#   png("graph_003.png", width = 800, height = 800)
#   g <- ehep::PlotPopulationCurve(demographics[[1]]$Female, xaxis = ages)
#   print(g)
#   dev.off()
#
#   png("graph_004.png", width = 800, height = 800)
#   g <- ehep::PlotPopulationCurves(
#     demographics[[1]]$Female,
#     demographics[[2]]$Female,
#     demographics[[3]]$Female,
#     demographics[[4]]$Female,
#     demographics[[5]]$Female,
#     demographics[[6]]$Female,
#     xaxis = ages,
#     colors = c("royalblue4", "royalblue3", "royalblue2", "royalblue1"),
#     shapes = c(0, 1, 2, 5)
#   )
#   print(g)
#   dev.off()
#
# })
