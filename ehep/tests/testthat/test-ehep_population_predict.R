library(ehep)

withr::local_dir("..")
e <- ehep:::GPE
local_vars("globalDebug", envir = e)
GPE$globalDebug <- TRUE

test_that("explodeMortalityRates: basic", {
  refFemale <- c(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5,
                 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7,
                 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
                 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10,
                 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)

  refMale <- c(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5,
               6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 11, 11, 11, 11, 11,
               11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
               13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14,
               14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14)

  results <- ehep:::explodeMortalityRates(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))

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

  # Rates are expressed as "deaths per 1000", so rates = 1000 sets
  # the actual per-person multiplier to unity.
  rates[] <- 1000
  ratesList <- list(Female = rates, Male = rates/2)

  popDf <- data.frame(Female = pop$female@values, Male = pop$male@values)
  results <- ehep:::computeDeaths(popDf, ratesList)

  compSum = sum(seq(10000,0,-100))
  testthat::expect_equal(sum(results$Female), compSum)
  testthat::expect_equal(sum(results$Male), compSum/2)
})

test_that("ComputeDemographicsProjection: simple", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  pop <- ehep:::loadInitialPopulation(sheetName = "TEST_TotalPop")
  pars <- ehep:::loadStochasticParameters(sheetName = "TEST_StochasticParms")
  pcp <- ehep:::loadPopulationChangeParameters(sheetName = "TEST_PopValues")
  ages <- ehep:::GPE$ages
  years <- ehep:::GPE$years

  # This conversion needs to go away. History: ComputeDemographicsProjection()
  # was one of the first functions written for EHEP, before we started to carry
  # PopulationPyramid objects around. It was initially easier to just convert
  # between formats when ComputeDemographicsProjection() is called rather than
  # risk breaking ComputeDemographicsProjection() itself.
  initialPopulationDf <- data.frame(
    Age = pop$age,
    Female = pop$female@values,
    Male = pop$male@values,
    Total = pop$total@values
  )

  mf <- .generateFertilityRatesMatrix(pars, years, pcp, stochasticity = FALSE)
  mm <- .generateMortalityRatesMatrix(pars, years, pcp, stochasticity = FALSE)

  demographics <- ComputeDemographicsProjection(
    initialPopulationDf,
    mf,
    mm,
    years,
    normalize = NULL,
    growthFlag = TRUE,
    debug = FALSE
  )

  # Save a graph to eyeball for general shape, etc
  # png("graph_003.png")
  # g <- ehep::PlotPopulationCurves(demographics)
  # print(g)
  # dev.off()


  # EXP$demographics <-
  #   ComputeDemographicsProjection(
  #     initialPopulationDf,
  #     EXP$fertilityRatesMatrix,
  #     EXP$mortalityRatesMatrix,
  #     GPE$years,
  #     normalize = scenario$BaselinePop,
  #     growthFlag = scenario$o_PopGrowth,
  #     debug = TRUE
  #   )
  #
  #
  #
  #
  #
  # ComputeDemographicsProjection <- function(initial_population_pyramid,
  #                                           fertility_rates,
  #                                           mortality_rates,
  #                                           years,
  #                                           normalize = NULL,
  #                                           growthFlag = TRUE,
  #                                           debug = FALSE)

#  print(demographics)

  testthat::expect_true(!is.null(demographics))
})
