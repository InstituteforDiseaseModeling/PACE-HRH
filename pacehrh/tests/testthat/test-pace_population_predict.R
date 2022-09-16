library(pacehrh)

withr::local_dir("..")
e <- pacehrh:::GPE
local_vars("globalDebug", envir = e)
GPE$globalDebug <- TRUE

test_that("computeBirths: basic", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- pacehrh:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  rates <- vector(mode = "double", length = length(pacehrh:::GPE$ages))

  rates[] <- 1
  r <- list(femaleFertility = rates)
  testthat::expect_equal(pacehrh:::.computeBirths(pop$female@values, r),
                         sum(pop$female@values))

  rates[] <- 0
  r <- list(femaleFertility = rates)
  testthat::expect_equal(pacehrh:::.computeBirths(pop$female@values, r), 0)
})

test_that("computeDeaths: basic", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- pacehrh:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  rates <- vector(mode = "double", length = length(pacehrh:::GPE$ages))

  # Rates are expressed as "deaths per 1000", so rates = 1000 sets
  # the actual per-person multiplier to unity.
  rates[] <- 1
  r <- list(femaleMortality = rates, maleMortality = rates/2)

  results <- pacehrh:::.computeDeaths(data.frame(
    Female = pop$female@values,
    Male = pop$male@values
  ), r)

  compSum = sum(seq(10000,0,-100))
  testthat::expect_equal(sum(results$Female), compSum)
  testthat::expect_equal(sum(results$Male), compSum/2)
})

# test_that("ComputeDemographicsProjection: simple", {
#   testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")
#
#   e <- pacehrh:::GPE
#   local_vars("inputExcelFile", envir = e)
#
#   e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
#
#   pop <- pacehrh:::loadInitialPopulation(sheetName = "TEST_TotalPop")
#   pars <- pacehrh:::loadStochasticParameters(sheetName = "TEST_StochasticParms")
#   pcp <- pacehrh:::loadPopulationChangeParameters(sheetName = "TEST_PopValues")
# #  pcp <- pacehrh:::loadPopulationChangeParameters(sheetName = "TEST_PopValues_Const")
#   ages <- pacehrh:::GPE$ages
#   years <- pacehrh:::GPE$years
#
#   # This conversion needs to go away. History: ComputeDemographicsProjection()
#   # was one of the first functions written for pacehrh, before we started to carry
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
#   g <- pacehrh::PlotPopulationCurve(demographics[[1]]$Female, xaxis = ages)
#   print(g)
#   dev.off()
#
#   png("graph_004.png", width = 800, height = 800)
#   g <- pacehrh::PlotPopulationCurves(
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
