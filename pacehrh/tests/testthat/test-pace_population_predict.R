library(pacehrh)
library(ggplot2)

withr::local_dir("..")
e <- pacehrh:::GPE
local_vars("globalDebug", envir = e)
GPE$globalDebug <- TRUE

test_that("computeBirths: basic", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/model_inputs.xlsx"
  pop <- pacehrh:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  rates <- vector(mode = "double", length = length(pacehrh:::GPE$ages))

  rates[] <- 1
  r <- list(femaleFertility = rates)
  testthat::expect_equal(pacehrh:::.computeBirths(pop$Female, r),
                         sum(pop$Female))

  rates[] <- 0
  r <- list(femaleFertility = rates)
  testthat::expect_equal(pacehrh:::.computeBirths(pop$Female, r), 0)
})

test_that("computeDeaths: basic", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/model_inputs.xlsx"
  pop <- pacehrh:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  rates <- vector(mode = "double", length = length(pacehrh:::GPE$ages))

  # Rates are expressed as "deaths per 1000", so rates = 1000 sets
  # the actual per-person multiplier to unity.
  rates[] <- 1
  r <- list(femaleMortality = rates, maleMortality = rates/2)

  results <- pacehrh:::.computeDeaths(data.frame(
    Female = pop$Female,
    Male = pop$Male
  ), r)

  compSum = sum(seq(10000,0,-100))
  testthat::expect_equal(sum(results$Female), compSum)
  testthat::expect_equal(sum(results$Male), compSum/2)
})

test_that("Population predictions: flat", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("roundingLaw", envir = e)

  inputFile <- "./simple_config/super_simple_inputs.xlsx"

  # Set input file, and cheat the system into thinking the global configuration
  # is already loaded
  pacehrh::SetInputExcelFile(inputFile)
  e$globalConfigLoaded <- TRUE

  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, inputFile)

  # This configuration is set up so
  # (1) Every year bucket has the same population;
  # (2) Everybody lives to 100, then dies; and
  # (3) the birth rate exactly replenishes the population.

  initPop <- pacehrh:::loadInitialPopulation(sheetName = "Flat_Population")
  pcr <- pacehrh:::loadPopulationChangeRates(sheetName = "Flat_Rates")

  years <- 2020:2040
  pacehrh::SetRoundingLaw("none") # Turn rounding off

  # Turn off stochasticity and generate several years of rates
  pcr <- pacehrh:::addRatesMatricesToPopulationChangeRates(pcr, years, NULL)

  population <- pacehrh::ComputePopulationProjection(
    initPop,
    pcr,
    years)

  df <- pacehrh:::gatherPopulation(population)
  dff <- df[df$Gender == "FM",]

  png("graph_004.png", width = 1000, height = 800)
  g <- ggplot(data = dff, aes(x = Age, y = Population, group = Year, color = as.character(Year)))
  g <- g + geom_line()
  g <- g + facet_wrap(vars(Year))
  g <- g + theme(legend.position = "none")
  print(g)
  dev.off()
})

test_that("Population predictions: rising", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("roundingLaw", envir = e)

  inputFile <- "./simple_config/super_simple_inputs.xlsx"
  # Set input file, and cheat the system into thinking the global configuration
  # is already loaded
  pacehrh::SetInputExcelFile(inputFile)
  e$globalConfigLoaded <- TRUE

  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, inputFile)

  # This configuration is set up so
  # (1) Every year bucket has the same population;
  # (2) Everybody lives to 100, then dies; and
  # (3) The birth rate more than replenishes the population.

  initPop <- pacehrh:::loadInitialPopulation(sheetName = "Flat_Population")
  pcr <- pacehrh:::loadPopulationChangeRates(sheetName = "Rise_Rates")

  # Turn off stochasticity and generate several years of rates

  years <- 2020:2050
  pcr <- pacehrh:::addRatesMatricesToPopulationChangeRates(pcr, years, NULL)

  # Turn off rounding

  pacehrh::SetRoundingLaw("none")

  population <- pacehrh::ComputePopulationProjection(
    initPop,
    pcr,
    years)

  df <- pacehrh:::gatherPopulation(population)
  dff <- df[df$Gender == "FM",]

  png("graph_005.png", width = 1600, height = 1000)
  g <- ggplot(data = dff, aes(x = Age, y = Population, group = Year, color = as.character(Year)))
  g <- g + geom_line()
  g <- g + facet_wrap(vars(Year))
  g <- g + theme(legend.position = "none")
  print(g)
  dev.off()
})
