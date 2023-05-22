library(pacehrh)
library(ggplot2)

withr::local_dir("..")

test_that("Rates graphs: flat", {
  e <- pacehrh:::GPE
  local_vars("globalConfigLoaded", envir = e)

  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()
  pacehrh::InitializeCadreRoles()

  scenario <- "MergedModel"
  nTrials <- 5
  startYear <- 2025
  endYear <- 2050
  nMonths <- 12 * length(startYear:endYear)

  pacehrh::SetGlobalStartEndYears(startYear, endYear)

  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                            trials = nTrials)

  g <- pacehrh::PlotFertilityRatesStats(results, type = "boxplot", log = FALSE)
#  print(g)

  testthat::expect_true(!is.null(g))

  g <- pacehrh::PlotFertilityRatesStats(results, type = "boxplot", log = TRUE)
#  print(g)

  g <- pacehrh::PlotFertilityRatesStats(results, se = FALSE, type = "lines")
#  print(g)

  g <- pacehrh::PlotFertilityRatesStats(results, se = TRUE, type = "lines")
#  print(g)

  g <- pacehrh::PlotFertilityRatesStats(results, se = FALSE, type = "ribbon")
#  print(g)

  g <- pacehrh::PlotFertilityRatesStats(results, se = TRUE, type = "ribbon")
#  print(g)

  g <- pacehrh::PlotFertilityRates(results[[5]]$PopulationRates, 2030)
#  print(g)

  g <- pacehrh::PlotMortalityRates(results[[5]]$PopulationRates, 2030)
#  print(g)

  g <- pacehrh::PlotResultsFertilityRates(results, 1, 2030)
#  print(g)

  g <- pacehrh::PlotResultsMortalityRates(results, 5, 2030)
#  print(g)

  # png("graph_004.png", width = 1000, height = 800)
  # g <-
  #   ggplot(data = dff,
  #          aes(
  #            x = Age,
  #            y = Population,
  #            group = Year,
  #            color = as.character(Year)
  #          ))
  # g <- g + geom_line()
  # g <- g + facet_wrap(vars(Year))
  # g <- g + theme(legend.position = "none")
  # print(g)
  # dev.off()
})
