library(pacehrh)

withr::local_dir("..")

.scenario <- "MergedModel"
.nTrials <- 5
.startYear <- 2025
.endYear <- 2050
.nMonths <- 12 * length(.startYear:.endYear)

.results <- NULL

test_that("Plotting: setup", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE
  local_vars("inputExcelFile", envir = gpe)
  local_vars("globalConfigLoaded", envir = gpe)
  local_vars("initialPopulation", envir = bve)
  local_vars("populationLabels", envir = bve)

  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()
  pacehrh::InitializeCadreRoles()

  pacehrh::SetGlobalStartEndYears(.startYear, .endYear)

  .results <<-
    pacehrh::RunExperiments(scenarioName = .scenario,
                            trials = .nTrials)

  testthat::expect_true(!is.null(.results))
})

test_that("Plotting: basic population plots", {
  testthat::expect_true(!is.null(.results))
  testthat::expect_equal(length(.results), .nTrials)

  png("graph_101.png", width = 1000, height = 1000)
  g <- pacehrh::PlotPopulationCurve(.results[[2]]$Population[["2030"]]$Male)
  print(g)
  dev.off()

  png("graph_102.png", width = 1000, height = 1000)
  g <- pacehrh::PlotResultsPopulationCurve(.results, trial = 2, year = 2030, sex = "m")
  print(g)
  dev.off()

  r <- .results[[1]]$Population

  g <- pacehrh::PlotPopulationCurves(
    r[['2025']]$Male,
    r[['2030']]$Male,
    r[['2035']]$Male,
    r[['2040']]$Male,
    r[['2045']]$Male,
    r[['2050']]$Male,
    xaxis = pacehrh:::GPE$ages,
    colors = c("royalblue4", "royalblue3", "royalblue2", "royalblue1"),
    shapes = c(0, 1, 2, 5)
  )

  png("graph_103.png", width = 1000, height = 1000)
  print(g)
  dev.off()

  df <- pacehrh:::gatherPopulation(r)
  g <- pacehrh::PlotPyramid(df, 2030)

  png("graph_104.png", width = 1000, height = 1000)
  print(g)
  dev.off()

  g <- pacehrh::PlotPyramids(df)
  png("graph_105.png", width = 1000, height = 1000)
  print(g)
  dev.off()

  g <- pacehrh::PlotResultsMortalityRates(.results, trial = 1, year = 2030)
  png("graph_106.png", width = 1000, height = 1000)
  print(g)
  dev.off()
})
