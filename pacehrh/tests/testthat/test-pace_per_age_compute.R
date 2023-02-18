library(pacehrh)

withr::local_dir("..")

gpe <- pacehrh:::GPE
bve <- pacehrh:::BVE

.validateSuperSimpleInitialPopulation <- function(pop){
  testthat::expect_false(is.null(pop))
  testthat::expect_equal(names(pop), c("Age", "Female", "Male", "Total"))

  types <- sapply(pop, typeof)
  names(types) <- NULL
  testthat::expect_equal(types, c("double", "double", "double", "double"))

  testthat::expect_true(all(pop$Female == 100))
  testthat::expect_true(all(pop$Male == 100))
  testthat::expect_true(all(pop$Total == 200))
}

test_that("Per-Age: baseline", {
  local_vars("inputExcelFile", envir = gpe)
  local_vars("ignoreGlobalConfigExcelFileSetting", envir = gpe)
  local_vars("globalConfigLoaded", envir = gpe)
  local_vars("perAgeStats", envir = gpe)
  local_vars("stochasticity", envir = gpe)
  local_vars("roundingLaw", envir = gpe)

  gpe$globalConfigLoaded <- FALSE

  pacehrh::SetInputExcelFile("./simple_config/super_simple_inputs.xlsx")
  testthat::expect_true(gpe$ignoreGlobalConfigExcelFileSetting)

  pacehrh::InitializePopulation(popSheet = "Flat_Population")
  .validateSuperSimpleInitialPopulation(bve$initialPopulation)

  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()

  pacehrh::SetGlobalStartEndYears(2025, 2055)
  pacehrh::SetStochasticity(FALSE)
  pacehrh::SetRoundingLaw("none")
  pacehrh::SetPerAgeStats("monthly")

  scenario <- "TEST_Simple_2"
  nTrials <- 5

  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                            trials = nTrials)

  testthat::expect_true(!is.null(results))
  testthat::expect_equal(length(results), nTrials)

  r <- results[[1]]
  expectedDataNames <- c("AnnualTimes", "AnnualCounts", "SeasonalityResults", "MonthlyPerAge", "Population", "PopulationRates")
  testthat::expect_true(length(setdiff(names(r), expectedDataNames)) == 0)
  testthat::expect_true(length(setdiff(expectedDataNames, names(r))) == 0)

  r <- results[[1]]$MonthlyPerAge
  testthat::expect_true(!is.null(r))
  if (is.null(r)){
    message("Major failure: results structure did not include MonthlyPerAge section")
    return()
  }

  testthat::expect_equal(names(r), c("Counts", "Times"))
  testthat::expect_equal(names(r$Counts), c("Female", "Male"))
  testthat::expect_equal(names(r$Times), c("Female", "Male"))

  nTasks <- sum(pacehrh:::BVE$taskData$computeMethod == "TimePerTask")
  nAges <- length(pacehrh:::GPE$ages)
  nMonths <- 12 * (length(pacehrh:::GPE$years))

  testthat::expect_equal(dim(r$Times$Female), c(nTasks, nAges, nMonths))

  # Check that the per-age values sum to the previously calculated all-age. Note
  # the usual caveat that tests for equality between floating point numbers are
  # seldom perfect.

  r <- results[[1]]
  m3 <- r$MonthlyPerAge$Times

  out <- sapply(dimnames(m3$Female)$Task, function(task){
    allAgeValues <- r$SeasonalityResults[[task]]$Time

    mf <- m3$Female[task,,]
    mm <- m3$Male[task,,]
    perAgeValueSums <- (colSums(mm) + colSums(mf))

    difference <- abs(allAgeValues - perAgeValueSums)
    return(all(difference < 10e-6))
  })

  testthat::expect_true(all(out))
})

test_that("Per-Age: annual only", {
  local_vars("inputExcelFile", envir = gpe)
  local_vars("ignoreGlobalConfigExcelFileSetting", envir = gpe)
  local_vars("globalConfigLoaded", envir = gpe)
  local_vars("perAgeStats", envir = gpe)
  local_vars("stochasticity", envir = gpe)
  local_vars("roundingLaw", envir = gpe)
  
  gpe$globalConfigLoaded <- FALSE
  
  pacehrh::SetInputExcelFile("./simple_config/super_simple_inputs.xlsx")
  testthat::expect_true(gpe$ignoreGlobalConfigExcelFileSetting)
  
  pacehrh::InitializePopulation(popSheet = "Flat_Population")
  .validateSuperSimpleInitialPopulation(bve$initialPopulation)
  
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()
  
  pacehrh::SetGlobalStartEndYears(2025, 2055)
  pacehrh::SetStochasticity(FALSE)
  pacehrh::SetRoundingLaw("none")
  pacehrh::SetPerAgeStats("annual")
  
  scenario <- "TEST_Simple_2"
  nTrials <- 5
  
  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                            trials = nTrials)
  
  testthat::expect_true(!is.null(results))
  testthat::expect_equal(length(results), nTrials)
  
  r <- results[[1]]
  expectedDataNames <- c("AnnualTimes", "AnnualCounts", "SeasonalityResults", "AnnualPerAge", "Population", "PopulationRates")
  testthat::expect_true(length(setdiff(names(r), expectedDataNames)) == 0)
  testthat::expect_true(length(setdiff(expectedDataNames, names(r))) == 0)
  
  r <- results[[1]]$AnnualPerAge
  testthat::expect_true(!is.null(r))
  if (is.null(r)){
    message("Major failure: results structure did not include AnnualPerAge section")
    return()
  }
  
  # A couple of oddnesses here:
  #
  # - The order of sub-sections in the AnnualPerAge section is {$Times, $Counts}, 
  # but in the MonthlyPerAge section is {$Counts, $Times}. This might be because
  # the MonthlyPerAge section is stored as an environment - which hashes it's 
  # contents - before being converted to a list. The as.list() operation picks
  # up the hashed order of the sub-sections instead of the order of creation.
  #
  # - The AnnualPerAge section includes shoulder years. That's because the
  # shoulder years are needed for the seasonality calculations that would
  # follow if MonthlyPerAge stats were selected.
  
  testthat::expect_equal(names(r), c("Times", "Counts"))
  testthat::expect_equal(names(r$Counts), c("Female", "Male"))
  testthat::expect_equal(names(r$Times), c("Female", "Male"))
  
  nTasks <- length(pacehrh:::BVE$taskData$computeMethod)
  nAges <- length(pacehrh:::GPE$ages)
  nYears <- length(pacehrh:::BVE$years) # BVE$years includes shoulder years
  
  testthat::expect_equal(dim(r$Times$Female), c(nTasks, nAges, nYears))
})
