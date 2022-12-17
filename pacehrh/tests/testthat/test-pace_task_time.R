library(pacehrh)

withr::local_dir("..")

gpe <- pacehrh:::GPE
bve <- pacehrh:::BVE

test_that("Populations: read sub-ranges", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("populationLabels", envir = bve)

  testPop <- data.frame(Range = pacehrh:::GPE$ages,
                        Female = seq(1, length(pacehrh:::GPE$ages), 1),
                        Male = seq(1, length(pacehrh:::GPE$ages), 1) + 100)

  # Range = 0,1,2, ... 100
  # Female = 1,2,3, ... 99,100,101
  # Male = 101,102,103, ... 199,200,201

  dt <- data.table::data.table(
    Labels = c("label_1", "label_2", "label_3", "label_4", "-", "all"),
    Male = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE),
    Female = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
    Start = c(0, 0, 15, 15, 0, NA),
    End = c(50, 100, 49, 49, 0, NA)
  )

  bve$populationLabels <- dt

  testthat::expect_false(is.null(bve$populationLabels))
  testthat::expect_warning(pacehrh:::.computeApplicablePopulation(testPop, "notalabel"))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "label_1"), sum(1:51) + sum(101:151))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "label_2"), sum(1:101) + sum(101:201))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "label_3"), sum(16:50))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "label_4"), sum(116:150))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "-"), 0)
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "all"), sum(1:101) + sum(101:201))

  dt <- data.table::data.table(
    Labels = c("dup", "dup"),
    Male = c(TRUE, TRUE),
    Female = c(TRUE, TRUE),
    Start = c(0, 0),
    End = c(50, 100)
  )

  bve$populationLabels <- dt
  testthat::expect_warning(n <- pacehrh:::.computeApplicablePopulation(testPop, "dup"))
  testthat::expect_equal(n, sum(1:51) + sum(101:151))
})

test_that("Populations: read full ranges", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("inputExcelFile", envir = gpe)
  local_vars("globalConfigLoaded", envir = gpe)
  local_vars("initialPopulation", envir = bve)
  local_vars("populationLabels", envir = bve)
  local_vars("populationRangesTable", envir = bve)

  gpe$inputExcelFile <- "./simple_config/super_simple_inputs.xlsx"
  gpe$globalConfigLoaded <- TRUE

  pacehrh::InitializePopulation()

  # Test for existence of Population Ranges Table
  testthat::expect_true(!is.null(pacehrh:::BVE$populationRangesTable))

  # Test that population range table entries consist entirely of zeroes and ones.)
  for (m in pacehrh:::BVE$populationRangesTable){
    apply(m, 1, function(r){
      testthat::expect_equal(length(setdiff(r, c(0,1))), 0)
    })
  }

  # Test the dimensions of the range tables
  testthat::expect_equal(dim(pacehrh:::BVE$populationRangesTable$Male)[2],
                         length(pacehrh:::GPE$ages))

  testthat::expect_equal(dim(pacehrh:::BVE$populationRangesTable$Female)[2],
                         length(pacehrh:::GPE$ages))
})

test_that("Task time computations: simple", {
  local_vars("inputExcelFile", envir = gpe)
  local_vars("globalConfigLoaded", envir = gpe)
  local_vars("roundingLaw", envir = gpe)
  local_vars("stochasticity", envir = gpe)
  local_vars("initialPopulation", envir = bve)
  local_vars("populationLabels", envir = bve)
  local_vars("populationRangesTable", envir = bve)

  gpe$inputExcelFile <- "./simple_config/super_simple_inputs.xlsx"
  gpe$globalConfigLoaded <- TRUE

  pacehrh::InitializePopulation(popSheet = "Flat_Population")
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()

  scenario <- "TEST_Simple_2"
  nTrials <- 5
  startYear <- 2025
  endYear <- 2050
  nMonths <- 12 * length(startYear:endYear)

  shoulderYears <- pacehrh:::GPE$shoulderYears

  pacehrh::SetRoundingLaw("none")
  pacehrh::SetStochasticity(FALSE)

  pacehrh::SetGlobalStartEndYears(startYear, endYear)

  testthat::expect_true(TRUE)

  results <-
    pacehrh::RunExperiments(scenarioName = scenario,
                            trials = nTrials)

  # The TEST_Simple_2 scenario combined with stochasticity == FALSE and
  # roundingLaw == "none" produces a flat population (N = 100 for every
  # age/gender group, giving a total population of 20200), that doesn't change
  # from year to year (the reproductive rate in Flat_Rates is set up so the
  # population is exactly replenished) or from trial to trial.

  # Scan over all trials, all population years, and all population buckets.

  l <- lapply(results, function(r){ # Trials ... r = trial
    s1 <- sapply(r$Population, function(p){ # Population years ... p = pop tree
      all(p$Female == 100) # All population buckets in a population tree
    })

    s2 <- sapply(r$Population, function(p){
      all(p$Male == 100)
    })

    return(list(S1 = s1, S2 = s2))
  })

  # Every bucket should equal 100

  testthat::expect_true(all(unlist(l)))

  ref <- results[[1]]$AnnualCounts
  testthat::expect_true(all(sapply(results, function(r){r$AnnualCounts == ref})))

  ref <- results[[1]]$AnnualTimes
  testthat::expect_true(all(sapply(results, function(r){r$AnnualTimes == ref})))

  # print(results[[1]]$AnnualCounts)
  # print(results[[1]]$AnnualTimes)

#  print(pacehrh:::EXP$prevalenceRatesMatrix)
})
