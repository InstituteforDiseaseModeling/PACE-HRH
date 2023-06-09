library(pacehrh)

withr::local_dir("..")

test_that("Prevelance matrix generation: basic", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE
  local_vars("globalConfigLoaded", envir = gpe)
  local_vars("inputExcelFile", envir = gpe)

  pacehrh::SetInputExcelFile("./simple_config/model_inputs.xlsx")

  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  pacehrh::InitializePopulation()
  pacehrh::InitializeScenarios()
  pacehrh::InitializeStochasticParameters()
  pacehrh::InitializeSeasonality()
  pacehrh::InitializeCadreRoles()

  scenario <- "TEST_Prevalence"
  startYear <- 2025
  endYear <- 2050

  pacehrh::SetGlobalStartEndYears(startYear, endYear)

  # Set up as if to start a suite of experiments (read sheets, etc), but don't
  # actually run them
  res <- pacehrh::SaveBaseSettings(scenario)
  testthat::expect_true(!is.null(res))

  # Test that generated prevalence rates don't exceed the limits
  if (!is.null(res)) {
    pacehrh::SetStochasticity(TRUE)
    e <- new.env()
    m1 <- pacehrh:::generatePrevalenceRatesMatrix(debugEnv = e)
    testthat::expect_true(!is.null(m1))

    if (!is.null(m1)){
      rateLimits <- e$rateLimits

      tf <- sapply(1:dim(m1)[2], function(j) {
        values <- m1[, j]
        return(all((rateLimits$max - values) >= 0))
      })

      testthat::expect_true(all(tf))

      tf <- sapply(1:dim(m1)[2], function(j) {
        values <- m1[, j]
        return(all((values - rateLimits$min) >= 0))
      })

      testthat::expect_true(all(tf))
    }

    pacehrh::SetStochasticity(FALSE)
    e <- new.env()
    m2 <- pacehrh:::generatePrevalenceRatesMatrix(debugEnv = e)
    testthat::expect_true(!is.null(m2))

    if (!is.null(m2)){
      rateLimits <- e$rateLimits

      tf <- sapply(1:dim(m2)[2], function(j) {
        values <- m2[, j]
        return(all((values - rateLimits$min) >= 0))
      })

      testthat::expect_true(all(tf))

      tf <- sapply(1:dim(m2)[2], function(j) {
        values <- m2[, j]
        return(all((values - rateLimits$min) >= 0))
      })

      testthat::expect_true(all(tf))

      pacehrh::SetStochasticity(TRUE)
    }

    # Some code to generate graphs of generated prevalence rates.
    # ----------------------------------------------------------
    #
    # m1 <- pacehrh:::generatePrevalenceRatesMatrix()
    #
    # df1 <- data.frame(Year = dimnames(m1)[[2]], "IND.1" = m1["IND.1",], "IND.2" = m1["IND.2",])
    # df1 <- tidyr::pivot_longer(df1, c("IND.1", "IND.2"), names_to = "Task", values_to = "Rate")
    #
    # g1 <- ggplot(df1, aes(x = Year, y = Rate, color = Task, group = Task)) + geom_line() + geom_point()
    #
    # pacehrh::SetStochasticity(FALSE)
    # m2 <- pacehrh:::generatePrevalenceRatesMatrix()
    #
    # df2 <- data.frame(Year = dimnames(m2)[[2]], "IND.1" = m2["IND.1",], "IND.2" = m2["IND.2",])
    # df2 <- tidyr::pivot_longer(df2, c("IND.1", "IND.2"), names_to = "Task", values_to = "Rate")
    #
    # g2 <- ggplot(df2, aes(x = Year, y = Rate, color = Task, group = Task)) + geom_line() + geom_point()
    #
    #
    # png("graph_xxx.png", width = 1000, height = 800)
    # gridExtra::grid.arrange(g1, g2)
    # dev.off()
  }
})
