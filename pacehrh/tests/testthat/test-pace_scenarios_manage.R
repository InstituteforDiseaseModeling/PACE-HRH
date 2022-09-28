library(pacehrh)

withr::local_dir("..")

test_that("Scenario management: create empty scenarios table", {
  tbl <- pacehrh::CreateScenariosTable()

  testthat::expect_equal(class(tbl), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(nrow(tbl), 0L)

  testthat::expect_equal(names(tbl), pacehrh:::.scenarioColumnNames)
  testthat::expect_true(all(sapply(tbl,typeof) == pacehrh:::.scenarioColumnTypes))
})

test_that("Scenario management: AddScenario", {
  # Uncomment to turn on tracing
  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  tbl <- pacehrh::CreateScenariosTable()
  testthat::expect_equal(nrow(tbl), 0)

  # The following tests should all fail for invalid arguments, either too
  # few, or invalid types.
  testthat::expect_null(pacehrh::AddScenario())

  tbl <- pacehrh::AddScenario(tbl)
  testthat::expect_equal(nrow(tbl), 0)

  tbl <- pacehrh::AddScenario(tbl, NULL)
  testthat::expect_equal(nrow(tbl), 0)

  tbl <- pacehrh::AddScenario(tbl, NA)
  testthat::expect_equal(nrow(tbl), 0)

  tbl <- pacehrh::AddScenario(tbl, "")
  testthat::expect_equal(nrow(tbl), 0)

  tbl <- pacehrh::AddScenario(tbl, 12345)
  testthat::expect_equal(nrow(tbl), 0)

  # The following tests should succeed
  tbl <- pacehrh::AddScenario(tbl, "Scenario 1")
  testthat::expect_equal(nrow(tbl), 1)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1"))

  tbl <- pacehrh::AddScenario(tbl, "Scenario 2")
  testthat::expect_equal(nrow(tbl), 2)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2"))

  # Attempt to add duplicate ... should do nothing
  tbl <- pacehrh::AddScenario(tbl, "Scenario 2")
  testthat::expect_equal(nrow(tbl), 2)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2"))

  # Create a scenario with a field value
  tbl <- pacehrh::AddScenario(tbl, "Scenario 3", HrsPerWeek = 32)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))
  testthat::expect_equal(tbl$HrsPerWeek, c(NA, NA, 32))

  # Attempt to create a scenario with a bad field name
  tbl <- pacehrh::AddScenario(tbl, "Scenario 4", HrsPerWeek = 35, notafield = "notavalue")
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))
  testthat::expect_equal(tbl$HrsPerWeek, c(NA, NA, 32))
})

test_that("Scenario management: DeleteScenario", {
  # Uncomment to turn on tracing
  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  tbl <- pacehrh::CreateScenariosTable()
  testthat::expect_equal(nrow(tbl), 0)

  # Add scenarios
  tbl <- pacehrh::AddScenario(tbl, "Scenario 1")
  tbl <- pacehrh::AddScenario(tbl, "Scenario 2")
  tbl <- pacehrh::AddScenario(tbl, "Scenario 3")
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))

  # The following tests should all fail for invalid arguments, either too
  # few, or invalid types.
  testthat::expect_null(pacehrh::DeleteScenario())

  tbl <- pacehrh::DeleteScenario(tbl)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- pacehrh::DeleteScenario(tbl, NULL)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- pacehrh::DeleteScenario(tbl, NA)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- pacehrh::DeleteScenario(tbl, "")
  testthat::expect_equal(nrow(tbl), 3)

  # The following tests should succeed
  tbl <- pacehrh::DeleteScenario(tbl, "Scenario 2")
  testthat::expect_equal(nrow(tbl), 2)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 3"))

  tbl <- pacehrh::DeleteScenario(tbl, "Scenario 1")
  testthat::expect_equal(nrow(tbl), 1)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 3"))

  tbl <- pacehrh::DeleteScenario(tbl, "not_a_scenario")
  testthat::expect_equal(nrow(tbl), 1)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 3"))
})

test_that("Scenario management: UpdateScenario", {
  # Uncomment to turn on tracing
  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  tbl <- pacehrh::CreateScenariosTable()
  testthat::expect_equal(nrow(tbl), 0)

  # Add scenarios
  tbl <- pacehrh::AddScenario(tbl, "Scenario 1", HrsPerWeek = 35)
  tbl <- pacehrh::AddScenario(tbl, "Scenario 2", HrsPerWeek = 36)
  tbl <- pacehrh::AddScenario(tbl, "Scenario 3", HrsPerWeek = 37)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))
  testthat::expect_equal(tbl$HrsPerWeek, c(35, 36, 37))

  # The following tests should all fail for invalid arguments, either too
  # few, or invalid types.
  testthat::expect_null(pacehrh::UpdateScenario())

  tbl <- pacehrh::UpdateScenario(tbl)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- pacehrh::UpdateScenario(tbl, NULL)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- pacehrh::UpdateScenario(tbl, NA)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- pacehrh::UpdateScenario(tbl, "")
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- pacehrh::UpdateScenario(tbl, "not_a_scenario")
  testthat::expect_equal(nrow(tbl), 3)

  # The following tests should succeed
  tbl <- pacehrh::UpdateScenario(tbl, "Scenario 2", HrsPerWeek = 40)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$HrsPerWeek, c(35, 40, 37))

  tbl <- pacehrh::UpdateScenario(tbl, "Scenario 1", WeeksPerYr = 50, HrsPerWeek = 41)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$WeeksPerYr, c(50, NA, NA))
  testthat::expect_equal(tbl$HrsPerWeek, c(41, 40, 37))

  # Invalid fields should be ignored
  tbl2 <- pacehrh::UpdateScenario(tbl, "Scenario 3", notafield = "not a value")
  testthat::expect_equal(tbl2, tbl)
  testthat::expect_false("notafield" %in% names(tbl))
})

test_that("Scenario management: ReadScenario", {
  # Uncomment to turn on tracing
  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  tbl <- pacehrh::CreateScenariosTable()
  testthat::expect_equal(nrow(tbl), 0)

  # Add scenarios
  tbl <- pacehrh::AddScenario(tbl, "Scenario 1", HrsPerWeek = 35)
  tbl <- pacehrh::AddScenario(tbl, "Scenario 2", HrsPerWeek = 36)
  tbl <- pacehrh::AddScenario(tbl, "Scenario 3", HrsPerWeek = 37)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))
  testthat::expect_equal(tbl$HrsPerWeek, c(35, 36, 37))

  # The following tests should all fail for invalid arguments, either too
  # few, or invalid types.
  testthat::expect_null(pacehrh::ReadScenario())
  testthat::expect_null(pacehrh::ReadScenario(tbl))
  testthat::expect_null(pacehrh::ReadScenario(tbl, NULL))
  testthat::expect_null(pacehrh::ReadScenario(tbl, NA))
  testthat::expect_null(pacehrh::ReadScenario(tbl, ""))
  testthat::expect_null(pacehrh::ReadScenario(tbl, "not_a_scenario"))

  # The following test should succeed
  scenario <- pacehrh::ReadScenario(tbl, "Scenario 2")
  testthat::expect_true(!is.null(scenario))
  testthat::expect_equal(scenario$UniqueID, "Scenario 2")
  testthat::expect_equal(scenario$HrsPerWeek, 36)
})
