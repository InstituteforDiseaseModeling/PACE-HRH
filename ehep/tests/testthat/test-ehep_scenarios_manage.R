library(ehep)

withr::local_dir("..")

test_that("Scenario management: startup default", {
  e <- ehep:::GPE
  local_vars("scenarios", envir = e)
  testthat::expect_null(e$scenarios)
})

test_that("Scenario management: create empty scenarios table", {
  tbl <- ehep:::CreateScenariosTable()

  testthat::expect_equal(class(tbl), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_equal(nrow(tbl), 0L)

  testthat::expect_equal(names(tbl), ehep:::.scenarioColumnNames)
  testthat::expect_true(all(sapply(tbl,typeof) == ehep:::.scenarioColumnTypes))
})

test_that("Scenario management: AddScenario", {
  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  tbl <- ehep:::CreateScenariosTable()
  testthat::expect_equal(nrow(tbl), 0)

  # The following tests should all fail for invalid arguments, either too
  # few, or invalid types.
  testthat::expect_null(ehep::AddScenario())

  tbl <- ehep::AddScenario(tbl)
  testthat::expect_equal(nrow(tbl), 0)

  tbl <- ehep::AddScenario(tbl, NULL)
  testthat::expect_equal(nrow(tbl), 0)

  tbl <- ehep::AddScenario(tbl, NA)
  testthat::expect_equal(nrow(tbl), 0)

  tbl <- ehep::AddScenario(tbl, "")
  testthat::expect_equal(nrow(tbl), 0)

  tbl <- ehep::AddScenario(tbl, 12345)
  testthat::expect_equal(nrow(tbl), 0)

  # The following tests should succeed
  tbl <- ehep::AddScenario(tbl, "Scenario 1")
  testthat::expect_equal(nrow(tbl), 1)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1"))

  tbl <- ehep::AddScenario(tbl, "Scenario 2")
  testthat::expect_equal(nrow(tbl), 2)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2"))

  # Attempt to add duplicate ... should do nothing
  tbl <- ehep::AddScenario(tbl, "Scenario 2")
  testthat::expect_equal(nrow(tbl), 2)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2"))

  # Create a scenario with a field value
  tbl <- ehep::AddScenario(tbl, "Scenario 3", HrsPerWeek = 32)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))
  testthat::expect_equal(tbl$HrsPerWeek, c(NA, NA, 32))

  # Attempt to create a scenario with a bad field name
  tbl <- ehep::AddScenario(tbl, "Scenario 4", HrsPerWeek = 35, notafield = "notavalue")
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))
  testthat::expect_equal(tbl$HrsPerWeek, c(NA, NA, 32))
})

test_that("Scenario management: DeleteScenario", {
  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  tbl <- ehep:::CreateScenariosTable()
  testthat::expect_equal(nrow(tbl), 0)

  # Add scenarios
  tbl <- ehep::AddScenario(tbl, "Scenario 1")
  tbl <- ehep::AddScenario(tbl, "Scenario 2")
  tbl <- ehep::AddScenario(tbl, "Scenario 3")
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))

  # The following tests should all fail for invalid arguments, either too
  # few, or invalid types.
  testthat::expect_null(ehep::DeleteScenario())

  tbl <- ehep::DeleteScenario(tbl)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- ehep::DeleteScenario(tbl, NULL)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- ehep::DeleteScenario(tbl, NA)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- ehep::DeleteScenario(tbl, "")
  testthat::expect_equal(nrow(tbl), 3)

  # The following tests should succeed
  tbl <- ehep::DeleteScenario(tbl, "Scenario 2")
  testthat::expect_equal(nrow(tbl), 2)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 3"))

  tbl <- ehep::DeleteScenario(tbl, "Scenario 1")
  testthat::expect_equal(nrow(tbl), 1)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 3"))

  tbl <- ehep::DeleteScenario(tbl, "not_a_scenario")
  testthat::expect_equal(nrow(tbl), 1)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 3"))
})

test_that("Scenario management: UpdateScenario", {
  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  tbl <- ehep:::CreateScenariosTable()
  testthat::expect_equal(nrow(tbl), 0)

  # Add scenarios
  tbl <- ehep::AddScenario(tbl, "Scenario 1", HrsPerWeek = 35)
  tbl <- ehep::AddScenario(tbl, "Scenario 2", HrsPerWeek = 36)
  tbl <- ehep::AddScenario(tbl, "Scenario 3", HrsPerWeek = 37)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))
  testthat::expect_equal(tbl$HrsPerWeek, c(35, 36, 37))

  # The following tests should all fail for invalid arguments, either too
  # few, or invalid types.
  testthat::expect_null(ehep::UpdateScenario())

  tbl <- ehep::UpdateScenario(tbl)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- ehep::UpdateScenario(tbl, NULL)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- ehep::UpdateScenario(tbl, NA)
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- ehep::UpdateScenario(tbl, "")
  testthat::expect_equal(nrow(tbl), 3)

  tbl <- ehep::UpdateScenario(tbl, "not_a_scenario")
  testthat::expect_equal(nrow(tbl), 3)

  # The following tests should succeed
  tbl <- ehep::UpdateScenario(tbl, "Scenario 2", HrsPerWeek = 40)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$HrsPerWeek, c(35, 40, 37))

  tbl <- ehep::UpdateScenario(tbl, "Scenario 1", WeeksPerYr = 50, HrsPerWeek = 41)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$WeeksPerYr, c(50, NA, NA))
  testthat::expect_equal(tbl$HrsPerWeek, c(41, 40, 37))

  # Invalid fields should be ignored
  tbl2 <- ehep::UpdateScenario(tbl, "Scenario 3", notafield = "not a value")
  testthat::expect_equal(tbl2, tbl)
  testthat::expect_false("notafield" %in% names(tbl))
})

test_that("Scenario management: ReadScenario", {
  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  tbl <- ehep:::CreateScenariosTable()
  testthat::expect_equal(nrow(tbl), 0)

  # Add scenarios
  tbl <- ehep::AddScenario(tbl, "Scenario 1", HrsPerWeek = 35)
  tbl <- ehep::AddScenario(tbl, "Scenario 2", HrsPerWeek = 36)
  tbl <- ehep::AddScenario(tbl, "Scenario 3", HrsPerWeek = 37)
  testthat::expect_equal(nrow(tbl), 3)
  testthat::expect_equal(tbl$UniqueID, c("Scenario 1", "Scenario 2", "Scenario 3"))
  testthat::expect_equal(tbl$HrsPerWeek, c(35, 36, 37))

  # The following tests should all fail for invalid arguments, either too
  # few, or invalid types.
  testthat::expect_null(ehep::ReadScenario())
  testthat::expect_null(ehep::ReadScenario(tbl))
  testthat::expect_null(ehep::ReadScenario(tbl, NULL))
  testthat::expect_null(ehep::ReadScenario(tbl, NA))
  testthat::expect_null(ehep::ReadScenario(tbl, ""))
  testthat::expect_null(ehep::ReadScenario(tbl, "not_a_scenario"))

  # The following test should succeed
  scenario <- ehep::ReadScenario(tbl, "Scenario 2")
  testthat::expect_true(!is.null(scenario))
  testthat::expect_equal(scenario$UniqueID, "Scenario 2")
  testthat::expect_equal(scenario$HrsPerWeek, 36)
})
