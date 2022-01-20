# This test should run before all the others in the EHEP test suite because it
# tests for the existence of objects created during the library load process

library(ehep)

test_that("Library load: environments", {
  testthat::expect_true(is.environment(ehep:::globalPackageEnvironment))
  testthat::expect_true(is.environment(ehep:::baseValuesEnvironment))
  testthat::expect_true(is.environment(ehep:::epsilonValuesEnvironment))
  testthat::expect_true(is.environment(ehep:::experimentValuesEnvironment))

  testthat::expect_true(is.environment(ehep:::GPE))
  testthat::expect_true(is.environment(ehep:::BVE))
  testthat::expect_true(is.environment(ehep:::EPS))
  testthat::expect_true(is.environment(ehep:::EXP))
})
