library(pacehrh)

withr::local_dir("..")

gpe <- pacehrh:::GPE
bve <- pacehrh:::BVE

test_that("Configuration: stochasticity", {
  local_vars("stochasticity", envir = gpe)

  testthat::expect_equal(gpe$stochasticity, TRUE)

  # Interrogate current value. Should be TRUE.
  value <- pacehrh::SetStochasticity()
  testthat::expect_equal(value, TRUE)

  # Flip to FALSE
  value <- pacehrh::SetStochasticity(FALSE)
  testthat::expect_equal(value, TRUE)
  testthat::expect_equal(pacehrh::SetStochasticity(), FALSE)

  # Flip back to TRUE
  value <- pacehrh::SetStochasticity(TRUE)
  testthat::expect_equal(value, FALSE)
  testthat::expect_equal(pacehrh::SetStochasticity(), TRUE)

  # Check that trying to set a bad value doesn't overwrite existing
  value <- pacehrh::SetStochasticity('invalidvalue')
  testthat::expect_equal(value, TRUE)
  testthat::expect_equal(pacehrh::SetStochasticity(), TRUE)
})
