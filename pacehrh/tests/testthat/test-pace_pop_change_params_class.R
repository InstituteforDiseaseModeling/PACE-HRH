library(ehep)

withr::local_dir("..")

test_that("PopulationChangeParameters class: create", {
  pcp <- ehep::PopulationChangeParameters()
  testthat::expect_s4_class(pcp, "PopulationChangeParameters")
  testthat::expect_true(length(pcp@values) > 0)
  testthat::expect_equal(length(pcp@values), sum(pcp@values == 0.0))
})
