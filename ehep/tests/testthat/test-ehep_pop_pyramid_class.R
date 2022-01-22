library(ehep)

withr::local_dir("..")

test_that("PopulationPyramid class: create", {
  pp <- ehep::PopulationPyramid()
  testthat::expect_s4_class(pp, "PopulationPyramid")
  testthat::expect_equal(length(pp@values), pp@length)
  testthat::expect_equal(length(pp@values), length(ehep:::GPE$ages))
})

test_that("PopulationPyramid class: bad create", {
  # Population vectors default to length = 101
  testthat::expect_error(pp <-
                           ehep::PopulationPyramid(values = replicate(10, 42)),
                         regexp = "wrong length")
})


