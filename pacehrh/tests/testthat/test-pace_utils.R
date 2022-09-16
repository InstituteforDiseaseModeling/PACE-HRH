library(ehep)

withr::local_dir("..")

test_that("Utilities: is.blank", {
  testthat::expect_true(ehep:::is.blank(""))
  testthat::expect_true(ehep:::is.blank(NULL))
  testthat::expect_true(ehep:::is.blank(NA))
  testthat::expect_false(ehep:::is.blank("notablank"))
  testthat::expect_false(ehep:::is.blank(0))
  testthat::expect_false(ehep:::is.blank(TRUE))
})
