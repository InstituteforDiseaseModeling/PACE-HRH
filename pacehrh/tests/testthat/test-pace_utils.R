library(pacehrh)

withr::local_dir("..")

test_that("Utilities: is.blank", {
  testthat::expect_true(pacehrh:::is.blank(""))
  testthat::expect_true(pacehrh:::is.blank(NULL))
  testthat::expect_true(pacehrh:::is.blank(NA))
  testthat::expect_false(pacehrh:::is.blank("notablank"))
  testthat::expect_false(pacehrh:::is.blank(0))
  testthat::expect_false(pacehrh:::is.blank(TRUE))
})
