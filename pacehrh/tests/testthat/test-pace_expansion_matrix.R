library(pacehrh)

withr::local_dir("..")

test_that("Expansion matrix: basic generation", {
  breaks <- c(0,4,9,14,19,24)
  ages <- 0:100
  m <- pacehrh:::.generateExpansionMatrix(ages = ages, breaks = breaks)

  # Every row should have exactly one 1 and the rest zeros
  testthat::expect_true(all(apply(m, 1, sum) == 1))

  # The cumulative sum of the number of 1's in each column should track the
  # bounds parameter
  x <- cumsum(apply(m, 2, sum)) - 1
  x <- x[1:length(x) - 1]
  testthat::expect_identical(x, breaks)
})

test_that("Expansion matrix: NULL and zero-length breaks parameter", {
  breaks <- NULL
  ages <- 0:100
  m <- pacehrh:::.generateExpansionMatrix(ages = ages, breaks = breaks)

  # R x 1 matrix
  testthat::expect_equal(dim(m), c(length(ages), 1))

  # All 1s
  testthat::expect_true(all(m[,1] == 1))

  breaks <- vector()
  ages <- 0:10
  m <- pacehrh:::.generateExpansionMatrix(ages = ages, breaks = breaks)

  # R x 1 matrix
  testthat::expect_equal(dim(m), c(length(ages), 1))

  # All 1s
  testthat::expect_true(all(m[,1] == 1))
})
