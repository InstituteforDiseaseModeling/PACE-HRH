library(pacehrh)

withr::local_dir("..")

test_that("Experiments: aggregate times", {
  testthat::expect_equal(pacehrh:::.computeTotalTimes(NULL), 0)

  badList <- list(notTime = pacehrh:::GPE$years)
  testthat::expect_equal(pacehrh:::.computeTotalTimes(badList), 0)

  # Create a test times list structure: two elements, Time and N, each containing
  # matrices of size {years x tasks}
  timeVal <- 100
  NVal <- 2
  years <- pacehrh:::GPE$years

  m <- length(years)
  n <- 2

  testTimes <- matrix(timeVal, nrow = m, ncol = n)
  testNs <- matrix(NVal, nrow = m, ncol = n)
  testResult <- vector(mode = "numeric", length = m)
  testResult[1:m] <- timeVal * n

  rownames(testTimes) <- years
  rownames(testNs) <- years
  names(testResult) <- years

  goodList <- list(Time = testTimes, N = testNs)
  testthat::expect_equal(pacehrh:::.computeTotalTimes(goodList), testResult)
})
