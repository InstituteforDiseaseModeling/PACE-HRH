library(pacehrh)

test_that("Iterator: basic operation", {
  n <- 10
  values <- runif(n)
  iterObj <- pacehrh:::iter(values)

  testthat::expect_true(!is.null(iterObj))
  testthat::expect_named(iterObj, c("state", "length", "recycle"))

  out <- vector()
  for (i in 1:n){
    out <- c(out, nextElem(iterObj))
  }

  testthat::expect_false(hasNext(iterObj))
  testthat::expect_error(nextElem(iterObj))
  testthat::expect_identical(out, values)
})

test_that("Iterator: recycling", {
  n <- 10
  m <- 3
  values <- runif(n)
  iterObj <- pacehrh:::iter(values, recycle = TRUE)

  testthat::expect_true(!is.null(iterObj))

  out <- vector()
  for (i in 1:(m*n)){
    out <- c(out, nextElem(iterObj))
  }

  expected <- do.call(c,lapply(seq_len(m),function(x){values}))

  testthat::expect_true(hasNext(iterObj))
  testthat::expect_identical(out, expected)
})
