library(ehep)

withr::local_dir("..")

test_that("Mortality rates matrix: basic", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  pars <- ehep:::loadStochasticParameters(sheetName = "TEST_StochasticParms")
  pcp <- ehep:::loadPopulationChangeParameters(sheetName = "TEST_PopValues")
  years <- ehep:::GPE$years

  testthat::expect_named(pcp, c("initValues", "changeRates"))

  # CASE A

  mDecreasing <-
    ehep:::.generateMortalityRatesMatrix(pars,
                                         years,
                                         pcp,
                                         stochasticity = FALSE)

  nRows <- dim(mDecreasing)[1]
  nCols <- dim(mDecreasing)[2]
  testthat::expect_equal(nRows, length(ehep:::getMortalityRates(pcp$initValues)))
  testthat::expect_equal(nCols, length(years))

  # In the decreasing mortality matrix without stochasticity the mortality rates
  # change by the same proportional amount from year to year.

  for (i in 1:nRows) {
    x <- mDecreasing[i, 2:nCols]
    y <- mDecreasing[i, 1:(nCols - 1)]

    ratios <- x / y
    initRatio <- x[1] / y[1]

    testthat::expect_true(all(abs(ratios - initRatio) < 1.0e-15))
  }

  # CASE B

  # Full stochasticity

  e <- vector()

  for (n in 1:10) {
    mFull <-
      ehep:::.generateMortalityRatesMatrix(pars,
                                           years,
                                           pcp,
                                           stochasticity = TRUE)

    for (i in 1:nRows) {
      x <- mFull[i, 2:nCols]
      y <- mFull[i, 1:(nCols - 1)]
      e <- c(e, x / y)
    }
  }

  # Check that the mean and variance are no more than N% off the expected values
  p = pars[pars$Value == "Annual delta mortality rates",]$p
  q = pars[pars$Value == "Annual delta mortality rates",]$q

  m <- 0  # mean of normal distribution underlying truncated norm dist
  s <- p  # standard deviation
  lims <- (p * q) * c(-1, 1)
  a <- lims[1]
  b <- lims[2]

  # Small p => probability density; big P => cumulative density function
  pa <- dnorm(a, mean = m, sd = s)
  pb <- dnorm(b, mean = m, sd = s)
  Pa <- pnorm(a, mean = m, sd = s)
  Pb <- pnorm(b, mean = m, sd = s)

  f <- 1 - (((b * pb) - (a * pa))/(Pb - Pa)) - ((pb - pa)/(Pb - Pa))^2

  sdExpected <- sqrt((s^2)*f) # Expected sd of the truncated norm dist
  mExpected = 0.98 # Value hard-wired into the simple_config spreadsheet

  cat(paste(
    "\nz(mean) = ", round(abs(mean(e) - mExpected) / mExpected, digits = 5),
    " z(sd) = ", round(abs(sd(e) - sdExpected) / sdExpected, digits = 5),
    "\n",
    sep = ""
  ))

  testthat::expect_true(abs(mean(e) - mExpected)/mExpected < 0.05)
  testthat::expect_true(abs(sd(e) - sdExpected)/sdExpected < 0.10)

  # Save a graph to eyeball for general shape, etc
  png("graph_002.png")
  hist(e, breaks = 25, main = paste("mortality rates stochastic distribution N = ", length(e)))
  dev.off()
})
