library(ehep)

withr::local_dir("..")

test_that("Fertility rates matrix: basic", {
  testthat::expect_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"

  pars <- ehep:::loadStochasticParameters(sheetName = "TEST_StochasticParms")
  pcp <- ehep:::loadPopulationChangeParameters(sheetName = "TEST_PopValues")
  years <- ehep:::GPE$years

  testthat::expect_named(pcp, c("initValues", "changeRates"))

  # CASE A

  mBaseline <-
    ehep:::.generateFertilityRatesMatrix(pars,
                                         years,
                                         pcp,
                                         stochasticity = FALSE,
                                         optConstantFertility = TRUE)

  nRows <- dim(mBaseline)[1]
  nCols <- dim(mBaseline)[2]
  testthat::expect_equal(nRows, length(ehep:::getFertilityRates(pcp$initValues)))
  testthat::expect_equal(nCols, length(years))

  # In the baseline matrix - no stochasticity, constant fertility rates - the
  # fertility rates don't change from year to year.

  for (i in 1:nRows) {
    rowData <- mBaseline[i, ]
    initVal <- rowData[1]
    testthat::expect_true(all(rowData == initVal))
  }

  # CASE B

  mDecreasing <-
    ehep:::.generateFertilityRatesMatrix(pars,
                                         years,
                                         pcp,
                                         stochasticity = FALSE,
                                         optConstantFertility = FALSE)

  nRows <- dim(mDecreasing)[1]
  nCols <- dim(mDecreasing)[2]
  testthat::expect_equal(nRows, length(ehep:::getFertilityRates(pcp$initValues)))
  testthat::expect_equal(nCols, length(years))

  # In the decreasing fertility matrix without stochasticity the fertility rates
  # change by the same proportional amount from year to year.

  for (i in 1:nRows) {
    x <- mDecreasing[i, 2:nCols]
    y <- mDecreasing[i, 1:(nCols - 1)]

    ratios <- x / y
    initRatio <- x[1] / y[1]

    testthat::expect_true(all(abs(ratios - initRatio) < 1.0e-15))
  }

  # CASE C

  # Full stochasticity, decreasing fertility rates

  e <- vector()

  for (n in 1:10) {
    mFull <-
      ehep:::.generateFertilityRatesMatrix(pars,
                                           years,
                                           pcp,
                                           stochasticity = TRUE,
                                           optConstantFertility = FALSE)

    for (i in 1:nRows) {
      x <- mFull[i, 2:nCols]
      y <- mFull[i, 1:(nCols - 1)]
      e <- c(e, x / y)
    }
  }

  # Check that the mean and variance are no more than 5% off the expected values
  p = pars[pars$Value == "Annual delta fertility rates",]$p
  q = pars[pars$Value == "Annual delta fertility rates",]$q

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

  # print(paste("mean(obs)", mean(e), "mean(exp)", 0.98, "%delta", abs(mean(e) - 0.98)/0.98))
  # print(paste("sd(obs)", sd(e), "sd(exp)", sdExpected, "%delta", abs(sd(e) - sdExpected)/sdExpected))

  mExpected = 0.98 # Value hard-wired into the simple_config spreadsheet

  testthat::expect_true(abs(mean(e) - mExpected)/mExpected < 0.05)
  testthat::expect_true(abs(sd(e) - sdExpected)/sdExpected < 0.05)

  # Save a graph to eyeball for general shape, etc
  png("graph_001.png")
  hist(e, breaks = 25)
  dev.off()
})
