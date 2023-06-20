library(pacehrh)

withr::local_dir("..")

test_that("Rates limits", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE
  local_vars("inputExcelFile", envir = gpe)

  pacehrh::SetInputExcelFile("./simple_config/model_inputs.xlsx")

  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  pacehrh::InitializeStochasticParameters(
    stochasticParametersSheetName = "TEST_StochasticParms",
    changeRateLimitsSheetName = "ChangeRateLimits"
  )

  testthat::expect_true(!is.null(bve$changeRateLimits))

  l <- pacehrh:::.getRatesLimits("femaleFertility")
  testthat::expect_true(!is.na(l[[1]]))
  testthat::expect_true(!is.na(l[[2]]))
  testthat::expect_true(l[[1]] <= l[[2]])

  l <- pacehrh:::.getRatesLimits("maleMortality")
  testthat::expect_true(!is.na(l[[1]]))
  testthat::expect_true(!is.na(l[[2]]))
  testthat::expect_true(l[[1]] <= l[[2]])

  l <- pacehrh:::.getRatesLimits("incidence")
  testthat::expect_true(!is.na(l[[1]]))
  testthat::expect_true(!is.na(l[[2]]))
  testthat::expect_true(l[[1]] <= l[[2]])

  l <- pacehrh:::.getRatesLimits("notalabel")
  testthat::expect_true(is.na(l[[1]]))
  testthat::expect_true(is.na(l[[2]]))

  # Error case: NULL change rate limits table (perhaps through init failure)
  local_vars("changeRateLimits", envir = bve)
  bve$changeRateLimits <- NULL
  l <- pacehrh:::.getRatesLimits("femaleFertility")
  testthat::expect_true(is.na(l[[1]]))
  testthat::expect_true(is.na(l[[2]]))
})

test_that("Rates limits: no sheet", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE
  local_vars("inputExcelFile", envir = gpe)

  pacehrh::SetInputExcelFile("./simple_config/model_inputs.xlsx")

  # withr::defer(pacehrh::Trace(originalTraceState))
  # originalTraceState <- pacehrh::Trace(TRUE)

  testthat::expect_warning(
    {
      pacehrh::InitializeStochasticParameters(
        stochasticParametersSheetName = "TEST_StochasticParms",
        changeRateLimitsSheetName = "notasheet"
      )
    },
    regexp = "Could not read change rate limits sheet"
  )

  testthat::expect_true(is.null(bve$changeRateLimits))

  # If nothing has been defined, trying to read rate limits should return the
  # default "no limits" values.
  l <- pacehrh:::.getRatesLimits("femaleFertility")
  testthat::expect_true(is.na(l[[1]]))
  testthat::expect_true(is.na(l[[2]]))

  l <- pacehrh:::.getRatesLimits("notalabel")
  testthat::expect_true(is.na(l[[1]]))
  testthat::expect_true(is.na(l[[2]]))
})

test_that("Fertility rates matrix: basic", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/model_inputs.xlsx"

  pars <- pacehrh:::loadStochasticParameters(stochasticParametersSheetName = "TEST_StochasticParms")
  rates <- pacehrh:::loadPopulationChangeRates(sheetName = "TEST_PopValues")
  years <- pacehrh:::GPE$years

  # CASE A

  rates <- rates[["femaleFertility"]]

  mBaseline <-
    pacehrh:::.generateRatesMatrix(pars,
                                years,
                                rates,
                                stochasticity = FALSE,
                                optConstantFertility = TRUE)

  nRows <- dim(mBaseline)[1]
  nCols <- dim(mBaseline)[2]
  testthat::expect_equal(nRows, length(rates$bandedRates$initValues))
  testthat::expect_equal(nCols, length(years))

  # In the baseline matrix - no stochasticity, constant fertility rates - the
  # fertility rates don't change from year to year.

  results <- sapply(1:nRows, function(i){
    rowData <- mBaseline[i, ]
    initVal <- rowData[1]
    return(all(rowData == initVal))
  })

  testthat::expect_true(all(results))

  # CASE A-1 - Decreasing with tight rate cap

  limits <- c(0.99, 1.01)

  m <-
    pacehrh:::.generateRatesMatrix(pars,
                                   years,
                                   rates,
                                   limits = limits,
                                   stochasticity = FALSE,
                                   optConstantFertility = FALSE)

  testthat::expect_true(!is.null(m))

  # Test that all rates values are above the lower limit
  initValues <- m[, 1]
  tf <- sapply(2:dim(m)[2], function(j){
    return(all(m[,j] - (initValues * limits[1]) >= 0))
  })

  testthat::expect_true(all(tf))

  # Test that all rates values are below the upper limit
  initValues <- m[, 1]
  tf <- sapply(2:dim(m)[2], function(j){
    return(all((initValues * limits[2]) - m[,j] >= 0))
  })

  testthat::expect_true(all(tf))

  # CASE A-2 - Decreasing with tight rate cap and stochasticity

  limits <- c(0.99, 1.01)

  m <-
    pacehrh:::.generateRatesMatrix(pars,
                                   years,
                                   rates,
                                   limits = limits,
                                   stochasticity = TRUE,
                                   optConstantFertility = FALSE)

  testthat::expect_true(!is.null(m))

  # Test that all rates values are above the lower limit
  initValues <- m[, 1]
  tf <- sapply(2:dim(m)[2], function(j){
    return(all(m[,j] - (initValues * limits[1]) >= 0))
  })

  testthat::expect_true(all(tf))

  # Test that all rates values are below the upper limit
  initValues <- m[, 1]
  tf <- sapply(2:dim(m)[2], function(j){
    return(all((initValues * limits[2]) - m[,j] >= 0))
  })

  testthat::expect_true(all(tf))

  # CASE B

  mDecreasing <-
    pacehrh:::.generateRatesMatrix(pars,
                                years,
                                rates,
                                stochasticity = FALSE,
                                optConstantFertility = FALSE)

  nRows <- dim(mDecreasing)[1]
  nCols <- dim(mDecreasing)[2]
  testthat::expect_equal(nRows, length(rates$bandedRates$initValues))
  testthat::expect_equal(nCols, length(years))

  # In the decreasing fertility matrix without stochasticity the fertility rates
  # change by the same proportional amount from year to year.

  results <- sapply(2:(nRows-1), function(i){
    x <- mDecreasing[i, 2:nCols]
    y <- mDecreasing[i, 1:(nCols - 1)]

    ratios <- x / y
    initRatio <- x[1] / y[1]

    return(all(abs(ratios - initRatio) < 1.0e-15))
  })

  testthat::expect_true(all(results))

  # CASE C

  # Full stochasticity, decreasing fertility rates

  e <- vector()

  for (n in 1:10) {
    mFull <-
      pacehrh:::.generateRatesMatrix(pars,
                                  years,
                                  rates,
                                  stochasticity = TRUE,
                                  optConstantFertility = FALSE)

    nRows <- dim(mFull)[1]

    for (i in 2:(nRows-1)) {
      x <- mFull[i, 2:nCols]
      y <- mFull[i, 1:(nCols - 1)]
      e <- c(e, x / y)
    }
  }

  # Check that the mean and variance are no more than N% off the expected values
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
  png("graph_001.png")
  hist(e, breaks = 25, main = paste0("fertility rates stochastic distribution N = ", length(e)))
  dev.off()
})
