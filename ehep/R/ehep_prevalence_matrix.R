#' Generate A Matrix Of Stochastically Varied Prevalence/Incidence Rates
#'
#' @return Rates matrix
generatePrevalenceRatesMatrix <- function(){
  # Gather stuff we're going to need
  b <- baseValuesEnvironment
  e <- epsilonValuesEnvironment

  pars <- GPE$stochasticParams
  years <- GPE$years
  tasks <- GPE$taskData

  indexes <- which(GPE$taskData$applyStochasticity)

  tasks <-
    GPE$taskData[indexes, c("Indicator",
                          "StartingRateInPop",
                          "AnnualDeltaRatio",
                          "ServiceCat")]

  nRows = NROW(tasks)
  nCols = length(years)

  # Grab the initial prevalence/incidence values, and apply a stochastic tweak
  p = pars[pars$Value == "Incidence rates", ]$p
  initRates <-
    .adjustInitialIncidenceRates(tasks$StartingRateInPop, nRows, p)

  # Initialize a rates matrix
  m <-
    matrix(
      nrow = nRows,
      ncol = nCols,
      dimnames = list(as.character(indexes), as.character(years))
    )
  m[, 1] <- initRates

  # Derive the rest of the matrix
  p = pars[pars$Value == "Annual delta incidence rates", ]$p
  q = pars[pars$Value == "Annual delta incidence rates", ]$q
  lims = p * q * c(-1, 1)

  deltaRatios <- tasks$AnnualDeltaRatio

  for (j in 2:nCols){
    e <- deltaRatios * truncnorm::rtruncnorm(
      nRows,
      mean = 0,
      sd = p,
      a = lims[1],
      b = lims[2]
    )
    deltas <- deltaRatios * (1 + e)
    m[ ,j] <- m[ ,j-1] * deltas
  }

  return(m)
}

.adjustInitialIncidenceRates <- function(rates, n, p){
  p <- p * c(-1, 1)
  return(rates * (1 + runif(length(rates), p[1], p[2])))
}
