#' Generate A Matrix Of Stochastically Varied Fertility Rates
#'
#' @return Rates matrix
generateFertilityRatesMatrix <- function(){
  # Gather stuff we're going to need
  b <- baseValuesEnvironment
  e <- epsilonValuesEnvironment
  pars <- globalPackageEnvironment$stochasticParams
  years <- globalPackageEnvironment$years

  initValsBase <- b$populationChangeParameters$initValues
  initValsEpsilons <- e$populationChangeParameters$initValues
  deltasBase <- b$populationChangeParameters$changeRates
  deltasEpsilons <- e$populationChangeParameters$changeRates

  # Grab the initial fertility rates, then apply stochastics
  initRates <- getFertilityRates(initValsBase)
  rateNames <- names(initRates)

  p = pars[pars$Value == "Fertility rates",]$p
  p <- p * c(-1, 1)
  initRates <- initRates * (1 + runif(length(initRates), p[1], p[2]))

  # Initialize a rates matrix
  nRows = length(initRates)
  nCols = length(years)
  m <- matrix(nrow = nRows, ncol = nCols, dimnames = list(rateNames, as.character(years)))
  m[,1] <- initRates

  # If fertility rate changes are turned off, propagate the initial rate to all years
  if (b$scenario$o_Fertility_decr == FALSE){
    for (j in 2:nCols){
      m[,j] <- initRates
    }
    return(m)
  }

  # Grab the fertility rates deltas base values and stochastic parameters
  deltaRatios <- getFertilityRates(deltasBase)
  p = pars[pars$Value == "Annual delta fertility rates",]$p
  q = pars[pars$Value == "Annual delta fertility rates",]$q
  lims = p * q * c(-1, 1)

  # TODO: make all the rtruncnorm calls at once, build a lookup table

  # Apply stochastic tweaks to every year of the time series
  for (j in 2:nCols){
    e <- deltaRatios * truncnorm::rtruncnorm(
      length(deltaRatios),
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
