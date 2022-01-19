#' Generate A Matrix Of Stochastically Varied Fertility Rates
#'
#' @return Rates matrix
generateFertilityRatesMatrix <- function(){
  # Gather stuff we're going to need
  pars <- GPE$stochasticParams
  years <- GPE$years

  initValsBase <- BVE$populationChangeParameters$initValues
  deltasBase <- BVE$populationChangeParameters$changeRates

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
  # if (b$scenario$o_Fertility_decr == FALSE){
  #   for (j in 2:nCols){
  #     m[,j] <- initRates
  #   }
  #   return(m)
  # }

  # Grab the fertility rates deltas base values and stochastic parameters.
  # If fertility rate changes are turned off, assume a delta ratio of 1, but
  # allow that unity value to be tweaked up and down over time.

  if (BVE$scenario$o_Fertility_decr == FALSE){
    deltaRatios <- 1
  } else {
    deltaRatios <- getFertilityRates(deltasBase)
  }

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
