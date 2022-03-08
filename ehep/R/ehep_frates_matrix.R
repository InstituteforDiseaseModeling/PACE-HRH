#' Generate A Matrix Of Stochastically Varied Fertility Rates
#'
#' @return Rates matrix
generateFertilityRatesMatrix <- function(){
  pars <- GPE$stochasticParams
  years <- GPE$years
  pcp <- BVE$populationChangeParameters

  m <- .generateFertilityRatesMatrix(
    pars,
    years,
    pcp,
    stochasticity = TRUE,
    seed = NULL,
    optConstantFertility = ifelse(BVE$scenario$o_Fertility_decr == TRUE, FALSE, TRUE)
  )

  return(m)
}

.generateFertilityRatesMatrix <-
  function(pars,
           years,
           pcp,
           stochasticity = TRUE,
           seed = NULL,
           optConstantFertility = FALSE) {
    initValsBase <- pcp$initValues
    deltasBase <- pcp$changeRates

    # Grab the initial fertility rates, then apply stochastics
    initRates <- getFertilityRates(initValsBase)
    rateNames <- names(initRates)

    # Initialize a rates matrix
    nRows = length(initRates)
    nCols = length(years)
    m <-
      matrix(
        nrow = nRows,
        ncol = nCols,
        dimnames = list(rateNames, as.character(years))
      )

    # Grab the fertility rates deltas base values. If fertility rate changes are
    # turned off, assume a delta ratio of 1. (Same fertility rate year over
    # year, but still subject to stochastic tweaks if stochasticity == TRUE.)

    deltaRatios <- getFertilityRates(deltasBase)

    if (optConstantFertility == TRUE) {
      deltaRatios[1:length(deltaRatios)] <- 1
    }

    if (stochasticity == FALSE){
      m[, 1] <- initRates

      # Apply the same delta ratio to every year of the time series
      for (j in 2:nCols) {
        m[, j] <- m[, j - 1] * deltaRatios
      }

      return(m)
    }

    p = pars[pars$Value == "Fertility rates", ]$p
    p <- p * c(-1, 1)
    initRates <-
      initRates * (1 + runif(length(initRates), p[1], p[2]))

    m[, 1] <- initRates

    # Grab the stochastic parameters for fertility rates deltas.

    p = pars[pars$Value == "Annual delta fertility rates", ]$p
    q = pars[pars$Value == "Annual delta fertility rates", ]$q
    lims = p * q * c(-1, 1)

    # TODO: make all the rtruncnorm calls at once, build a lookup table

    # Apply stochastic tweaks to every year of the time series
    for (j in 2:nCols) {
      e <- deltaRatios * truncnorm::rtruncnorm(
        length(deltaRatios),
        mean = 0,
        sd = p,
        a = lims[1],
        b = lims[2]
      )
      deltas <- deltaRatios * (1 + e)
      m[, j] <- m[, j - 1] * deltas
    }

    return(m)
  }

