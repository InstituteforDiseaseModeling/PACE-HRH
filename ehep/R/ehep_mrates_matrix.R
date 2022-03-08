#' Generate A Matrix Of Stochastically Varied Mortality Rates
#'
#' @return Rates matrix
generateMortalityRatesMatrix <- function(){
  pars <- GPE$stochasticParams
  years <- GPE$years
  pcp <- BVE$populationChangeParameters

  m <- .generateMortalityRatesMatrix(
    pars,
    years,
    pcp,
    stochasticity = TRUE,
    seed = NULL
  )

  return(m)
}

.generateMortalityRatesMatrix <- function(pars,
                                          years,
                                          pcp,
                                          stochasticity = TRUE,
                                          seed = NULL) {
  initValsBase <- pcp$initValues
  deltasBase <- pcp$changeRates

  # Grab the initial mortality rates, then apply stochastics
  initRates <- getMortalityRates(initValsBase)
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

  if (stochasticity == FALSE){
    m[, 1] <- initRates

    deltaRatios <- getMortalityRates(deltasBase)

    # Apply the same delta ratio to every year of the time series
    for (j in 2:nCols) {
      m[, j] <- m[, j - 1] * deltaRatios
    }

    return(m)
  }

  p = pars[pars$Value == "Mortality rates", ]$p
  p <- p * c(-1, 1)
  initRates <-
    initRates * (1 + runif(length(initRates), p[1], p[2]))

  m[, 1] <- initRates

  # Grab the mortality rates deltas base values and stochastic parameters
  deltaRatios <- getMortalityRates(deltasBase)
  p = pars[pars$Value == "Annual delta mortality rates", ]$p
  q = pars[pars$Value == "Annual delta mortality rates", ]$q
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
