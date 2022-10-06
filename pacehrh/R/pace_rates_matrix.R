#' Generate A Matrix Of Stochastically Varied Population Change Rates
#'
#' @param label Lookup string for the rates type
#'
#' @return Rates matrix
generateRatesMatrix <- function(label = NULL){
  pars <- GPE$stochasticParams
  years <- BVE$years

  if (is.null(label)){
    return(NULL)
  }

  if (!assertthat::is.string(label)){
    traceMessage("Non-string label passed to generateRatesMatrix()")
    return(NULL)
  }

  rates <- BVE$populationChangeRates[[label]]

  if (is.null(rates)){
    return(NULL)
  }

  m <- .generateRatesMatrix(
    pars,
    years,
    rates,
    stochasticity = TRUE,
    seed = NULL,
    optConstantFertility = ifelse(BVE$scenario$o_Fertility_decr == TRUE, FALSE, TRUE)
  )

  return(m)
}

.generateRatesMatrix <-
  function(pars,
           years,
           rates,
           stochasticity = TRUE,
           seed = NULL,
           optConstantFertility = FALSE) {
    # Grab the initial fertility rates, then apply stochastics
    initRates <- rates$bandedRates$initValues
    rateNames <- rates$bandedRates$labels

    # Initialize a rates matrix
    nRows = length(initRates)
    nCols = length(years)
    m <-
      matrix(
        nrow = nRows,
        ncol = nCols,
        dimnames = list(rateNames, as.character(years))
      )

    # Grab the change rates values (deltaRatios)
    # If fertility rate changes are turned off, assume a delta ratio of 1.
    # (The same change rate year over year, but still subject to stochastic
    # tweaks if stochasticity == TRUE.)

    deltaRatios <- rates$bandedRates$changeRates

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

    if (rates$type == "Fertility"){
      p = pars[pars$Value == "Fertility rates", ]$p
    } else {
      p = pars[pars$Value == "Mortality rates", ]$p
    }

    p <- p * c(-1, 1)
    initRates <-
      initRates * (1 + runif(length(initRates), p[1], p[2]))

    m[, 1] <- initRates

    # Grab the stochastic parameters for fertility rates deltas.

    if (rates$type == "Fertility"){
      p = pars[pars$Value == "Annual delta fertility rates", ]$p
      q = pars[pars$Value == "Annual delta fertility rates", ]$q
    } else {
      p = pars[pars$Value == "Annual delta mortality rates", ]$p
      q = pars[pars$Value == "Annual delta mortality rates", ]$q
    }

    lims = p * q * c(-1, 1)

    # TODO: make all the rtruncnorm calls at once, build a lookup table

    # Apply stochastic tweaks to every year of the time series
    for (j in 2:nCols) {
      e <- truncnorm::rtruncnorm(
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

