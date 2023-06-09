.varyInitialIncidenceRates <- function(rates, p){
  p <- p * c(-1, 1)
  return(rates * (1 + runif(length(rates), p[1], p[2])))
}

#' Generate A Matrix Of Stochastically Varied Prevalence/Incidence Rates
#'
#' @param debugEnv Caller-create environment into which the
#'   `generatePrevalenceRatesMatrix()` call can insert some debugging
#'   information to return to the caller. Intended for unit testing.
#'
#' @return Rates matrix
#'
#' @noRd
generatePrevalenceRatesMatrix <- function(debugEnv = NULL){
  if (!GPE$stochasticity){
    return(.generateNonStochPrm(debugEnv))
  }

  # Gather stuff we're going to need
  pars <- BVE$stochasticParams
  years <- BVE$years

  tasks <-
    BVE$taskData[, c(
      "Indicator",
      "StartingRateInPop",
      "AnnualDeltaRatio",
      "ServiceCat",
      "RelevantPop",
      "applyStochasticity"
    )]

  nRows = NROW(tasks)
  nCols = length(years)

  # Mask for tasks affected by prevalence stochasticity
  sMask <- tasks$applyStochasticity

  # Grab the initial prevalence/incidence values, and apply a stochastic tweak
  p = pars[pars$Value == "Incidence rates", ]$p

  initRates <- rep(1.0, nRows)
  initRates[sMask] <-
    .varyInitialIncidenceRates(tasks[sMask,]$StartingRateInPop, p)

  # Calculate min/max constraints on rates
  limits <- .getRatesLimits("Incidence")
  rateLimits <- .getMinMaxRates(initRates, limits)
  maskedRateLimits <- lapply(rateLimits, function(x){x[sMask]})

  # Write some information back to the user-provided debugging environment
  if (!is.null(debugEnv)) {
    assign("rateLimits", rateLimits, envir = debugEnv)
  }

  # Initialize a rates matrix
  m <-
    matrix(
      data = 1.0,
      nrow = nRows,
      ncol = nCols,
      dimnames = list(tasks$Indicator, as.character(years))
    )
  m[, 1] <- initRates

  # Derive the rest of the matrix
  p = pars[pars$Value == "Annual delta incidence rates", ]$p
  q = pars[pars$Value == "Annual delta incidence rates", ]$q
  lims = p * q * c(-1, 1)

  deltaRatios <- .computeAdjustedDeltaRatios(tasks)

  for (j in 2:nCols){
    e <- truncnorm::rtruncnorm(
      sum(sMask),
      mean = 0,
      sd = p,
      a = lims[1],
      b = lims[2]
    )
    deltas <- deltaRatios[sMask] * (1 + e)
    m[sMask, j] <- .applyRateLimits(m[sMask, j-1] * deltas, maskedRateLimits)
  }

  return(m)
}

.computeAdjustedDeltaRatios <- function(tasks) {
  deltaRatios <- tasks$AnnualDeltaRatio

  # Set annual prevalence deltas to unity if the scenario requires it
  if (BVE$scenario$o_MHIVTB_decr == FALSE){
    # Determine which rows refer to Malaria/HIV/TB
    mhivtbMask <-
      (
        tasks$ServiceCat == "Malaria" |
          tasks$ServiceCat == "Tuberculosis" | tasks$ServiceCat == "HIV"
      )

    deltaRatios[mhivtbMask] <- 1
  }

  if (BVE$scenario$o_ChildDis_decr == FALSE){
    # Determine which rows refer to childhood diseases
    childDisMask <- (tasks$RelevantPop == "1-4")

    deltaRatios[childDisMask] <- 1
  }

  return(deltaRatios)
}

.generateNonStochPrm <- function(debugEnv = NULL){
  years <- BVE$years

  tasks <-
    BVE$taskData[, c(
      "Indicator",
      "StartingRateInPop",
      "AnnualDeltaRatio",
      "ServiceCat",
      "RelevantPop",
      "applyStochasticity"
    )]

  nRows = NROW(tasks)
  nCols = length(years)

  # Mask for tasks affected by prevalence stochasticity
  sMask <- tasks$applyStochasticity

  initRates <- rep(1.0, nRows)
  initRates[sMask] <- tasks[sMask,]$StartingRateInPop

  # Calculate min/max constraints on rates
  limits <- .getRatesLimits("Incidence")
  rateLimits <- .getMinMaxRates(initRates, limits)
  maskedRateLimits <- lapply(rateLimits, function(x){x[sMask]})

  if (!is.null(debugEnv)) {
    assign("rateLimits", rateLimits, envir = debugEnv)
  }

  # Initialize a rates matrix
  m <-
    matrix(
      data = 1.0,
      nrow = nRows,
      ncol = nCols,
      dimnames = list(tasks$Indicator, as.character(years))
    )
  m[, 1] <- initRates

  deltaRatios <- .computeAdjustedDeltaRatios(tasks)

  # Derive the rest of the matrix
  deltas <- deltaRatios[sMask]
  for (j in 2:nCols){
    m[sMask, j] <- .applyRateLimits(m[sMask, j-1] * deltas, maskedRateLimits)
  }

  return(m)
}
