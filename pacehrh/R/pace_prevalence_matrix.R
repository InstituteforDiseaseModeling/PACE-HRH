.varyInitialIncidenceRates <- function(rates, p){
  p <- p * c(-1, 1)
  return(rates * (1 + runif(length(rates), p[1], p[2])))
}

#' Generate A Matrix Of Stochastically Varied Prevalence/Incidence Rates
#'
#' @return Rates matrix
#'
#' @noRd
generatePrevalenceRatesMatrix <- function(){
  if (!GPE$stochasticity){
    return(.generateNonStochPrm())
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

  # Determine which rows refer to Malaria/HIV/TB
  mhivtbMask <-
    (
      tasks$ServiceCat == "Malaria" |
        tasks$ServiceCat == "Tuberculosis" | tasks$ServiceCat == "HIV"
    )

  # Determine which rows refer to childhood diseases
  childDisMask <- (tasks$RelevantPop == "1-4")

  # Grab the initial prevalence/incidence values, and apply a stochastic tweak
  p = pars[pars$Value == "Incidence rates", ]$p

  initRates <- rep(1.0, nRows)

  initRates[sMask] <-
    .varyInitialIncidenceRates(tasks[sMask,]$StartingRateInPop, p)

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

  # Grab the annual prevalence deltas
  deltaRatios <- tasks$AnnualDeltaRatio

  # Set annual prevalence deltas to unity if the scenario requires it
  if (BVE$scenario$o_MHIVTB_decr == FALSE){
    deltaRatios[mhivtbMask] <- 1
  }

  if (BVE$scenario$o_ChildDis_decr == FALSE){
    deltaRatios[childDisMask] <- 1
  }

  for (j in 2:nCols){
    e <- truncnorm::rtruncnorm(
      sum(sMask),
      mean = 0,
      sd = p,
      a = lims[1],
      b = lims[2]
    )
    deltas <- deltaRatios[sMask] * (1 + e)
    m[sMask, j] <- m[sMask, j-1] * deltas
  }

  return(m)
}

.generateNonStochPrm <- function(){
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

  # Determine which rows refer to Malaria/HIV/TB
  mhivtbMask <-
    (
      tasks$ServiceCat == "Malaria" |
        tasks$ServiceCat == "Tuberculosis" | tasks$ServiceCat == "HIV"
    )

  # Determine which rows refer to childhood diseases
  childDisMask <- (tasks$RelevantPop == "1-4")

  initRates <- rep(1.0, nRows)
  initRates[sMask] <- tasks[sMask,]$StartingRateInPop

  # Initialize a rates matrix
  m <-
    matrix(
      data = 1.0,
      nrow = nRows,
      ncol = nCols,
      dimnames = list(tasks$Indicator, as.character(years))
    )
  m[, 1] <- initRates

  # Grab the annual prevalence deltas
  deltaRatios <- tasks$AnnualDeltaRatio

  # Set annual prevalence deltas to unity if the scenario requires it
  if (BVE$scenario$o_MHIVTB_decr == FALSE){
    deltaRatios[mhivtbMask] <- 1
  }

  if (BVE$scenario$o_ChildDis_decr == FALSE){
    deltaRatios[childDisMask] <- 1
  }

  # Derive the rest of the matrix
  deltas <- deltaRatios[sMask]
  for (j in 2:nCols){
    m[sMask, j] <- m[sMask, j-1] * deltas
  }

  return(m)
}
