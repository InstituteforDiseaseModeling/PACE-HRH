#' Apply Stochasticity To Select Task Parameters
#'
#' @param tasks Task parameter value matrix
#'
#' @return Adjusted task parameters matrix
#'
#' @noRd
varyTaskValues <- function(tasks){
  # Do nothing if stochasticity is turned off
  if (!GPE$stochasticity){
    return(tasks)
  }

  pars <- BVE$stochasticParams

  # Tweak MinsPerContact value
  m <- tasks[,"MinsPerContact"]
  nonZeroMask <- ((!is.na(m)) & (m > 0))

  p = pars[pars$Value == "Minutes per contact",]$p

  # Generate log-normal distribution with mean = 1 and sd = p
  logm <- - (log(1 + p^2))/2
  logsd <- sqrt(log((1 + p^2)))
  samples <- rlnorm(sum(nonZeroMask), logm, logsd)

  # Use log-normal distribution to tweak MinsPerContact values
  m[nonZeroMask] <- samples * m[nonZeroMask]

  tasks[, "MinsPerContact"] <- m

  return(tasks)
}
