#' Apply Stochasticity To Select Task Parameters
#'
#' @param tasks Task parameter value matrix
#'
#' @return Adjusted task parameters matrix
varyTaskValues <- function(tasks){
  pars <- GPE$stochasticParams

  # Tweak MinsPerContact value
  m <- tasks@values[,"MinsPerContact"]
  nonZeroMask <- ((!is.na(m)) & (m > 0))

  p = pars[pars$Value == "Minutes per contact",]$p

  # Generate log-normal distribution with mean = 1 and sd = p
  logm <- - (log(1 + p^2))/2
  logsd <- sqrt(log((1 + p^2)))
  samples <- rlnorm(sum(nonZeroMask), logm, logsd)

  # Use log-normal distribution to tweak MinsPerContact values
  m[nonZeroMask] <- samples * m[nonZeroMask]

  tasks@values[, "MinsPerContact"] <- m

  return(tasks)
}
