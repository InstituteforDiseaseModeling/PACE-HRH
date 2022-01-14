#' @exportMethod setMortalityRates
#'
setGeneric(
  name = "setMortalityRates",
  def = function(object, values)
  {
    standardGeneric("setMortalityRates")
  }
)

#' Set Mortality Rates On A PopulationChangeParameters Object
#'
#' @param object \code{PopulationChangeParameters} object
#' @param values
#'
#' @return Updated \code{PopulationChangeParameters} object
#'
setMethod(
  f = "setMortalityRates",
  signature = c("PopulationChangeParameters", "numeric"),
  definition = function(object, values)
  {
    assertthat::assert_that(length(values) == 8)
    object@values[8:15] <- values
    return(object)
  }
)
