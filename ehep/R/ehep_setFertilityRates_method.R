# #' @exportMethod setFertilityRates
#'
methods::setGeneric(
  name = "setFertilityRates",
  def = function(object, values)
  {
    standardGeneric("setFertilityRates")
  }
)

#' Set Fertility Rates On A PopulationChangeParameters Object
#'
#' @param object \code{PopulationChangeParameters} object
#' @param values
#'
#' @return Updated \code{PopulationChangeParameters} object
#'
methods::setMethod(
  f = "setFertilityRates",
  signature = c("PopulationChangeParameters", "numeric"),
  definition = function(object, values)
  {
    assertthat::assert_that(length(values) == 7)
    object@values[c(4,16:21)] <- values
    return(object)
  }
)
