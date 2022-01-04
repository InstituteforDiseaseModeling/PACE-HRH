#' @exportMethod getMortalityRates
#'
setGeneric(
  name = "getMortalityRates",
  def = function(object)
  {
    standardGeneric("getMortalityRates")
  }
)

#' Get Mortality Rates From A PopulationChangeParameters Object
#'
#' @param object \code{PopulationChangeParameters} object
#'
#' @return Labeled vector of mortality rates
#'
setMethod(
  f = "getMortalityRates",
  signature = c("PopulationChangeParameters"),
  definition = function(object)
  {
    x <- object@values[8:15]
    names(x) <- .VarNames[8:15]
    return(x)
  }
)
