#' @exportMethod getFertilityRates
#'
setGeneric(
  name = "getFertilityRates",
  def = function(object)
  {
    standardGeneric("getFertilityRates")
  }
)

#' Get Fertility Rates From A PopulationChangeParameters Object
#'
#' @param object \code{PopulationChangeParameters} object
#'
#' @return Labeled vector of fertility rates
#'
setMethod(
  f = "getFertilityRates",
  signature = c("PopulationChangeParameters"),
  definition = function(object)
  {
    x <- object@values[c(4,16:21)]
    names(x) <- .PcpVarNames[c(4,16:21)]
    return(x)
  }
)
