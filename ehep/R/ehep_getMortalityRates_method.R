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
    mrateIndexes <- c(8:13, 22:29)
    x <- object@values[mrateIndexes]
    names(x) <- .PcpVarNames[mrateIndexes]
    return(x)
  }
)
