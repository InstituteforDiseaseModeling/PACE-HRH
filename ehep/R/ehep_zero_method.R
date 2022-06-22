# #' @exportMethod zero
#'
setGeneric(
  name = "zero",
  def = function(object)
  {
    standardGeneric("zero")
  }
)

#' Zero A PopulationChangeParameters Object
#'
#' @param object \code{PopulationChangeParameters} object
#'
#' @return Updated \code{PopulationChangeParameters} object
#'
setMethod(
  f = "zero",
  signature = c("PopulationChangeParameters"),
  definition = function(object)
  {
    object@values <- replicate(length(.PcpVarNames), 0.0)
    return(object)
  }
)
