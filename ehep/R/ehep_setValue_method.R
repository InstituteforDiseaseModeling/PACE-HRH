#' @exportMethod setValue
#'
setGeneric(
  name = "setValue",
  def = function(object, label, value)
  {
    standardGeneric("setValue")
  }
)

#' Set Population Change Parameter Values
#'
#' @param object \code{PopulationChangeParameters} object
#' @param label Name of variable to write
#' @param value Value to write to variable
#'
#' @return Updated \code{PopulationChangeParameters} object
#'
setMethod(
  f = "setValue",
  signature = c("PopulationChangeParameters", "character", "numeric"),
  definition = function(object, label, value)
  {
    assertthat::is.string(label)
    assertthat::is.number(value)

    if (!is.na(.VarLookup[label])){
      object@values[.VarLookup[label]] <- value
    }

    return(object)
  }
)
