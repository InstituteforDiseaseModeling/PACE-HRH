#' @exportMethod setFromVector
#'
setGeneric(
  name = "setFromVector",
  def = function(object, values)
  {
    standardGeneric("setFromVector")
  }
)

#' Set Population Change Parameters
#'
#' @param object PopulationChangeParameters object
#' @param values Vector of values to copy into PopulationChangeParameters object
#'
#' @return Updated \code{PopulationChangeParameters} object
#'
setMethod(
  f = "setFromVector",
  signature = c("PopulationChangeParameters", "numeric"),
  definition = function(object, values)
  {
    assertthat::assert_that(length(values) == 21)
    assertthat::assert_that(is.numeric(values) == TRUE)

    # Clear any names on the vector
    names(values) <- NULL

    object@values = values
    return(object)
  }
)

#' Set Pyramid Age Values
#'
#' @param object PopulationPyramid object
#' @param values Vector of values to copy into PopulationPyramid object
#'
#' @return Updated \code{PopulationPyramid} object
#'
setMethod(
  f = "setFromVector",
  signature = c("PopulationPyramid", "numeric"),
  definition = function(object, values)
  {
    assertthat::assert_that(length(values) == length(globalPackageEnvironment$ages))
    assertthat::assert_that(is.numeric(values) == TRUE)

    # Clear any names on the vector
    names(values) <- NULL

    object@values = values
    return(object)
  }
)
