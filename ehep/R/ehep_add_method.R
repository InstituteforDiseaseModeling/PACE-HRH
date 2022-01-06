#' @exportMethod add
#'
setGeneric(
  name = "add",
  def = function(obj1, obj2)
  {
    standardGeneric("add")
  }
)

#' Add Two PopulationChangeParameters Objects Together
#'
#' @param obj1 \code{PopulationChangeParameters} object
#' @param obj2 \code{PopulationChangeParameters} object
#'
#' @return Updated \code{PopulationChangeParameters} object
#'
setMethod(
  f = "add",
  signature = c("PopulationChangeParameters", "PopulationChangeParameters"),
  definition = function(obj1, obj2)
  {
    obj1@values <- obj1@values + obj2@values
    return(obj1)
  }
)

#' Add Two PopulationPyramid Objects Together
#'
#' @param obj1 \code{PopulationPyramid} object
#' @param obj2 \code{PopulationPyramid} object
#'
#' @return Updated \code{PopulationPyramid} object
#'
setMethod(
  f = "add",
  signature = c("PopulationPyramid", "PopulationPyramid"),
  definition = function(obj1, obj2)
  {
    obj1@values <- obj1@values + obj2@values
    return(obj1)
  }
)

#' Add Two TaskParameters Objects Together
#'
#' @param obj1 \code{TaskParameters} object
#' @param obj2 \code{TaskParameters} object
#'
#' @return Updated \code{TaskParameters} object
#'
setMethod(
  f = "add",
  signature = c("TaskParameters", "TaskParameters"),
  definition = function(obj1, obj2)
  {
    assertthat::assert_that(length(obj1@values) == length(obj2@values))

    obj1@values <- obj1@values + obj2@values
    return(obj1)
  }
)
