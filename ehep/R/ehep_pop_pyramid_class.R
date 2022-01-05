#' Population Pyramid Class
#'
#' @slot values numeric
#'
#' @return Class of type \code{PopulationPyramid}
#'
#' @export PopulationPyramid
#' @exportClass PopulationPyramid
#'
PopulationPyramid <- setClass(
  # Set the name for the class
  "PopulationPyramid",

  # Define the variables
  slots = c(
    values = "numeric"
  ),

  prototype = list(values = replicate(length(globalPackageEnvironment$ages), 0.0))
)

