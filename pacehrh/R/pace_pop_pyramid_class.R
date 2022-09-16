.defaultPyramidSize <- length(GPE$ages)

#' Population Pyramid Class
#'
#' @slot length numeric
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
    length = "numeric",
    values = "numeric"
  ),

  prototype = list(values = replicate(.defaultPyramidSize, 0.0),
                   length = .defaultPyramidSize)
)

setValidity("PopulationPyramid",
            function(object){
              if (length(object@values) == object@length) return(TRUE)
              else paste("wrong length")
            })
