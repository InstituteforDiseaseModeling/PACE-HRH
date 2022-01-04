#' Task Parameters Class
#'
#' @slot values numeric
#' @slot labels character
#'
#' @return Class of type \code{TaskParameters}
#'
#' @export TaskParameters
#' @exportClass TaskParameters
#'
TaskParameters <- setClass(
  "TaskParameters",

  slots = c(
    values = "numeric",
    labels = "character"
  ),

  prototype = list(values = NULL,
                   labels = NULL),

  validity = function(object){
    if (length(object@values) != length(object@labels)){
      return("values and labels vectors must be the same length")
    } else {
      return(TRUE)
    }
  }
)
