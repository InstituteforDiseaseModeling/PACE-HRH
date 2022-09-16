#' Task Parameters Class
#'
#' @slot values numeric matrix
#'
#' @return Class of type \code{TaskParameters}
#'
#' @export TaskParameters
#' @exportClass TaskParameters
#'
TaskParameters <- setClass(
  "TaskParameters",

  slots = c(
    values = "matrix"
  ),

  prototype = list(values = NULL),

  validity = function(object){
    if (!(is.numeric(object@values) & is.matrix(object@values))){
      return("Expecting numeric matrix")
    }

    return(TRUE)
  }
)
