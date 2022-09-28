# #' @exportMethod getValue
#'
methods::setGeneric(
  name = "getValue",
  def = function(object, label)
  {
    standardGeneric("getValue")
  }
)

methods::setMethod(
  f = "getValue",
  signature = c("TaskParameters", "character"),
  definition = function(object, label)
  {
    assertthat::is.string(label)

    index <- which(object@labels == label)

    if (length(index) == 1){
      return(object@values[index])
    } else if (length(index) == 0){
      traceMessage(paste("Invalid task label: ", label, sep = ""))
      return(NULL)
    } else {
      traceMessage(paste("Duplicate task label: ", label, ". Using first instance.", sep = ""))
      return(object@values[index[1]])
    }
  }
)
