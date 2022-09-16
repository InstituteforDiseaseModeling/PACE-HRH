#' Turn Package Tracing On/Off
#'
#' @param state TRUE/FALSE to set tracing state, NULL or empty to return
#' current state.
#'
#' @return Original state
#' @export
#'
#' @examples
#' oldState <- Trace()
#' print(oldState)
#'
Trace <- function(state = NULL){
  originalState <- GPE$traceState

  # IDEA: A different way to drive default behavior when Trace() is called
  # without parameters would be to use the hasArg() function.

  if (!is.null(state) & (typeof(state) == 'logical')){
    GPE$traceState <- state
  }

  invisible(originalState)
}

#' Log A Trace Message
#'
#' @param msgString Trace message
traceMessage <- function(msgString){
  if (GPE$traceState == TRUE){
    # o <- sys.calls()[[sys.nframe() - 1]]
    # fname <- deparse(o[[1]])
    # message(paste0(fname, ": ", msgString))
    # message(traceback(x = 0))
    message(msgString)
  }
}
