# The functions in this file are for internal sanity checks. Some have extensive
# side effects. For example, .checkScenarios() not only checks that the
# GPE$scenarios variable has been assigned - ie is not NULL - but also attempts
# to initialize the variable if it can.

.checkScenarios <- function(autoCorrect = TRUE){
  if (is.null(GPE$scenarios)) {
    if (autoCorrect){
      InitializeScenarios()

      if (is.null(GPE$scenarios)) {
        traceMessage(paste0("Failed to load scenarios info from ", GPE$inputExcelFile))
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }

  # TODO: Insert checks for size and shape

  return(TRUE)
}
