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

.checkPopRates <- function(rates, type, sex){
  if (nrow(rates) == 0){
    return(FALSE)
  }

  # Special case: one band covering ALL ages
  if (nrow(rates) == 1){
    if (rates$BandStart[1] == GPE$ageMin && rates$BandEnd[1] == GPE$ageMax){
      return(TRUE)
    } else {
      traceMessage(paste0("Incomplete BandStart/BandEnd range (", rates$BandStart[1], ", ", rates$BandEnd[1], ")"))
      return(FALSE)
    }
  }

  rates <- rates[order(rates$BandStart),]

  if (is.unsorted(rates$BandEnd, na.rm = TRUE)){
    traceMessage(paste0("Out of order BandEnd values for (", type, ", ", sex, ")"))
    return(FALSE)
  }

  # The following code checks that the bands interlace correctly.
  #
  # Say
  #    BandStart <- c(0,1,5,10,15,20,35,50,60,75) and
  #    BandEnd <- c(0,4,9,14,19,34,49,59,74,100)
  #
  # The interlace of these two vectors is:
  # c(0,0,1,4,5,9,10,14,15,19,20,34,35,49,50,59,60,74,75,100)
  #
  # This is a good sequence if
  #    (1) it is ordered,
  #    (2) it starts at AgeMin and ends at AgeMax, and
  #    (3) every other gap = 1 (consecutive numbers)

  assertthat::assert_that(length(rates$BandStart) == length(rates$BandEnd))

  x <- vector(mode = "numeric", length = 2 * length(rates$BandStart))
  oddMask <- seq(1,length(x),2)
  evenMask <- seq(2,length(x),2)
  x[oddMask] <- rates$BandStart
  x[evenMask] <- rates$BandEnd

  if ((x[1] != GPE$ageMin) || (x[length(x)] != GPE$ageMax)){
    str <- paste0("Incomplete BandStart/BandEnd range (", x[1], ", ", x[length(x)], ")")
    str <- paste0(str, " for (", type, ", ", sex, ")")
    traceMessage(str)
    return(FALSE)
  }

  if (is.unsorted(x, na.rm = TRUE)){
    traceMessage(paste0("BandStart and BandEnd sequences overlap for (", type, ", ", sex, ")"))
    return(FALSE)
  }

  d <- diff(x)

  mask <- seq(2,length(d)-1,2)
  if (all(d[mask] == 1) == FALSE){
    traceMessage(paste0("Gaps in BandStart/BandEnd sequence for (", type, ", ", sex, ")"))
    return(FALSE)
  }

  return(TRUE)
}
