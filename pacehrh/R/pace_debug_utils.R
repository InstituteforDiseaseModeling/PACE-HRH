writeExpDemographicsWide <- function(filename = "out.csv") {
  popData <- EXP$demographics

  if (is.null(popData)) {
    return(invisible(NULL))
  }

  ages <- GPE$ages
  outdf <- data.frame(Age = ages)
  years <- names(popData)

  for (i in seq_along(popData)) {
    year <- years[i]
    pop <- popData[[i]]

    # Emit population data
    df <- data.frame(pop$Female, pop$Male)
    colNames <- c(paste(year, "female", sep = "_"),
                  paste(year, "male", sep = "_"))
    names(df) <- colNames
    outdf <- cbind(outdf, df)

    # Emit fertility and mortality rates data if present
    if (!is.null(pop$rates.femaleFertility)) {
      df <-
        data.frame(pop$rates.femaleFertility, pop$rates.femaleMortality, pop$rates.maleMortality)
      colNames <- c(
        paste(year, "frates.F", sep = "_"),
        paste(year, "mrates.F", sep = "_"),
        paste(year, "mrates.M", sep = "_")
      )
      names(df) <- colNames
      outdf <- cbind(outdf, df)
    }
  }

  write.csv(outdf, file = filename, row.names = FALSE)

  return(invisible(TRUE))
}
