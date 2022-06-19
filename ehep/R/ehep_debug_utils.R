#' Display Environments
#'
#' @return
dispEnvs <- function(){
  print("----- GLOBAL -----")
  print(ls.str(globalPackageEnvironment))
  print("------ BASE ------")
  print(ls.str(baseValuesEnvironment))
  print("--- EXPERIMENT ---")
  print(ls.str(experimentValuesEnvironment))
  invisible(NULL)
}

writeCurrentExpDemographics_wide <- function(filename = "out.csv") {
  if (!exists("experimentValuesEnvironment$demographics")) {
    return(invisible(NULL))
  }

  popData <- EXP$demographics

  if (is.null(popData)) {
    return(invisible(NULL))
  }

  ages <- GPE$ages
  outdf <- data.frame(Age = ages)
  years <- names(popData)

  for (i in 1:length(popData)) {
    year <- years[i]
    pop <- popData[[i]]

    # Emit population data
    df <- data.frame(pop$Female, pop$Male)
    colNames <- c(paste(year, "female", sep = "_"),
                  paste(year, "male", sep = "_"))
    names(df) <- colNames
    outdf <- cbind(outdf, df)

    # Emit fertility and mortality rates data if present
    if (length(which(names(pop) == "frates.Female")) == 1) {
      df <-
        data.frame(pop$frates.Female, pop$mrates.Female, pop$mrates.Male)
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
