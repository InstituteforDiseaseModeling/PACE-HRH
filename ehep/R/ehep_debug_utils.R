#' Display Environments
#'
#' @return
dispEnvs <- function(){
  print("----- GLOBAL -----")
  print(ls.str(globalPackageEnvironment))
  print("------ BASE ------")
  print(ls.str(baseValuesEnvironment))
  print("----- EPSILON ----")
  print(ls.str(epsilonValuesEnvironment))
  print("--- EXPERIMENT ---")
  print(ls.str(experimentValuesEnvironment))
  invisible(NULL)
}

saveExperimentDemographics <-
  function(filename = "out.csv", format = "fat") {
    if (!exists("experimentValuesEnvironment$demographics")) {
      invisible(NULL)
    }

    popData <- experimentValuesEnvironment$demographics

    if (is.null(popData)) {
      invisible(NULL)
    }

    ages <- globalPackageEnvironment$ages
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

    write.csv(outdf, file = "full_population_2020_2040.csv", row.names = FALSE)
    invisible(TRUE)
  }
