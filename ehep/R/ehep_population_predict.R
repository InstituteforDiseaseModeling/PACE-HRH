ages <- GPE$ages

.mratesExpansionMatrix <- .generateExpansionMatrix(ages = ages,
                                                   breaks = c(0, 4, 9, 14, 19, 34, 49, 59, 74))

.fratesExpansionMatrix <- .generateExpansionMatrix(ages = ages,
                                                   breaks = c(14, 19, 24, 29, 34, 39, 44, 49))

# Expected fields: {
# "MortalityInfants",
# "Mortality1_4",
# "Mortality5_9",
# "Mortality10_14",
# "Mortality15_19",
# "Mortality20_34",
# "Mortality35_49F",
# "Mortality50_59F",
# "Mortality60_74F",
# "Mortality75+F",
# "Mortality35_49M",
# "Mortality50_59M",
# "Mortality60_74M",
# "Mortality75+M"
# }

#' Convert Mortality Rates From Banded To Per-Age
#'
#' Mortality rates are reported in age bands - 1-4 years, 5-9 years, etc.
#' \code{explodeMortalityRates} converts the vector of banded rates into a
#' vector with one rate per year of age.
#'
#' @param bandedAnnualRates Rates reported in age buckets
#'
#' @return List of vectors of per-year-of-age rates, for males and females
#'
explodeMortalityRates <- function(bandedAnnualRates){
  if (GPE$globalDebug){
    assertthat::assert_that(length(bandedAnnualRates) == 14)
    assertthat::assert_that(length(ages) > 75)
  }

  r <- bandedAnnualRates[1:10]
  outf <- as.vector(.mratesExpansionMatrix %*% r)

  r <- bandedAnnualRates[c(1:6, 11:14)]
  outm <- as.vector(.mratesExpansionMatrix %*% r)

  return(list(Female = outf, Male = outm))
}

# Expected fields:
# {
# "AnnualBirthRate15_19",
# "AnnualBirthRate20_24",
# "AnnualBirthRate25_29",
# "AnnualBirthRate30_34",
# "AnnualBirthRate35_39",
# "AnnualBirthRate40_44",
# "AnnualBirthRate45_49"
# }

#' Convert Fertility Rates From Banded To Per-Age
#'
#' Birth rates are reported in age bands - 15-19 years, 20-29 years, etc.
#' \code{explodeFertilityRates} converts the vector of banded rates into a
#' vector with one rate per year of age.
#'
#' @param bandedAnnualRates Rates reported in age buckets
#'
#' @return List of vectors of per-year-of-age rates, for males and females
#'
explodeFertilityRates <- function(bandedAnnualRates){
  if (GPE$globalDebug){
    assertthat::assert_that(length(bandedAnnualRates) == 7)
    assertthat::assert_that(length(ages) > 50)
  }

  outm <- vector(mode = "double", length = length(ages))

  r <- c(0, bandedAnnualRates, 0)
  outf <- as.vector(.fratesExpansionMatrix %*% r)

  return(list(Female = outf, Male = outm))
}

#' Compute Births
#'
#' Compute the total number of births for a year, given the female population
#' pyramid and the annual fertility rates per age.
#'
#' @param femalePopulation Vector of female population pyramid
#' @param rates Exploded vector of annual fertility rates
#'
#' @return Total number of expected births
#'
computeBirths <- function(femalePopulation, rates){
  if (GPE$globalDebug){
    assertthat::assert_that(is.vector(femalePopulation))
    assertthat::assert_that(is.vector(rates))
    assertthat::assert_that(length(femalePopulation) == length(ages))
    assertthat::assert_that(length(femalePopulation) == length(rates))
  }

  return(sum(round(femalePopulation * rates, 0)))
}

#' Compute Deaths
#'
#' Compute the total number of deaths for a year, given the population
#' pyramids for females and males, and the annual rates per age for both
#' sexes.
#'
#' @param population Dataframe of female and male population pyramids
#' @param rates List with exploded vectors of annual death rates, as returned by
#' \code{explodeMortalityRates}
#'
#' @return List of expected deaths, one vector for each sex
#'
computeDeaths <- function(population, rates){
  if (GPE$globalDebug){
    assertthat::assert_that(length(rates$Female) == length(rates$Male))
    assertthat::assert_that(length(rates$Female) == length(population$Female))
    assertthat::assert_that(length(rates$Male) == length(population$Male))
  }

  outf <- round(population$Female * rates$Female / 1000, 0)
  outm <- round(population$Male * rates$Male / 1000, 0)

  return(list(Female = outf, Male = outm))
}

#' Compute Demographics Projection
#'
#' Use an initial population pyramid, fertility rates, and mortality rates
#' to predict future population pyramids.
#'
#' @param initialPopulationPyramid Population pyramids dataframe. Must have
#' \code{$Age}, \code{$Male} and \code{$Female} fields.
#' @param fertilityRates Fertility rates matrix
#' @param mortalityRates Mortality rates matrix
#' @param years Vector of years to model
#' @param normalize Whether or not to normalize the initial population (default = NULL)
#' @param growthFlag If FALSE, normalize each year to the same population as
#' the initial year (default = TRUE)
#' @param debug Flag for debugging output
#'
#' @return Demographics time-series
#'
#' @export
#'
ComputeDemographicsProjection <- function(initialPopulationPyramid,
                                          fertilityRates,
                                          mortalityRates,
                                          years,
                                          normalize = NULL,
                                          growthFlag = TRUE,
                                          debug = FALSE) {
  if (.normalizationOn(normalize)) {
    initialPopulationPyramid <-
      .normalizePopulation(initialPopulationPyramid, normalize)
  }

  initialPopulationTotal <-
    sum(initialPopulationPyramid$Female) + sum(initialPopulationPyramid$Male)

  previousPyramid = NULL

  assertthat::has_name(initialPopulationPyramid, "Age")
  range <- initialPopulationPyramid$Age

  projection <- lapply(years, function(currentYear){
    # Special case: the first element of the projection is just the population
    # pyramid for the starting year.

    if (is.null(previousPyramid)){
      out <- data.frame(Range = range, Female = initialPopulationPyramid$Female, Male = initialPopulationPyramid$Male)
    } else {
      previousYear <- currentYear - 1

      currentYearFertilityRates <- explodeFertilityRates(fertilityRates[, as.character(currentYear)])
      currentYearMortalityRates <- explodeMortalityRates(mortalityRates[, as.character(currentYear)])

      # Shuffle the end-of-year snapshots from the previous year to the next
      # population bucket
      f <- c(0, previousPyramid$Female)[1:length(ages)]
      m <- c(0, previousPyramid$Male)[1:length(ages)]

      currentPyramid <- data.frame(Range = range, Female = f, Male = m)

      # Compute deaths for all except the newborns (which were zeroed in the
      # previous step)
      deaths <- computeDeaths(currentPyramid,
                              currentYearMortalityRates)

      f <- f - deaths$Female
      m <- m - deaths$Male

      # Compute births for the current year, based on the average number of
      # fertile women alive during each time bucket.
      fAverage <- (currentPyramid$Female + f) / 2
      births <- computeBirths(fAverage, currentYearFertilityRates$Female)

      births.m <- births * GPE$ratioMalesAtBirth
      births.f <- births * GPE$ratioFemalesAtBirth

      infantDeaths.m <- births.m * currentYearMortalityRates$Male[1] / 1000
      infantDeaths.f <- births.f * currentYearMortalityRates$Female[1] / 1000

      f[1] <- round(births.f - infantDeaths.f, 0)
      m[1] <- round(births.m - infantDeaths.m, 0)

      if (!debug){
        out <- data.frame(Range = range, Female = f, Male = m)
      } else {
        out <- data.frame(Range = range, Female = f, Male = m,
                          frates = currentYearFertilityRates,
                          mrates = currentYearMortalityRates)
      }
    }

    previousPyramid <<- out
    return(out)
  })

  names(projection) <- years

  if (growthFlag == FALSE){
    for (i in seq_along(projection)){
      pdata <- projection[[i]]
      ptotal <- sum(pdata$Male) + sum(pdata$Female)
      normfactor <- initialPopulationTotal / ptotal

      projection[[i]]$Female <- round(projection[[i]]$Female * normfactor, 0)
      projection[[i]]$Male <- round(projection[[i]]$Male * normfactor, 0)
    }
  }

  return(projection)
}

.normalizationOn <- function(normalize) {
  if (is.null(normalize)) {
    return(FALSE)
  }
  if (!is.numeric(normalize)) {
    return(FALSE)
  }
  if (normalize < 0) {
    return(FALSE)
  }
  return(TRUE)
}

.normalizePopulation <- function(popDf, normalizedTotal){
  total <- sum(popDf$Female) + sum(popDf$Male)
  normFactor <- normalizedTotal / total

  popDf$Male <- round(popDf$Male * normFactor, 0)
  popDf$Female <- round(popDf$Female * normFactor, 0)
  popDf$Total <- popDf$Male + popDf$Female

  return(popDf)
}
