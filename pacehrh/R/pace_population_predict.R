.explodeRates <- function(rates, year) {
  assertthat::assert_that(is.numeric(year))

  l <- lapply(rates, function(r) {
    if (!is.null(r$bandedRates)) {
      # TODO: This will crash if the year value passed to .explodeRates isn't in the
      # range of years used to create the rates matrix. Add try-catch trap.
      return(as.vector(r$bandedRates$expansionMatrix %*% r$ratesMatrix[, as.character(year)]))
    } else {
      return(vector(mode = "double", length = length(GPE$ages)))
    }
  })

  names(l) <- names(rates)
  return(l)
}

.computeBirths <- function(femalePopulation, rates) {
  if (GPE$roundingLaw == "none") {
    return(sum(femalePopulation * rates[["femaleFertility"]]))
  } else if (GPE$roundingLaw == "late") {
    return(round(sum(femalePopulation * rates[["femaleFertility"]]), 0))
  } else if (GPE$roundingLaw == "early") {
    return(sum(round(femalePopulation * rates[["femaleFertility"]], 0)))
  } else {
    return(sum(round(femalePopulation * rates[["femaleFertility"]], 0)))
  }
}

.computeDeaths <- function(population, rates) {
  if (GPE$roundingLaw == "none") {
    outf <- population$Female * rates[["femaleMortality"]]
    outm <- population$Male * rates[["maleMortality"]]
  } else {
    outf <- round(population$Female * rates[["femaleMortality"]], 0)
    outm <- round(population$Male * rates[["maleMortality"]], 0)
  }

  return(list(Female = outf, Male = outm))
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

.normalizePopulationEx <- function(pop, normalizedTotal) {
  total <- sum(pop$Female) + sum(pop$Male)
  normFactor <- normalizedTotal / total

  if (GPE$roundingLaw == "none") {
    pop$Male <- pop$Male * normFactor
    pop$Female <- pop$Female * normFactor
  } else {
    pop$Male <- round(pop$Male * normFactor, 0)
    pop$Female <- round(pop$Female * normFactor, 0)
  }

  pop$Total <- pop$Male + pop$Female

  return(pop)
}

#' Compute A Population Projection
#'
#' Use an initial population pyramid and population change rates
#' to predict future population pyramids.
#'
#' @param initialPopulation Population structure
#' @param populationChangeRates Population change rates (both fertility and mortality)
#' @param years Vector of years to model
#' @param normalize Whether or not to normalize the initial population
#' default = NULL, meaning don't normalize. A numeric value means normalize
#' to that value.
#' @param growthFlag If FALSE, normalize each year to the same population as
#' the initial year (default = TRUE)
#'
#' @return Demographics time-series
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' library(pacehrh)
#' pacehrh::Trace(TRUE)
#'
#' pacehrh::InitializePopulation()
#' pacehrh::InitializeScenarios()
#' pacehrh::InitializeStochasticParameters()
#' pacehrh::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' set.seed(54321)
#'
#' scenarioData <- SaveBaseSettings(scenario)
#' ConfigureExperimentValues()
#'
#' exp <- pacehrh:::EXP
#' gpe <- pacehrh:::GPE
#'
#' population <- ComputePopulationProjection(
#'   exp$initialPopulation,
#'   exp$populationChangeRates,
#'   gpe$years,
#'   normalize = scenarioData$BaselinePop,
#'   growthFlag = scenarioData$o_PopGrowth
#' )
#' }
ComputePopulationProjection <- function(initialPopulation,
                                        populationChangeRates,
                                        years,
                                        normalize = NULL,
                                        growthFlag = TRUE) {
  if (.normalizationOn(normalize)) {
    initialPopulation <-
      .normalizePopulationEx(initialPopulation, normalize)
  }

  initialPopulationTotal <-
    sum(initialPopulation$Female) + sum(initialPopulation$Male)

  previousPyramid <- NULL

  range <- initialPopulation$Age

  projection <- lapply(years, function(currentYear) {
    # Special case: the first element of the projection is just the population
    # pyramid for the starting year.

    if (is.null(previousPyramid)) {
      out <- data.frame(Range = range, Female = initialPopulation$Female, Male = initialPopulation$Male)
    } else {
      rates <- .explodeRates(populationChangeRates, currentYear)

      # Shuffle the end-of-year snapshots from the previous year to the next
      # population bucket
      f <- c(0, previousPyramid$Female)[seq_along(GPE$ages)]
      m <- c(0, previousPyramid$Male)[seq_along(GPE$ages)]

      currentPyramid <- data.frame(Range = range, Female = f, Male = m)

      # Compute deaths for all except the newborns (which were zeroed in the
      # previous step)
      deaths <- .computeDeaths(currentPyramid, rates)

      f <- f - deaths$Female
      m <- m - deaths$Male

      # Compute births for the current year, based on the average number of
      # fertile women alive during each time bucket.
      fAverage <- (currentPyramid$Female + f) / 2
      births <- .computeBirths(fAverage, rates)

      maleBirths <- births * GPE$ratioMalesAtBirth
      femaleBirths <- births * GPE$ratioFemalesAtBirth

      maleInfantDeaths <- maleBirths * (rates[["maleMortality"]][1])
      femaleInfantDeaths <- femaleBirths * (rates[["femaleMortality"]][1])

      if (GPE$roundingLaw == "none") {
        f[1] <- femaleBirths - femaleInfantDeaths
        m[1] <- maleBirths - maleInfantDeaths
      } else {
        f[1] <- round(femaleBirths - femaleInfantDeaths, 0)
        m[1] <- round(maleBirths - maleInfantDeaths, 0)
      }

      out <- data.frame(Range = range, Female = f, Male = m, rates = rates)
    }

    previousPyramid <<- out
    return(out)
  })

  names(projection) <- years

  if (growthFlag == FALSE) {
    for (i in seq_along(projection)) {
      pdata <- projection[[i]]
      ptotal <- sum(pdata$Male) + sum(pdata$Female)
      normfactor <- initialPopulationTotal / ptotal

      if (GPE$roundingLaw == "none") {
        projection[[i]]$Female <- projection[[i]]$Female * normfactor
        projection[[i]]$Male <- projection[[i]]$Male * normfactor
      } else {
        projection[[i]]$Female <- round(projection[[i]]$Female * normfactor, 0)
        projection[[i]]$Male <- round(projection[[i]]$Male * normfactor, 0)
      }
    }
  }

  return(projection)
}
