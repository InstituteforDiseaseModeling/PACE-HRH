.explodeRates <- function(rates, year){
  assertthat::assert_that(is.numeric(year))
  assertthat::assert_that(year >= GPE$startYear & year <= GPE$endYear)

  l <- lapply(rates, function(r){
    if (!is.null(r$bandedRates)){
      return(as.vector(r$bandedRates$expansionMatrix %*% r$ratesMatrix[, as.character(year)]))
    } else {
      return(vector(mode = "double", length = length(GPE$ages)))
    }
  })

  names(l) <- names(rates)
  return(l)
}

.computeBirths <- function(femalePopulation, rates){
  return(sum(round(femalePopulation * rates[["femaleFertility"]], 0)))
}

.computeDeaths <- function(population, rates){
  outf <- round(population$Female * rates[["femaleMortality"]], 0)
  outm <- round(population$Male * rates[["maleMortality"]], 0)
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

.normalizePopulationEx <- function(pop, normalizedTotal){
  total <- sum(pop$female@values) + sum(pop$male@values)
  normFactor <- normalizedTotal / total

  pop$male@values <- round(pop$male@values * normFactor, 0)
  pop$female@values <- round(pop$female@values * normFactor, 0)
  pop$total@values <- pop$male@values + pop$female@values

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
#' pacehrh::InitializeHealthcareTasks()
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
                                            growthFlag = TRUE){
  if (.normalizationOn(normalize)) {
    initialPopulation <-
      .normalizePopulationEx(initialPopulation, normalize)
  }

  initialPopulationTotal <-
    sum(initialPopulation$female@values) + sum(initialPopulation$male@values)

  previousPyramid <- NULL

  assertthat::has_name(initialPopulation, "age")
  range <- initialPopulation$age

  projection <- lapply(years, function(currentYear){
    # Special case: the first element of the projection is just the population
    # pyramid for the starting year.

    if (is.null(previousPyramid)){
      out <- data.frame(Range = range, Female = initialPopulation$female@values, Male = initialPopulation$male@values)
    } else {
      previousYear <- currentYear - 1

      rates <- .explodeRates(populationChangeRates, currentYear)

      # currentYearFertilityRates <- explodeFertilityRates(fertilityRates[, as.character(currentYear)])
      # currentYearMortalityRates <- explodeMortalityRates(mortalityRates[, as.character(currentYear)])

      # Shuffle the end-of-year snapshots from the previous year to the next
      # population bucket
      f <- c(0, previousPyramid$Female)[1:length(GPE$ages)]
      m <- c(0, previousPyramid$Male)[1:length(GPE$ages)]

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

      births.m <- births * GPE$ratioMalesAtBirth
      births.f <- births * GPE$ratioFemalesAtBirth

      infantDeaths.m <- births.m * (rates[["maleMortality"]][1])
      infantDeaths.f <- births.f * (rates[["femaleMortality"]][1])

      f[1] <- round(births.f - infantDeaths.f, 0)
      m[1] <- round(births.m - infantDeaths.m, 0)

      out <- data.frame(Range = range, Female = f, Male = m, rates = rates)
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
