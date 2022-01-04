ages <- globalPackageEnvironment$ages

# Expected fields: {"Infants", "1-4y", "5-9y", "10-14y", "15-19y", "20-24y", "AdultFemale", "AdultMale"}

#' Convert Mortality Rates From Banded To Per-Age
#'
#' Mortality rates are reported in age bands - 1-4 years, 5-9 years, etc.
#' \code{explodeMortalityRates} converts the vector of banded rates into a
#' vector with one rate per year of age.
#'
#' @param banded_annual_rates Rates reported in age buckets
#'
#' @return List of vectors of per-year-of-age rates, for males and females
#'
explodeMortalityRates <- function(banded_annual_rates){
  assertthat::assert_that(length(banded_annual_rates) == 8)
  assertthat::assert_that(length(ages) > 25)

  outf <- vector(mode = "double", length = length(ages))
  outm <- vector(mode = "double", length = length(ages))

  outf[1] = banded_annual_rates[1]
  outf[2:5] = replicate(4, banded_annual_rates[2])
  outf[6:10] = replicate(5, banded_annual_rates[3])
  outf[11:15] = replicate(5, banded_annual_rates[4])
  outf[16:20] = replicate(5, banded_annual_rates[5])
  outf[21:25] = replicate(5, banded_annual_rates[6])
  outf[26:length(ages)] = replicate(76, banded_annual_rates[7])

  outm[1:25] = outf[1:25]
  outm[26:length(ages)] = replicate(76, banded_annual_rates[8])

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
#' @param banded_annual_rates Rates reported in age buckets
#'
#' @return List of vectors of per-year-of-age rates, for males and females
#'
explodeFertilityRates <- function(banded_annual_rates){
  assertthat::assert_that(length(banded_annual_rates) == 7)
  assertthat::assert_that(length(ages) > 50)

  # Initialize output vectors. ASSUME default value is zero.
  outf <- vector(mode = "double", length = length(ages))
  outm <- vector(mode = "double", length = length(ages))

  outf[16:20] = replicate(5, banded_annual_rates[1])
  outf[21:25] = replicate(5, banded_annual_rates[2])
  outf[26:30] = replicate(5, banded_annual_rates[3])
  outf[31:35] = replicate(5, banded_annual_rates[4])
  outf[36:40] = replicate(5, banded_annual_rates[5])
  outf[41:45] = replicate(5, banded_annual_rates[6])
  outf[46:50] = replicate(5, banded_annual_rates[7])

  return(list(Female = outf, Male = outm))
}

#' Compute Births
#'
#' Compute the total number of births for a year, given the female population
#' pyramid and the annual fertility rates per age.
#'
#' @param female_population Vector of female population pyramid
#' @param rates Exploded vector of annual fertility rates
#'
#' @return Total number of expected births
#'
computeBirths <- function(female_population, rates){
  # Sanity checks
  assertthat::assert_that(is.vector(female_population))
  assertthat::assert_that(is.vector(rates))
  assertthat::assert_that(length(female_population) == length(ages))
  assertthat::assert_that(length(female_population) == length(rates))

  return(sum(round(female_population * rates, 0)))
}

#' Compute Deaths
#'
#' Compute the total number of deaths for a year, given the population
#' pyramids for females and males, and the annual rates per age for both
#' sexes.
#'
#' @param population Dataframe of female and male population pyramids, as
#' returned by, for example, \code{loadInitialPopulation}
#' @param rates List with exploded vectors of annual death rates, as returned by
#' \code{explodeMortalityRates}
#'
#' @return List of expected deaths, one vector for each sex
#'
computeDeaths <- function(population, rates){
  # Sanity checks
  assertthat::assert_that(length(rates$Female) == length(rates$Male))
  assertthat::assert_that(length(rates$Female) == length(population$Female))
  assertthat::assert_that(length(rates$Male) == length(population$Male))

  outf <- round(population$Female * rates$Female / 1000, 0)
  outm <- round(population$Male * rates$Male / 1000, 0)

  return(list(Female = outf, Male = outm))
}

#' Compute Demographics Projection
#'
#' Use an initial population pyramid, fertility rates, and mortality rates
#' to predict future population pyramids.
#'
#' @param initial_population_pyramid Population pyramids dataframe. Must have
#' \code\{$Age}, \code{$Male} and \code{$Female} fields
#' @param fertility_rates Fertility rates
#' @param mortality_rates Mortality rates
#' @param years Vector of years to model
#' @param debug Flag for debugging output
#'
#' @return Demographics time-series
#'
#' @export
#'
ComputeDemographicsProjection <- function(initial_population_pyramid,
                                           fertility_rates,
                                           mortality_rates,
                                           years,
                                           debug = FALSE){

  previous_pyramid = NULL

  assertthat::has_name(initial_population_pyramid, "Age")
  range <- initial_population_pyramid$Age

  demographics_projection <- lapply(years,function(current_year){
    # Special case: the first element of the projection is just the population
    # pyramid for the starting year.

    if (is.null(previous_pyramid)){
      out <- data.frame(Range = range, Female = initial_population_pyramid$Female, Male = initial_population_pyramid$Male)
    } else {
      previous_year <- current_year - 1

      # TODO: Replace magic number subsetting with something automagical

      previous_year_fertility_rates <- explodeFertilityRates(unlist(fertility_rates[fertility_rates$Year == previous_year, 2:8]))
      current_year_fertility_rates <- explodeFertilityRates(unlist(fertility_rates[fertility_rates$Year == current_year, 2:8]))

      previous_year_mortality_rates <- explodeMortalityRates(unlist(mortality_rates[mortality_rates$Year == previous_year, 2:9]))
      current_year_mortality_rates <- explodeMortalityRates(unlist(mortality_rates[mortality_rates$Year == current_year, 2:9]))

      # Shuffle the end-of-year snapshots from the previous year to the next
      # population bucket
      f <- c(0, previous_pyramid$Female)[1:length(ages)]
      m <- c(0, previous_pyramid$Male)[1:length(ages)]

      currentPyramid <- data.frame(Range = range, Female = f, Male = m)

      # Compute deaths for all except the newborns (which were zeroed in the
      # previous step)
      deaths <- computeDeaths(currentPyramid,
                              current_year_mortality_rates)

      f <- f - deaths$Female
      m <- m - deaths$Male

      # Compute births for the current year, based on the average number of
      # fertile women alive during each time bucket.
      fAverage <- (currentPyramid$Female + f) / 2
      births <- computeBirths(fAverage, current_year_fertility_rates$Female)

      births.m <- births * globalPackageEnvironment$ratio_males_at_birth
      births.f <- births * globalPackageEnvironment$ratio_females_at_birth

      infantDeaths.m <- births.m * current_year_mortality_rates$Male[1] / 1000
      infantDeaths.f <- births.f * current_year_mortality_rates$Female[1] / 1000

      f[1] <- round(births.f - infantDeaths.f, 0)
      m[1] <- round(births.m - infantDeaths.m, 0)

      if (!debug){
        out <- data.frame(Range = range, Female = f, Male = m)
      } else {
        out <- data.frame(Range = range, Female = f, Male = m,
                          frates = current_year_fertility_rates,
                          mrates = current_year_mortality_rates)
      }
    }

    previous_pyramid <<- out
    return(out)
  })

  names(demographics_projection) <- years

  return(demographics_projection)
}

# ComputeDemographicsProjection <- function(...){
#   return(.computeDemographicsProjection(
#     globalPackageEnvironment$initialPopulation,
#     globalPackageEnvironment$fertilityRates,
#     globalPackageEnvironment$mortalityRates,
#     globalPackageEnvironment$years,
#     ...))
# }

# deaths <- computeDeaths(previous_pyramid, previous_year_mortality_rates)
#
# f <- c(0, previous_pyramid$Female - deaths$Female)[1:length(ages)]
# m <- c(0, previous_pyramid$Male - deaths$Male)[1:length(ages)]
#
# births <- computeBirths(f, current_year_fertility_rates$Female)
#
# f[1] <- round(births * globalPackageEnvironment$ratio_females_at_birth, 0)
# m[1] <- round(births * globalPackageEnvironment$ratio_males_at_birth, 0)


