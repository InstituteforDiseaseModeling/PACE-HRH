#' Load Initial Population
#'
#' Read the initial population pyramid from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Population pyramid data frame. Values are rounded to integers.
#'
loadInitialPopulation <- function(sheetName = "TotalPop"){
  popData <- readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetName)

  assertthat::has_name(popData, "Male")
  assertthat::has_name(popData, "Female")

  popData$Male <- round(popData$Male, 0)
  popData$Female <- round(popData$Female, 0)
  popData$Total <- popData$Male + popData$Female

  return(popData)
}

#' Load Population Change Parameters
#'
#' @param sheetName Sheet name from model input Excel file.
#'
#' @return List with two vectors: \code{initValues} and \code{changeRates}
#'
loadPopulationChangeParameters <- function(sheetName = "PopValues"){
  popValues <- readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetName)

  if (!is.null(popValues)){
    initValues <- popValues$Value2020
    changeRates <- popValues$AnnualChange

    abbrNames <- c("FertRate",
                   "FertYears",
                   "AnnualBirthRateAll",
                   "AnnualBirthRate15_19",
                   "AnnualBirthRate20_29",
                   "AnnualBirthRate30_39",
                   "AnnualBirthRate40_49",
                   "MortalityInfants",
                   "Mortality1-4",
                   "Mortality5-9",
                   "Mortality10-14",
                   "Mortality15-19",
                   "Mortality20-24",
                   "MortalityAdultF",
                   "MortalityAdultM"
    )

    names(initValues) <- abbrNames
    names(changeRates) <- abbrNames

    popValues <- list(initValues = initValues, changeRates = changeRates)
  }

  return(popValues)
}

#' Generate Time-Series of Mortality Rates
#'
#' Take the initial value and change rate information returned by a call to
#' \code{loadPopulationChangeParameters} and derive a database of annual
#' mortality rates stratified by age.
#'
#' @param sheetName Population parameters list (see \code{loadPopulationChangeParameters}).
#'
#' @return Dataframe of annual mortality rates
#'
generateMortalityRates <- function(popChangeParamsList = NULL){
  if (is.null(popChangeParamsList)){
    return(NULL)
  }

  years <- globalPackageEnvironment$startYear:globalPackageEnvironment$endYear

  mortalityColRange <- 8:15
  initValues <- popChangeParamsList$initValues[mortalityColRange]
  rates <- popChangeParamsList$changeRates[mortalityColRange]

  return(.generateDf(initValues, rates, years))
}

#' Generate Time-Series of Fertility Rates
#'
#' Take the initial value and change rate information returned be a call to
#' \code{loadPopulationChangeParameters} and derive a database of annual
#' fertility rates stratified by population age cohort.
#'
#' @param sheetName Population parameters list (see \code{loadPopulationChangeParameters}).
#'
#' @return Dataframe of annual fertility rates
#'
#' @importFrom magrittr %>%
#'
generateFertilityRates <- function(popChangeParamsList = NULL){
  if (is.null(popChangeParamsList)){
    return(NULL)
  }

  years <- globalPackageEnvironment$startYear:globalPackageEnvironment$endYear

  birthRatesColRange <- 4:7
  initValues <- popChangeParamsList$initValues[birthRatesColRange]
  rates <- popChangeParamsList$changeRates[birthRatesColRange]

  return(.generateDf(initValues, rates, years))
}

.generateDf <- function(initValues, rates, years){
  assertthat::assert_that(length(initValues) == length(rates))

  prevValues <- NULL

  l <- lapply(years, function(year){
    if (is.null(prevValues)){
      return(as.data.frame(lapply(prevValues <<- initValues, c)))
    } else {
      return(as.data.frame(lapply(prevValues <<- prevValues * rates, c)))
    }
  })

  out <- data.table::rbindlist(l)
  out <- out %>% dplyr::mutate(Year = years, .before = 1)

  return(out)
}




#' Initialize Population Data
#'
#' Load basic population information into
#'
#' @return NULL (invisible)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
InitializePopulation <- function(){
  if (!globalPackageEnvironment$globalConfigLoaded){
    loadGlobalConfig()
    globalPackageEnvironment$globalConfigLoaded <- TRUE
  }

  globalPackageEnvironment$initialPopulation <- loadInitialPopulation()

  globalPackageEnvironment$populationChangeParameters <-
    loadPopulationChangeParameters()

  globalPackageEnvironment$mortalityRates <-
    generateMortalityRates(globalPackageEnvironment$populationChangeParameters)

  globalPackageEnvironment$fertilityRates <-
    generateFertilityRates(globalPackageEnvironment$populationChangeParameters)

  invisible(NULL)
}

