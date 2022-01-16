#' Load Initial Population
#'
#' Read the initial population pyramid from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return List with three \code{PopulationPyramid} objects:
#' \code{female}, \code{male} and \code{total}
#'
loadInitialPopulation <- function(sheetName = "TotalPop"){
  popData <- readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetName)

  assertthat::has_name(popData, "Age")
  assertthat::has_name(popData, "Male")
  assertthat::has_name(popData, "Female")

  # For consistency we use the integer range 0:100 to label age buckets. The
  # character-string age bucket labels read from the model inputs file are saved
  # for plotting, etc
  assertthat::assert_that(length(popData$Male) == length(popData$Female))
  assertthat::assert_that(length(popData$Male) == length(popData$Age))
  assertthat::assert_that(length(popData$Age) == length(globalPackageEnvironment$ages))
  globalPackageEnvironment$ageLabels <- popData$Age

  male <- PopulationPyramid()
  female <- PopulationPyramid()
  total <- PopulationPyramid()

  male <- setFromVector(male, round(popData$Male, 0))
  female <- setFromVector(female, round(popData$Female, 0))
  total <- setFromVector(total,
                         round(popData$Male, 0) + round(popData$Female, 0))

  return(list(age = globalPackageEnvironment$ages,
              female = female,
              male = male,
              total = total))
}

#' Load Population Change Parameters
#'
#' @param sheetName Sheet name from model input Excel file.
#'
#' @return List with two \code{PopulationChangeParameters} objects:
#' \code{initValues} and \code{changeRates}
#'
loadPopulationChangeParameters <- function(sheetName = "PopValues"){
  popValues <- readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetName)

  if (!is.null(popValues)){
    initValues <- PopulationChangeParameters()
    changeRates <- PopulationChangeParameters()

    initValues <- setFromVector(initValues, popValues$Value2020)
    changeRates <- setFromVector(changeRates, popValues$AnnualChange)

    popValues <- list(initValues = initValues, changeRates = changeRates)
  }

  return(popValues)
}

#' Initialize Population Data
#'
#' Load basic population information into the global package environment,
#' from which it can be used for later processing.
#'
#' @return NULL (invisible)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ehep::InitializePopulation()
#' }
InitializePopulation <- function(){
  .checkAndLoadGlobalConfig()

  globalPackageEnvironment$initialPopulation <- loadInitialPopulation()

  globalPackageEnvironment$populationChangeParameters <-
    loadPopulationChangeParameters()

  invisible(NULL)
}

