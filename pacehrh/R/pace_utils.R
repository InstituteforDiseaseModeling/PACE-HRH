.populationRateCategories <- c("femaleFertility",
                               "maleFertility",
                               "femaleMortality",
                               "maleMortality")

#' Generate A Long-Format Data Frame Of Suite Mortality and Fertility Rates
#'
#' @param results Results structure (as returned by [RunExperiments()])
#' @param rateCategory Rate type ({"femaleFertility" | "maleFertility" | "femaleMortality" | "maleMortality"})
#'
#' @return Data frame
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#' results <-
#'   pacehrh::RunExperiments(scenarioName = "MergedModel",
#'                           trials = 20)
#'
#' df <- pacehrh::GetSuiteRates(results, rateCategory = "femaleFertility")
#' }
GetSuiteRates <- function(results, rateCategory = .populationRateCategories[1]) {
  if (is.null(results)){
    warning(paste0("Null results parameter. Returning NULL."))
    return(NULL)
  }

  if (!(rateCategory %in% .populationRateCategories)){
    warning(paste0("Invalid population rate category (", rateCategory, ")"))
    return(NULL)
  }

  trials <- names(results)

  l <- lapply(trials, function(trial) {
    t <-
      results[[trial]][["PopulationRates"]][[rateCategory]][["ratesMatrix"]]
    t <- as.data.table(t, keep.rownames = "Label")
    t$Trial = rep(trial, nrow(t))
    return(t)
  })

  outData <- data.table::rbindlist(l)

  outData <- data.table::melt(
    outData,
    id.vars = c("Label", "Trial"),
    variable.name = "Year",
    value.name = "Rate"
  )

  outData$Year <- as.character(outData$Year)

  return(outData)
}

is.blank <- function(str){
  if (is.null(str)) {
    return(TRUE)
  }

  if (is.na(str)) {
    return(TRUE)
  }

  if (is.character(str)) {
    if (nchar(trimws(str)) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  return(FALSE)
}






