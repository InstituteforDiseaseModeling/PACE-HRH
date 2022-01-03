#' @exportMethod getValue
#'
setGeneric(
  name = "getValue",
  def = function(object, label)
  {
    standardGeneric("getValue")
  }
)

#' Get Population Change Parameter Values
#'
#' @param object \code{PopulationChangeParameters} object
#' @param label Name of variable to read. Allowed variable names are \code{
#' "FertRate",
#' "FertYears",
#' "AnnualBirthRateAll",
#' "AnnualBirthRate15_19",
#' "AnnualBirthRate20_29",
#' "AnnualBirthRate30_39",
#' "AnnualBirthRate40_49",
#' "MortalityInfants",
#' "Mortality1_4",
#' "Mortality5_9",
#' "Mortality10_14",
#' "Mortality15_19",
#' "Mortality20_24",
#' "MortalityAdultF",
#' "MortalityAdultM",
#' "AnnualBirthRate20_24",
#' "AnnualBirthRate25_29",
#' "AnnualBirthRate30_34",
#' "AnnualBirthRate35_39",
#' "AnnualBirthRate40_44",
#' "AnnualBirthRate45_49"
#' }
#'
#' @return Variable value
#'
setMethod(
  f = "getValue",
  signature = c("PopulationChangeParameters", "numeric"),
  definition = function(object, label)
  {
    assertthat::is.string(label)

    return(object@values[.VarLookup[label]])
  }
)
