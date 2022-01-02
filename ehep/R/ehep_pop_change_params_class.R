.VarNames <- c("FertRate",
               "FertYears",
               "AnnualBirthRateAll",
               "AnnualBirthRate15_19",
               "AnnualBirthRate20_29",
               "AnnualBirthRate30_39",
               "AnnualBirthRate40_49",
               "MortalityInfants",
               "Mortality1_4",
               "Mortality5_9",
               "Mortality10_14",
               "Mortality15_19",
               "Mortality20_24",
               "MortalityAdultF",
               "MortalityAdultM",
               "AnnualBirthRate20_24",
               "AnnualBirthRate25_29",
               "AnnualBirthRate30_34",
               "AnnualBirthRate35_39",
               "AnnualBirthRate40_44",
               "AnnualBirthRate45_49"
)

.VarLookup <- 1:length(.VarNames)
names(.VarLookup) <- .VarNames

#' Population Change Parameters Class
#'
#' @slot values numeric
#'
#' @return Class of type \code{PopulationChangeParameters}
#'
#' @export PopulationChangeParameters
#' @exportClass PopulationChangeParameters
#'
PopulationChangeParameters <- setClass(
  # Set the name for the class
  "PopulationChangeParameters",

  # Define the variables
  slots = c(
    values = "numeric"
  ),

  prototype = list(values = replicate(length(.VarNames), 0.0))
)

#' Set Population Change Parameters
#'
#' @param object PopulationChangeParameters object
#' @param values Vector of values to copy into PopulationChangeParameters object
#'
#' @return Updated \code{PopulationChangeParameters} object
#'
#' @exportMethod setFromVector
#'
setGeneric(
  name = "setFromVector",
  def = function(object, values)
  {
    standardGeneric("setFromVector")
  }
)

setMethod(
  f = "setFromVector",
  signature = "PopulationChangeParameters",
  definition = function(object, values)
  {
    assertthat::assert_that(length(values) == 21)
    assertthat::assert_that(is.numeric(values) == TRUE)

    # Clear any names on the vector
    names(values) <- NULL

    object@values = values
    return(object)
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
#' @exportMethod getValue
#'
setGeneric(
  name = "getValue",
  def = function(object, label)
  {
    standardGeneric("getValue")
  }
)

setMethod(
  f = "getValue",
  signature = "PopulationChangeParameters",
  definition = function(object, label)
  {
    assertthat::is.string(label)

    return(object@values[.VarLookup[label]])
  }
)

#' Set Population Change Parameter Values
#'
#' @param object \code{PopulationChangeParameters} object
#' @param label Name of variable to write
#' @param value Value to write to variable
#'
#' @return Updated \code{PopulationChangeParameters} object
#'
#' @exportMethod setValue
#'
setGeneric(
  name = "setValue",
  def = function(object, label, value)
  {
    standardGeneric("setValue")
  }
)

setMethod(
  f = "setValue",
  signature = "PopulationChangeParameters",
  definition = function(object, label, value)
  {
    assertthat::is.string(label)
    assertthat::is.number(value)

    if (!is.na(.VarLookup[label])){
      object@values[.VarLookup[label]] <- value
    }

    return(object)
  }
)
