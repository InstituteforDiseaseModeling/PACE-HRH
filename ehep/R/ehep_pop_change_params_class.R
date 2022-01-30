.PcpVarNames <- c("FertRate",
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

.PcpVarLookup <- 1:length(.PcpVarNames)
names(.PcpVarLookup) <- .PcpVarNames

.PcpDefaultValues <- replicate(length(.PcpVarNames), 0.0)

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

  prototype = list(values = .PcpDefaultValues)
)
