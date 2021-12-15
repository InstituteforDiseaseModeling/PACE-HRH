#' Load Healthcare Task Information
#'
#' Read the healthcare task information from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Data frame of healthcare task parameters
#'
loadTaskParameters <- function(sheetName = "TaskValues"){
  popData <- readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetName)

  # Convert some of the NA values to sensible defaults
  assertthat::has_name(popData, "StartingRateInPop")
  popData$StartingRateInPop[is.na(popData$StartingRateInPop)] <- 0

  assertthat::has_name(popData, "RateMultiplier")
  popData$RateMultiplier[is.na(popData$RateMultiplier)] <- 1

  assertthat::has_name(popData, "AnnualDeltaRatio")
  popData$AnnualDeltaRatio[is.na(popData$AnnualDeltaRatio)] <- 1

  assertthat::has_name(popData, "NumContactsPerUnit")
  popData$NumContactsPerUnit[is.na(popData$NumContactsPerUnit)] <- 0

  assertthat::has_name(popData, "NumContactsAnnual")
  popData$NumContactsAnnual[is.na(popData$NumContactsAnnual)] <- 0

  assertthat::has_name(popData, "HoursPerWeek")
  popData$HoursPerWeek[is.na(popData$HoursPerWeek)] <- 0

  assertthat::has_name(popData, "FTEratio")
  popData$FTEratio[is.na(popData$FTEratio)] <- 0

  return(popData)
}



# [1] "Indicator"          "CommonName"         "ClinicalOrNon"      "ClinicalCat"        "ServiceCat"         "RelevantPop"        "Geography"          "StartingRateInPop"
# [9] "RateType"           "RateMultiplier"     "MultiplierReason"   "AnnualDeltaRatio"   "NumContactsPerUnit" "NumContactsAnnual"  "MinsPerContact"     "HoursPerWeek"
# [17] "FTEratio"
