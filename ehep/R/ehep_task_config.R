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
  taskData <- readxl::read_xlsx(globalPackageEnvironment$inputExcelFile, sheet = sheetName)

  # Convert some of the NA values to sensible defaults
  assertthat::has_name(taskData, "StartingRateInPop")
  taskData$StartingRateInPop[is.na(taskData$StartingRateInPop)] <- 0

  assertthat::has_name(taskData, "RateMultiplier")
  taskData$RateMultiplier[is.na(taskData$RateMultiplier)] <- 1

  assertthat::has_name(taskData, "AnnualDeltaRatio")
  taskData$AnnualDeltaRatio[is.na(taskData$AnnualDeltaRatio)] <- 1

  assertthat::has_name(taskData, "NumContactsPerUnit")
  taskData$NumContactsPerUnit[is.na(taskData$NumContactsPerUnit)] <- 0

  assertthat::has_name(taskData, "NumContactsAnnual")
  taskData$NumContactsAnnual[is.na(taskData$NumContactsAnnual)] <- 0

  assertthat::has_name(taskData, "HoursPerWeek")
  taskData$HoursPerWeek[is.na(taskData$HoursPerWeek)] <- 0

  assertthat::has_name(taskData, "FTEratio")
  taskData$FTEratio[is.na(taskData$FTEratio)] <- 0

  return(taskData)
}



# [1] "Indicator"          "CommonName"         "ClinicalOrNon"      "ClinicalCat"        "ServiceCat"         "RelevantPop"        "Geography"          "StartingRateInPop"
# [9] "RateType"           "RateMultiplier"     "MultiplierReason"   "AnnualDeltaRatio"   "NumContactsPerUnit" "NumContactsAnnual"  "MinsPerContact"     "HoursPerWeek"
# [17] "FTEratio"
