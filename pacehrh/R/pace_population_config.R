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
  popData <- readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)

  assertthat::has_name(popData, "Age")
  assertthat::has_name(popData, "Male")
  assertthat::has_name(popData, "Female")

  # For consistency we use the integer range 0:100 to label age buckets.
  assertthat::assert_that(length(popData$Male) == length(popData$Female))
  assertthat::assert_that(length(popData$Male) == length(popData$Age))
  assertthat::assert_that(length(popData$Age) == length(GPE$ages))

  male <- PopulationPyramid()
  female <- PopulationPyramid()
  total <- PopulationPyramid()

  male <- setFromVector(male, round(popData$Male, 0))
  female <- setFromVector(female, round(popData$Female, 0))
  total <- setFromVector(total,
                         round(popData$Male, 0) + round(popData$Female, 0))

  return(list(age = GPE$ages,
              female = female,
              male = male,
              total = total))
}

.popLabelRawColumns <-
  c("Relevant Population Labels", "Male", "Female", "Starting Age", "Ending Age")

.popLabelColumns <-
  c("Labels", "Male", "Female", "Start", "End")

loadPopulationLabels <- function(sheetName = "Lookup"){
  suppressMessages(
    df <- readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)
  )

  if (!all(.popLabelRawColumns %in% colnames(df))){
    warning(paste0("Invalid columns in population labels lookup sheet"))
    warning(paste0("Expected: ", paste0(.popLabelRawColumns, collapse = ", ")))
    return(NULL)
  }

  # TODO: include more explicit try-catch error handling. As is, when the read
  # fails, BVE$populationLabels stays NULL, which eventually triggers a fatal
  # error when RunExperiments() is called.

  # Remove blank columns
  df <- df[,apply(df,2,function(x){any(!is.na(x))})]
  # Remove blank rows
  df <- df[apply(df,1,function(x){any(!is.na(x))}),]

  # Filter the columns, and rename
  df <- df[,.popLabelRawColumns]
  names(df) <- .popLabelColumns

  dt <- data.table::setDT(df)
  data.table::setkey(dt, Labels)

  return(dt)
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
#' pacehrh::InitializePopulation()
#' }
InitializePopulation <- function(){
  .checkAndLoadGlobalConfig()

  BVE$initialPopulation <- NULL
  BVE$populationLabels <- NULL

  BVE$initialPopulation <- loadInitialPopulation()
  BVE$populationLabels <- loadPopulationLabels()

  return(invisible(NULL))
}

