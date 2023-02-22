#' Load Experiment Scenario Information
#'
#' Read the experiment scenario information from the model inputs Excel file.
#' The name and location of the model inputs Excel file is loaded from the
#' global configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Data frame of experiment scenario parameters, or NULL on error
#'
loadScenarios <- function(sheetName = "Scenarios") {
  scenarios <- NULL

  if (file.exists(GPE$inputExcelFile)){
    out <- tryCatch(
      {
        scenarios <-
          readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)
      },
      warning = function(war)
      {
        traceMessage(paste("WARNING:", war))
      },
      error = function(err)
      {
        traceMessage(paste("ERROR:", err))
      },
      finally =
        {

        }
    )
  } else {
    traceMessage(paste("Could not find model input file ",
                             GPE$inputExcelFile,
                             sep = ""))
  }

  if (is.null(scenarios)){
    return(NULL)
  }
    
  scenarios <- .checkColumns(scenarios, .scenarioMetaData)
  
  return(scenarios)
}

.checkColumns <- function(table, schema){
  rCols <- .checkRequiredColumns()
  if (is.null(rCols)){
    return(NULL)
  }
  
  oCols <- .checkOptionalColumns()
  if (is.null(oCols)){
    return(NULL)
  }

  table <- table[c(rCols, oCols)]
  
  return(table)  
}

.checkRequiredColumns <- function(){
  e <- rlang::caller_env()
  
  columns <- names(e$table)

  # Test that all required columns have shown up
  if (length(setdiff(e$schema$rcols, columns)) > 0){
    .raiseAlarm(setdiff(e$schema$rcols, columns), alarmType = "name")
    return(NULL)
  }
  
  # Test that all required columns have the correct types
  correctTypesMask <- (sapply(e$table[e$schema$rcols], typeof) == e$schema$rtypes)
  if (!all(correctTypesMask)){
    .raiseAlarm(e$schema$rcols[!correctTypesMask], alarmType = "type")
    return(NULL)
  }
  
  # Return full list of required columns
  return(e$schema$rcols)
}

.checkOptionalColumns <- function(){
  e <- rlang::caller_env()

  columns <- names(e$table)
  types <- sapply(e$table, typeof)
  out <- vector(mode = "character")
  badTypeColumnsList <- vector(mode = "character")
  
  for (i in seq_along(e$schema$ocols)){
    col <- e$schema$ocols[i]
    type <- e$schema$otypes[i]
    
    if (col %in% columns){ # If the table contains an optional column
      if (types[col] == type){ # If the column has the right type
        out <- c(out, col)
      } else {
        badTypeColumnsList <- c(badTypeColumnsList, col)
      }
    } else { # Add optional column with default values
      e$table[[col]] <- vector(mode = type, length = NROW(e$table))
      out <- c(out, col)
    }
  }

  # Raise alarm if user has supplied optional columns of the wrong type
  if (length(badTypeColumnsList) > 0){
    .raiseAlarm(badTypeColumnsList, alarmType = "type")
    return(NULL)
  }

  # Return list of present and validated optional columns
  return(out)
}

.raiseAlarm <- function(columns, alarmType = "name"){
  colStr <- paste(columns, collapse = ", " )
  
  if (alarmType == "name"){
    errorMsg <- paste0("Missing required columns in table: ", colStr)
  } else if (alarmType == "type"){
    errorMsg <- paste0("Columns with incorrect types in table: ", colStr)
  }
  
  raiseMessage(errorMsg)
}

#' Initialize Experiment Scenario Information
#'
#' Read experiment scenario information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @param loadFromExcel If TRUE, initialize the scenarios list from the model
#' inputs Excel file. If FALSE, initialize with a blank scenarios table.
#' @param ... See \code{loadScenarios()}
#'
#' @export
#'
#' @return NULL (invisible)
#'
#' @examples
#' \dontrun{
#' library(pacehrh)
#'
#' pacehrh::InitializePopulation()
#' pacehrh::InitializeScenarios()
#' pacehrh::InitializeStochasticParameters()
#' pacehrh::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#' }
InitializeScenarios <- function(loadFromExcel = TRUE, ...){
  .checkAndLoadGlobalConfig()

  if (loadFromExcel){
    scenarios <- loadScenarios(...)
  } else {
    scenarios <- CreateScenariosTable()
  }

  # TODO: Insert error handling

  GPE$scenarios <- scenarios
  invisible(NULL)
}
