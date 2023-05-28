#' Load Cadre Roles
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Dataframe of cadre roles information
#'
#' @noRd
loadCadreRoles <- function(sheetName = .defaultCadreRolesSheet){
  traceMessage(paste0("Loading cadre roles sheet ", sheetName))

  cadreData <- loadTable(sheet = sheetName, schema = .cadreRolesMetaData)

  return(cadreData)
}

#' Initialize Cadre Roles
#'
#' Read the cadre roles information from the model inputs Excel file, and
#' save to a location in the global package environment.
#'
#' @md
#' @param ... Parameters passed through to loadCadreRoles()
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
#' pacehrh::InitializeCadreRoles()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#' }
InitializeCadreRoles <- function(...){
  .checkAndLoadGlobalConfig()

  cadreRolesData <- loadCadreRoles(...)

  BVE$cadreRoles <- cadreRolesData
  return(invisible(NULL))
}

#' Load Cadre Task Allocation Sheet
#'
#' This is by far the most complex of the sheet loading routines, due to the
#' compact format of the Excel sheet. The Excel sheet has two header rows, so
#' the `read_xlsx()` function is called twice, once for the header rows and once
#' for the data rows. In order to convert the output into a computationally
#' useful long format {(task, year) x cadre-member-id}, the header rows are
#' combined to form a year+members key and the data table is melted using the
#' key. Then the key is split back out into member and year columns, the the
#' table is cast back into a wide format with each member as a separate column.
#'
#' @param sheetName
#'
#' @return Data table of per-year cadre allocation definitions
#' @noRd
loadTaskCadres <- function(sheetName = .defaultTaskCadresSheet) {
  traceMessage(paste0("Loading task cadres sheet ", sheetName))

  # Read the header rows and data rows separately
  suppressMessages(
    dataRows <- data.table::setDT(readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName, col_names = FALSE, skip = 2))
  )
  suppressMessages(
    headerRows <- readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName, col_names = FALSE, skip = 0, n_max = 2)
  )

  # Abort if either read fails
  if (is.null(dataRows) | is.null(headerRows)){
    return(NULL)
  }

  # Remove the first two entries from the header rows to focus on the members and
  # year info. Extract members and years, then combine into a single token to
  # use in the subsequent table melt operation.
  hr <- headerRows[-c(1,2)]
  years <- gsub("StartYear", "", hr[1,])
  members <- unlist(hr[2,])
  tokenSeparator <- "&&"
  key <- paste0(years, tokenSeparator, members)

  # Use the key values as headers to the data table, then melt the data table
  # to turn key values into row data.
  dt <- dataRows
  dt[is.na(dt)] <- 0
  names(dt) <- c(unlist(headerRows[2,1:2]), key)

  suppressWarnings(
    dt <-
      data.table::melt(
        dt,
        id.vars = c("Indicator", "CommonName"),
        variable.name = "Category",
        value.name = "allocation",
        variable.factor = FALSE
      )
  )

  # Take the key apart into year and member tokens, then assign individual
  # columns for the token values.
  tokens <- data.table::tstrsplit(dt[,Category], tokenSeparator)
  dt[, Year := as.numeric(tokens[[1]])]
  dt[, CadreMember := tokens[[2]]]

  # Remove unnecessary columns
  dt <- dt[CadreMember != "Total"]
  dt <- dt[CadreMember != "Unassigned"]

  # Cast to the desired final format
  dt <-
    data.table::dcast(
      dt,
      Indicator + Year ~ CadreMember,
      value.var = "allocation",
      fill = 0
    )

  return(dt)
}
