#' Save Cadre Information From A Suite Of Experiments
#'
#' @param filepath CSV file to write to (default = NULL)
#' @param run Identifier for the experiment run
#'
#' @return Cadre overhead data data table
#' @export
#'
#' @md
#' @examples
#' \dontrun{
#' results <-
#'   pacehrh::RunExperiments(scenarioName = "MergedModel",
#'                           trials = 20)
#'
#' SR <- pacehrh::SaveExtendedSuiteResults(results, filepath = "_SR.csv", run = "Run-1")
#' CA <- pacehrh::SaveCadreAllocations(SR, filepath = "_CA.csv")
#' cadreOverheadTimes <- pacehrh::SaveCadreOverheadData(filepath = "_COD.csv")
#' summaryStats <- pacehrh::ComputeSummaryStats(SR, CA)
#' }
SaveCadreOverheadData <- function(filepath = NULL, run = "Run-1") {
  scenario <- BVE$scenario$UniqueID

  # Transpose the Annual Overheads matrix, and add a Year column
  M <- BVE$cadreData$annualOverheads
  M <- M / 60.0
  dt <- data.table::data.table(t(M))
  dt <- dt[, Year := dimnames(M)[2]]

  # Melt the table to create one row per Role_ID + Year combination
  dt <-
    data.table::melt(
      dt,
      id.vars = c("Year"),
      variable.name = "Role_ID",
      value.name = "OverheadTime",
      variable.factor = FALSE
    )

  # Add some more columns
  dt <- dt[, Scenario_ID := scenario]
  dt <- dt[, Run := run]

  # Set the column order
  data.table::setcolorder(dt, c("Scenario_ID", "Role_ID", "Year", "Run", "OverheadTime"))

  if (!is.null(filepath)) {
    data.table::fwrite(dt,
                       file = filepath,
                       row.names = FALSE,
                       na = "NA")
  }

  return(dt)
}
