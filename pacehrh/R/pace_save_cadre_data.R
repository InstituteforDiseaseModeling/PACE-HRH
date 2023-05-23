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
#' }
SaveCadreOverheadData <- function(filepath = NULL, run = "Run-1") {
  scenario <- BVE$scenario$UniqueID
  roleNames <- rownames(data)

  data <-  BVE$cadreData$annualOverheads

  M <- BVE$cadreData$annualOverheads
  M <- M / 60.0
  dt <- data.table::data.table(t(M))
  dt <- dt[, Year := dimnames(M)[2]]

  dt <-
    data.table::melt(
      dt,
      id.vars = c("Year"),
      variable.name = "Role_ID",
      value.name = "OverheadTime",
      variable.factor = FALSE
    )

  dt <- dt[, Scenario_ID := scenario]
  dt <- dt[, Run := run]

  data.table::setcolorder(dt, c("Scenario_ID", "Role_ID", "Year", "Run", "OverheadTime"))

  if (!is.null(filepath)) {
    data.table::fwrite(dt,
                       file = filepath,
                       row.names = FALSE,
                       na = "NA")
  }

  return(dt)
}
