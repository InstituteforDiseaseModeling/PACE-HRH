#' Save Cadre Information From A Suite Of Experiments
#'
#' @param filepath CSV file to write to (default = "cadre.csv")
#'
#' @return NULL (invisible)
#' @export
#'
#' @md
#' @examples
#' \dontrun{
#'
#' CHANGE THIS EXAMPLE
#'
#' scenario <- "ScenarioName"
#'
#'results <-
#'  pacehrh::RunExperiments(scenarioName = scenario,
#'                       trials = 100)
#'
#' df <- pacehrh::SaveSuiteDemographics(results, breaks = c(50))
#'
#' df <- df %>%
#'   dplyr::group_by(Trial, Year, AgeBucket) %>%
#'   dplyr::summarize(Female = sum(Female), Male = sum(Male))
#' }
SaveCadreData <- function(filepath = "cadre.csv", run = "") {
  data <- BVE$cadreData$monthlyOverheads
  scenario <- BVE$scenario$UniqueID
  roleNames <- rownames(data)

  l <- strsplit(colnames(data), "\\.")
  years <- as.numeric(sapply(l, function(x)
    x[1]))
  months <- as.numeric(sapply(l, function(x)
    x[2]))

  msize <- length(months)

  scenarioCol <- character(length = msize)
  scenarioCol <- scenario

  runCol <- character(length = msize)
  runCol <- run

  roleCol <- character(length = msize)

  l <- lapply(1:dim(data)[1], function(i) {
    roleName <- roleNames[i]
    timeData <- data[i, ]

    return(
      data.frame(
        "Scenario_ID" = scenarioCol,
        "Role_ID" = roleName,
        "Run" = runCol,
        "Year" = years,
        "Month" = months,
        "OverheadTime" = timeData
      )
    )
  })

  out <- data.table::rbindlist(l)

  data.table::fwrite(out,
                     file = filepath,
                     row.names = FALSE,
                     na = "NA")
}
