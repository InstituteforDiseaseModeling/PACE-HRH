#' Run A Suite Of EHEP Modeling Experiments
#'
#' Results are written back into \code{experimentValuesEnvironment}.
#'
#' @param scenarioName (default = "ScenarioA")
#' @param trials (default = 100)
#' @param debug (default = FALSE)
#'
#' @return List of dataframes of per-task times, or NULL
#'
#' @export
#'
RunExperiments <- function(scenarioName = "ScenarioA", trials = 100, debug = FALSE){
  assertthat::is.number(trials)
  assertthat::assert_that(trials > 1)
  assertthat::is.flag(debug)

  SaveBaseSettings(scenarioName)

  set.seed(12345)

  l <- lapply(seq_len(trials), function(trial){
    ConfigureExperimentValues()

    results <- RunExperiment()

    results$Population <- EXP$demographics
    results$PopulationRates <- EXP$populationChangeRates

    return(results)
  })

  names(l) <- as.character(seq_len(trials))

  return(l)
}
