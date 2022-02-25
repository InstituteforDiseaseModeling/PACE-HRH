#' Run A Suite Of EHEP Modeling Experiments
#'
#' Results are written back into \code{experimentValuesEnvironment}.
#'
#' @param scenarioName (default = "ScenarioA")
#' @param debug (default = FALSE)
#'
#' @return List of dataframes of per-task times, or NULL
#'
#' @export
#'
RunExperiments <- function(scenarioName = "ScenarioA", trials = 100, debug = FALSE){
  assertthat::is.number(trials)
  assertthat::assert_that(trials > 1)

  SaveBaseSettings(scenarioName)
  InitializeEpsilons()

  l <- lapply(seq_len(trials), function(trial){
    NextEpsilons()
    results <- RunExperiment()
    results$Population <- EXP$demographics
    results$PopulationParams <- list(FRatesMatrix = EXP$fertilityRatesMatrix,
                                     MRatesMatrix = EXP$mortalityRatesMatrix)
    return(results)
  })

  names(l) <- as.character(seq_len(trials))

  return(l)
}
