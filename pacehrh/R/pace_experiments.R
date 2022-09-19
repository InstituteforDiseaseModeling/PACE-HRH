#' Run A Suite Of PACE-HRH Modeling Experiments
#'
#' Results are written back into \code{experimentValuesEnvironment}.
#'
#' @param scenarioName (default = "ScenarioA")
#' @param trials (default = 100)
#' @param debug (default = FALSE)
#' @param seed Random number generator seed to be used for this suite.
#' (default = `GPE$rngSeed`, as set up during package initialization)
#'
#' @return List of dataframes of per-task times, or NULL
#'
#' @export
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
RunExperiments <-
  function(scenarioName = "ScenarioA",
           trials = 100,
           debug = FALSE,
           seed = GPE$rngSeed) {
    assertthat::is.number(trials)
    assertthat::is.number(seed)
    assertthat::assert_that(trials > 1)
    assertthat::is.flag(debug)

    if (is.null(SaveBaseSettings(scenarioName))){
      warning("Critical failure. RunExperiments() ended.")
      return(NULL)
    }

    set.seed(seed)

    l <- lapply(seq_len(trials), function(trial) {
      ConfigureExperimentValues()

      results <- RunExperiment()

      results$Population <- EXP$demographics
      results$PopulationRates <- EXP$populationChangeRates

      return(results)
    })

    names(l) <- as.character(seq_len(trials))

    return(l)
  }
