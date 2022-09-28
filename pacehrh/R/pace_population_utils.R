#' Generate Matrices of Applicable Populations
#'
#' @param results Results structure as returned by \code{RunExperiments()}
#'
#' @return List of applicable population matrices (labels x years)
#' @export
#'
#' @examples
#' \dontrun{
#` results <-
#`   pacehrh::RunExperiments(scenarioName = scenario,
#`                           trials = nTrials)
#`
#`  pm <- pacehrh::ComputeApplicablePopulationMatrices(results)
#' }
ComputeApplicablePopulationMatrices <- function(results, labels = GPE$populationLabels$Labels){
  if (is.null(results)){
    return(NULL)
  }

  if (is.null(labels)){
    traceMessage("No population labels")
  }

  ll <- lapply(results, function(r){
    .computeApplicablePopulationMatrix(r, labels)
  })

  return(ll)
}

#' Generate Matrix of Applicable Populations
#'
#' @param result One entry from the results structure as returned by \code{RunExperiments()}
#'
#' @return An applicable population matrix (labels x years)
#' @export
#'
#' @examples
#' \dontrun{
#` results <-
#`   pacehrh::RunExperiments(scenarioName = scenario,
#`                           trials = nTrials)
#`
#`  m <- pacehrh::ComputeApplicablePopulationMatriX(results)
#' }
ComputeApplicablePopulationMatrix <- function(result, labels = GPE$populationLabels$Labels){
  if (is.null(result)){
    return(NULL)
  }

  if (is.null(labels)){
    traceMessage("No population labels")
  }

  return(.computeApplicablePopulationMatrix(result, labels))
}

.computeApplicablePopulationMatrix <- function(result, labels){
  l <- lapply(result$Population, function(pop){
    s <- sapply(labels, function(label){
      .computeApplicablePopulation(pop, label)
    })
  })

  return(t(do.call(rbind,l)))
}
