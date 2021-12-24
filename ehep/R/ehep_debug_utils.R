#' Display Environments
#'
#' @return
dispEnvs <- function(){
  print(ls.str(globalPackageEnvironment))
  print(ls.str(baseValuesEnvironment))
  print(ls.str(epsilonValuesEnvironment))
  print(ls.str(experimentValuesEnvironment))
}
