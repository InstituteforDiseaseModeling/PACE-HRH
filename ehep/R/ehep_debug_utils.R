#' Display Environments
#'
#' @return
dispEnvs <- function(){
  print("----- GLOBAL -----")
  print(ls.str(globalPackageEnvironment))
  print("------ BASE ------")
  print(ls.str(baseValuesEnvironment))
  print("----- EPSILON ----")
  print(ls.str(epsilonValuesEnvironment))
  print("--- EXPERIMENT ---")
  print(ls.str(experimentValuesEnvironment))
}
