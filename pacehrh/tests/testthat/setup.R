library(pacehrh)
library(withr)
library(assertthat)

cat("Running basic sanity checks ...\n")

# Check that needed environments have been created
assertthat::assert_that(is.environment(pacehrh:::globalPackageEnvironment))
assertthat::assert_that(is.environment(pacehrh:::baseValuesEnvironment))
assertthat::assert_that(is.environment(pacehrh:::experimentValuesEnvironment))

assertthat::assert_that(is.environment(pacehrh:::GPE))
assertthat::assert_that(is.environment(pacehrh:::BVE))
assertthat::assert_that(is.environment(pacehrh:::EXP))

withr::local_dir("..")

# Check that unit test setup is basically correct
assertthat::assert_that(file.exists("globalconfig.json"))

assertthat::assert_that(exists("inputExcelFile", where = pacehrh:::GPE))
assertthat::are_equal(pacehrh:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

assertthat::assert_that(exists("globalConfigLoaded", where = pacehrh:::GPE))
assertthat::assert_that(pacehrh:::GPE$globalConfigLoaded == FALSE)

assertthat::assert_that(exists("startYear", where = pacehrh:::GPE))
assertthat::assert_that(pacehrh:::GPE$startYear == 2020)

assertthat::assert_that(exists("endYear", where = pacehrh:::GPE))
assertthat::assert_that(pacehrh:::GPE$endYear == 2040)

assertthat::assert_that(exists("years", where = pacehrh:::GPE))
assertthat::assert_that(identical(as.numeric(pacehrh:::GPE$years), as.numeric(2020:2040)))

# ----------------------------------------------------------------

# Declare local_vars function for saving/restoring global variable values

local_vars <- function(vars = vector(), envir = parent.frame(), .local_envir = parent.frame()){
  old <- .saveVars(vars, envir)
  withr::defer(.restoreVars(old, envir), envir = .local_envir)
  return(invisible(old))
}

.saveVars <- function(vars, envir = parent.frame()){
  varCache <- rlang::env()

  for (var in vars){
    out <- tryCatch({
      value = get(var, pos = envir)
      assign(var, value, pos = varCache)
    },
    warning = function(war) {
      cat(paste0("WARNING: ", war, "\n"))
    },
    error = function(err) {
      cat(paste0("ERROR: ", err, "\n"))
    },
    finally = {

    })
  }

  return(varCache)
}

.restoreVars <- function(varCache, envir = parent.frame()){
  varNames <- rlang::env_names(varCache)
  for (var in varNames){
    value <- get(var, pos = varCache)
    assign(var, value, envir = envir)
  }

  rm(varCache)

  return(invisible(NULL))
}
