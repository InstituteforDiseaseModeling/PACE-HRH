library(ehep)
library(withr)
library(assertthat)

cat("Running basic sanity checks ...\n")

# Check that needed environments have been created
assertthat::assert_that(is.environment(ehep:::globalPackageEnvironment))
assertthat::assert_that(is.environment(ehep:::baseValuesEnvironment))
assertthat::assert_that(is.environment(ehep:::experimentValuesEnvironment))

assertthat::assert_that(is.environment(ehep:::GPE))
assertthat::assert_that(is.environment(ehep:::BVE))
assertthat::assert_that(is.environment(ehep:::EXP))

withr::local_dir("..")

# Check that unit test setup is basically correct
assertthat::assert_that(file.exists("globalconfig.json"))

assertthat::assert_that(exists("inputExcelFile", where = ehep:::GPE))
assertthat::are_equal(ehep:::GPE$inputExcelFile, "./config/R Model Inputs.xlsx")

assertthat::assert_that(exists("globalConfigLoaded", where = ehep:::GPE))
assertthat::assert_that(ehep:::GPE$globalConfigLoaded == FALSE)

assertthat::assert_that(exists("startYear", where = ehep:::GPE))
assertthat::assert_that(ehep:::GPE$startYear == 2020)

assertthat::assert_that(exists("endYear", where = ehep:::GPE))
assertthat::assert_that(ehep:::GPE$endYear == 2040)

assertthat::assert_that(exists("years", where = ehep:::GPE))
assertthat::assert_that(identical(as.numeric(ehep:::GPE$years), as.numeric(2020:2040)))

# ----------------------------------------------------------------

# Declare local_vars function for saving/restoring global variable values

local_vars <- function(vars = vector(), envir = parent.frame(), .local_envir = parent.frame()){
  old <- .saveVars(vars, envir)
  withr::defer(.restoreVars(old, envir), envir = .local_envir)
  return(invisible(old))
}

.saveVars <- function(vars, envir = parent.frame()){
  varCache <- list()

  for (var in vars){
    out <- tryCatch({
      expr <- paste0("get('", var, "', envir = envir)")
      value <- eval(parse(text = expr))
      expr <- paste0("varCache$`", var, "` <- value")
      eval(parse(text = expr))
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
  varNames <- names(varCache)
  for (var in varNames){
    value <- varCache[[var]]
    expr <- paste0("assign('", var, "', value, envir = envir)")
    eval(parse(text = expr))
  }

  return(invisible(NULL))
}
