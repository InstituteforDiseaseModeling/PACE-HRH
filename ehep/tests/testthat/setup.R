library(ehep)
library(withr)
library(assertthat)

cat("Running basic sanity checks ...\n")

# Check that needed environments have been created
assertthat::assert_that(is.environment(ehep:::globalPackageEnvironment))
assertthat::assert_that(is.environment(ehep:::baseValuesEnvironment))
assertthat::assert_that(is.environment(ehep:::epsilonValuesEnvironment))
assertthat::assert_that(is.environment(ehep:::experimentValuesEnvironment))

assertthat::assert_that(is.environment(ehep:::GPE))
assertthat::assert_that(is.environment(ehep:::BVE))
assertthat::assert_that(is.environment(ehep:::EPS))
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



