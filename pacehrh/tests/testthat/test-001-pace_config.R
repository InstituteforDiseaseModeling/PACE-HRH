library(ehep)
library(withr)
library(jsonlite)

# These tests are intended to run from the "tests" directory. The scripts
# themselves are executed from the "tests/testthat" directory, so the
# first step is to move the working directory up one level.

# local_* functions are great: they clean up after themselves in the right
# (LIFO) order when the unit test function terminates.
withr::local_dir("..")

configDir = "dir"
configFile = "file"
seed = 142857L

test_that("Global configuration: default", {
  globalConfigFile = "globalconfig.json"

  temp <- NULL

  if (file.exists(globalConfigFile)){
    temp <- tempfile(tmpdir = ".")
    file.rename(globalConfigFile, temp)
  }

  .doTest <- function(){
    withr::local_file(globalConfigFile)

    e <- ehep:::GPE
    withr::defer(e$inputExcelFile <- originalInputExcelFile)
    originalInputExcelFile <- e$inputExcelFile

    testJson <-
      list(configDirectoryLocation = configDir,
           inputExcelFile = configFile,
           suiteRngSeed = seed)
    write_json(testJson, globalConfigFile)

    testthat::expect_invisible(ehep:::loadGlobalConfig())
    testthat::expect_equal(ehep:::GPE$inputExcelFile,
                           paste(configDir, configFile, sep = "/"))
    testthat::expect_equal(ehep:::GPE$rngSeed, seed)
  }

  .doTest()

  if (!is.null(temp)){
    file.rename(temp, globalConfigFile)
  }
})

test_that("Global configuration: basic", {
  globalConfigFile = "in.json"
  withr::local_file(globalConfigFile)

  e <- ehep:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  testJson <-
    list(configDirectoryLocation = configDir,
         inputExcelFile = configFile,
         suiteRngSeed = seed)
  write_json(testJson, globalConfigFile)

  testthat::expect_invisible(ehep:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(ehep:::GPE$inputExcelFile,
                         paste(configDir, configFile, sep = "/"))
  testthat::expect_equal(ehep:::GPE$rngSeed, seed)
})

test_that("Global configuration: bad seed", {
  globalConfigFile = "in.json"
  withr::local_file(globalConfigFile)

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("rngSeed", envir = e)

  originalSeed <- e$rngSeed

  testJson <-
    list(configDirectoryLocation = configDir,
         inputExcelFile = configFile,
         suiteRngSeed = "notanumber")
  write_json(testJson, globalConfigFile)

  testthat::expect_invisible(ehep:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(e$inputExcelFile,
                         paste(configDir, configFile, sep = "/"))
  testthat::expect_equal(e$rngSeed, originalSeed)
})

test_that("Global configuration: missing seed", {
  globalConfigFile = "in.json"
  withr::local_file(globalConfigFile)

  e <- ehep:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("rngSeed", envir = e)

  originalSeed <- e$rngSeed

  testJson <-
    list(configDirectoryLocation = configDir,
         inputExcelFile = configFile)
  write_json(testJson, globalConfigFile)

  testthat::expect_invisible(ehep:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(e$inputExcelFile,
                         paste(configDir, configFile, sep = "/"))
  testthat::expect_equal(e$rngSeed, originalSeed)
})

test_that("Global configuration: missing file", {
  globalConfigFile = "notafile.json"

  e <- ehep:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  testthat::expect_invisible(ehep:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(ehep:::GPE$inputExcelFile,
                         originalInputExcelFile)
})

test_that("Global configuration: bad file", {
  globalConfigFile = "bad.json"
  withr::local_file(globalConfigFile)

  writeLines("this is not JSON!!!", globalConfigFile)

  e <- ehep:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  testthat::expect_invisible(ehep:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(ehep:::GPE$inputExcelFile,
                         originalInputExcelFile)
})

test_that("Global configuration: missing element", {
  globalConfigFile = "bad.json"
  withr::local_file(globalConfigFile)

  testJson <-
    list(inputExcelFile = configFile)
  write_json(testJson, globalConfigFile)

  e <- ehep:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  testthat::expect_invisible(ehep:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(ehep:::GPE$inputExcelFile, paste(".", configFile, sep = "/"))
})

test_that("Global configuration: misnamed element", {
  globalConfigFile = "bad.json"
  withr::local_file(globalConfigFile)

  testJson <-
    list(configDirectoryLocation_xxx = configDir,
         inputExcelFile_xxx = configFile)
  write_json(testJson, globalConfigFile)

  e <- ehep:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  withr::defer(ehep::Trace(originalTraceState))
  originalTraceState <- ehep::Trace(TRUE)

  testthat::expect_invisible(ehep:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(ehep:::GPE$inputExcelFile, paste(configDir, configFile, sep = "/"))
})

test_that("Global configuration: check-and-load", {
  e <- ehep:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile
  withr::defer(e$globalConfigLoaded <- originalLoadedFlag)
  originalLoadedFlag <- e$globalConfigLoaded

  testthat::expect_invisible(ehep:::.checkAndLoadGlobalConfig())
  testthat::expect_true(ehep:::GPE$globalConfigLoaded)
})

test_that("Global configuration: SetGlobalStartEndYears", {
  e <- ehep:::GPE

  withr::defer(e$startYear <- originalStartYear)
  originalStartYear <- e$startYear
  withr::defer(e$endYear <- originalEndYear)
  originalEndYear <- e$endYear
  withr::defer(e$years <- originalYears)
  originalYears <- e$years

  testthat::expect_invisible(ehep::SetGlobalStartEndYears("a", 1))
  testthat::expect_equal(e$startYear, originalStartYear)
  testthat::expect_equal(e$endYear, originalEndYear)
  testthat::expect_equal(e$years, originalYears)

  testthat::expect_invisible(ehep::SetGlobalStartEndYears(2, "b"))
  testthat::expect_equal(e$startYear, originalStartYear)
  testthat::expect_equal(e$endYear, originalEndYear)
  testthat::expect_equal(e$years, originalYears)

  testthat::expect_invisible(ehep::SetGlobalStartEndYears(120, 100))
  testthat::expect_equal(e$startYear, originalStartYear)
  testthat::expect_equal(e$endYear, originalEndYear)
  testthat::expect_equal(e$years, originalYears)

  start = 100
  end = 120
  testthat::expect_invisible(ehep::SetGlobalStartEndYears(start, end))
  testthat::expect_equal(e$startYear, start)
  testthat::expect_equal(e$endYear, end)
  testthat::expect_equal(e$years, start:end)

  # Defaults
  start = 2020
  end = 2040
  testthat::expect_invisible(ehep::SetGlobalStartEndYears())
  testthat::expect_equal(e$startYear, start)
  testthat::expect_equal(e$endYear, end)
  testthat::expect_equal(e$years, start:end)
})
