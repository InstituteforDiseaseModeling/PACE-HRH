library(pacehrh)
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

    e <- pacehrh:::GPE
    withr::defer(e$inputExcelFile <- originalInputExcelFile)
    originalInputExcelFile <- e$inputExcelFile

    testJson <-
      list(configDirectoryLocation = configDir,
           inputExcelFile = configFile,
           suiteRngSeed = seed)
    write_json(testJson, globalConfigFile)

    testthat::expect_invisible(pacehrh:::loadGlobalConfig())
    testthat::expect_equal(pacehrh:::GPE$inputExcelFile,
                           paste(configDir, configFile, sep = "/"))
    testthat::expect_equal(pacehrh:::GPE$rngSeed, seed)
  }

  .doTest()

  if (!is.null(temp)){
    file.rename(temp, globalConfigFile)
  }
})

test_that("Global configuration: basic", {
  globalConfigFile = "in.json"
  withr::local_file(globalConfigFile)

  e <- pacehrh:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  testJson <-
    list(configDirectoryLocation = configDir,
         inputExcelFile = configFile,
         suiteRngSeed = seed)
  write_json(testJson, globalConfigFile)

  testthat::expect_invisible(pacehrh:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile,
                         paste(configDir, configFile, sep = "/"))
  testthat::expect_equal(pacehrh:::GPE$rngSeed, seed)
})

test_that("Global configuration: bad seed", {
  globalConfigFile = "in.json"
  withr::local_file(globalConfigFile)

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("rngSeed", envir = e)

  originalSeed <- e$rngSeed

  testJson <-
    list(configDirectoryLocation = configDir,
         inputExcelFile = configFile,
         suiteRngSeed = "notanumber")
  write_json(testJson, globalConfigFile)

  testthat::expect_invisible(pacehrh:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(e$inputExcelFile,
                         paste(configDir, configFile, sep = "/"))
  testthat::expect_equal(e$rngSeed, originalSeed)
})

test_that("Global configuration: missing seed", {
  globalConfigFile = "in.json"
  withr::local_file(globalConfigFile)

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("rngSeed", envir = e)

  originalSeed <- e$rngSeed

  testJson <-
    list(configDirectoryLocation = configDir,
         inputExcelFile = configFile)
  write_json(testJson, globalConfigFile)

  testthat::expect_invisible(pacehrh:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(e$inputExcelFile,
                         paste(configDir, configFile, sep = "/"))
  testthat::expect_equal(e$rngSeed, originalSeed)
})

test_that("Global configuration: missing file", {
  globalConfigFile = "notafile.json"

  e <- pacehrh:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  withr::defer(pacehrh::Trace(originalTraceState))
  originalTraceState <- pacehrh::Trace(TRUE)

  testthat::expect_invisible(pacehrh:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile,
                         originalInputExcelFile)
})

test_that("Global configuration: bad file", {
  globalConfigFile = "bad.json"
  withr::local_file(globalConfigFile)

  writeLines("this is not JSON!!!", globalConfigFile)

  e <- pacehrh:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  withr::defer(pacehrh::Trace(originalTraceState))
  originalTraceState <- pacehrh::Trace(TRUE)

  testthat::expect_invisible(pacehrh:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile,
                         originalInputExcelFile)
})

test_that("Global configuration: missing element", {
  globalConfigFile = "bad.json"
  withr::local_file(globalConfigFile)

  testJson <-
    list(inputExcelFile = configFile)
  write_json(testJson, globalConfigFile)

  e <- pacehrh:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  withr::defer(pacehrh::Trace(originalTraceState))
  originalTraceState <- pacehrh::Trace(TRUE)

  testthat::expect_invisible(pacehrh:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, paste(".", configFile, sep = "/"))
})

test_that("Global configuration: misnamed element", {
  globalConfigFile = "bad.json"
  withr::local_file(globalConfigFile)

  testJson <-
    list(configDirectoryLocation_xxx = configDir,
         inputExcelFile_xxx = configFile)
  write_json(testJson, globalConfigFile)

  e <- pacehrh:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile

  withr::defer(pacehrh::Trace(originalTraceState))
  originalTraceState <- pacehrh::Trace(TRUE)

  testthat::expect_invisible(pacehrh:::loadGlobalConfig(globalConfigFile))
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, paste(configDir, configFile, sep = "/"))
})

test_that("Global configuration: check-and-load", {
  e <- pacehrh:::GPE
  withr::defer(e$inputExcelFile <- originalInputExcelFile)
  originalInputExcelFile <- e$inputExcelFile
  withr::defer(e$globalConfigLoaded <- originalLoadedFlag)
  originalLoadedFlag <- e$globalConfigLoaded

  testthat::expect_invisible(pacehrh:::.checkAndLoadGlobalConfig())
  testthat::expect_true(pacehrh:::GPE$globalConfigLoaded)
})

test_that("Global configuration: SetGlobalStartEndYears", {
  e <- pacehrh:::GPE

  withr::defer(e$startYear <- originalStartYear)
  originalStartYear <- e$startYear
  withr::defer(e$endYear <- originalEndYear)
  originalEndYear <- e$endYear
  withr::defer(e$years <- originalYears)
  originalYears <- e$years
  withr::defer(e$shoulderYears <- originalshoulderYears)
  originalshoulderYears <- e$shoulderYears

  testthat::expect_invisible(pacehrh::SetGlobalStartEndYears("a", 1))
  testthat::expect_equal(e$startYear, originalStartYear)
  testthat::expect_equal(e$endYear, originalEndYear)
  testthat::expect_equal(e$years, originalYears)

  testthat::expect_invisible(pacehrh::SetGlobalStartEndYears(2, "b"))
  testthat::expect_equal(e$startYear, originalStartYear)
  testthat::expect_equal(e$endYear, originalEndYear)
  testthat::expect_equal(e$years, originalYears)

  testthat::expect_invisible(pacehrh::SetGlobalStartEndYears(120, 100))
  testthat::expect_equal(e$startYear, originalStartYear)
  testthat::expect_equal(e$endYear, originalEndYear)
  testthat::expect_equal(e$years, originalYears)

  testthat::expect_invisible(pacehrh::SetGlobalStartEndYears(100, 120, -1))
  testthat::expect_equal(e$startYear, originalStartYear)
  testthat::expect_equal(e$endYear, originalEndYear)
  testthat::expect_equal(e$years, originalYears)

  start <- 100
  end <- 120
  shoulderYears <- 0
  testthat::expect_invisible(pacehrh::SetGlobalStartEndYears(start, end, shoulderYears))
  testthat::expect_equal(e$startYear, start)
  testthat::expect_equal(e$endYear, end)
  testthat::expect_equal(e$years, start:end)
  testthat::expect_equal(e$shoulderYears, shoulderYears)

  start <- 100
  end <- 120
  shoulderYears <- 3
  testthat::expect_invisible(pacehrh::SetGlobalStartEndYears(start, end, shoulderYears))
  testthat::expect_equal(e$startYear, start)
  testthat::expect_equal(e$endYear, end)
  testthat::expect_equal(e$years, start:end)
  testthat::expect_equal(e$shoulderYears, shoulderYears)

  # Defaults
  start = 2020
  end = 2040
  shoulderYears = 1
  testthat::expect_invisible(pacehrh::SetGlobalStartEndYears())
  testthat::expect_equal(e$startYear, start)
  testthat::expect_equal(e$endYear, end)
  testthat::expect_equal(e$years, start:end)
  testthat::expect_equal(e$shoulderYears, shoulderYears)
})
