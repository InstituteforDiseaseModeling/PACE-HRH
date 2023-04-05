library(pacehrh)

withr::local_dir("..")

test_that("Initialization: baseline", {
  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("scenarios", envir = e)
  local_vars("traceState", envir = e)

  result <- PaceInitialize()

  testthat::expect_true(result)
  testthat::expect_equal(e$inputExcelFile, "./config/model_inputs.xlsx")
})

test_that("Initialization: bad input file", {
  testthat::expect_warning(result <-
                             PaceInitialize(inputFile = "notafile"),
                           regexp = "Input file <notafile> not found.")
  testthat::expect_false(result)
})

test_that("Initialization: bad config file", {
  gpe <- pacehrh:::GPE

  local_vars("traceState", envir = gpe)
  # pacehrh::Trace(state = TRUE)

  testthat::expect_warning(result <-
                             PaceInitialize(globalConfigFile = "notafile",
                                            forceGlobalConfigReload = TRUE),
                           regexp = "Could not find global configuration file")

  # TODO: I'm not happy with this behavior. The user tells the system to reload
  # a file that doesn't exist, the system raises the appropriate warning, then
  # continues anyway using default global config values. loadGlobalConfig()
  # (called from PaceInitialize()) never fails, which makes it tolerant to
  # conditions like the globalconfig.json file not being present. But when the
  # user says explicitly "I want the configuration specified in this file and no
  # other", the system should not make a substitution.

  testthat::expect_true(result)
})

test_that("Initialization: config file", {
  gpe <- pacehrh:::GPE
  local_vars("globalConfigLoaded", envir = gpe)
  local_vars("inputExcelFile", envir = gpe)

  local_vars("traceState", envir = gpe)
  # pacehrh::Trace(state = TRUE)

  configFile <- "globalconfig_simple.json"
  inputFile <- "./simple_config/super_simple_inputs.xlsx"
  testthat::expect_true(file.exists(configFile))
  testthat::expect_true(file.exists(inputFile))

  # Force a load from a user-specified global configuration file.
  result <- PaceInitialize(globalConfigFile = configFile,
                           forceGlobalConfigReload = TRUE)

  testthat::expect_true(result)
  testthat::expect_equal(gpe$inputExcelFile, inputFile)
})

test_that("Initialization: non-default sheets", {
  gpe <- pacehrh:::GPE
  local_vars("globalConfigLoaded", envir = gpe)
  local_vars("inputExcelFile", envir = gpe)

  local_vars("traceState", envir = gpe)
  # pacehrh::Trace(state = TRUE)

  configFile <- "globalconfig_simple.json"
  inputFile <- "./simple_config/super_simple_inputs.xlsx"
  testthat::expect_true(file.exists(configFile))
  testthat::expect_true(file.exists(inputFile))

  # Force a load from a user-specified global configuration file, with non-
  # default tab names.
  result <- PaceInitialize(globalConfigFile = configFile,
                           forceGlobalConfigReload = TRUE,
                           scenariosSheet = "Scenarios_Alt",
                           populationSheet = "Flat_Population",
                           stochasticParametersSheet = "StochasticParameters_Alt",
                           seasonalityCurvesSheet = "SeasonalityCurves_Alt",
                           seasonalityOffsetsSheet = "SeasonalityOffsets_Alt"
                           )

  testthat::expect_true(result)
  testthat::expect_equal(gpe$inputExcelFile, inputFile)
})
