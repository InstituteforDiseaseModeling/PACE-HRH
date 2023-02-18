library(pacehrh)

withr::local_dir("..")

gpe <- pacehrh:::GPE
bve <- pacehrh:::BVE

test_that("Configuration: loadGlobalConfig", {
  local_vars("inputExcelFile", envir = gpe)
  local_vars("ignoreGlobalConfigExcelFileSetting", envir = gpe)
  local_vars("rngSeed", envir = gpe)
  local_vars("startYear", envir = gpe)
  local_vars("endYear", envir = gpe)

  gpe$ignoreGlobalConfigExcelFileSetting <- FALSE
  gpe$inputExcelFile <- "notafile"

  pacehrh:::loadGlobalConfig()
  testthat::expect_false(gpe$inputExcelFile == "notafile")
})

test_that("Configuration: stochasticity", {
  local_vars("stochasticity", envir = gpe)

  testthat::expect_equal(gpe$stochasticity, TRUE)

  # Interrogate current value. Should be TRUE.
  value <- pacehrh::SetStochasticity()
  testthat::expect_equal(value, TRUE)

  # Flip to FALSE
  value <- pacehrh::SetStochasticity(FALSE)
  testthat::expect_equal(value, TRUE)
  testthat::expect_equal(pacehrh::SetStochasticity(), FALSE)

  # Flip back to TRUE
  value <- pacehrh::SetStochasticity(TRUE)
  testthat::expect_equal(value, FALSE)
  testthat::expect_equal(pacehrh::SetStochasticity(), TRUE)

  # Check that trying to set a bad value doesn't overwrite existing
  value <- pacehrh::SetStochasticity('invalidvalue')
  testthat::expect_equal(value, TRUE)
  testthat::expect_equal(pacehrh::SetStochasticity(), TRUE)
})

test_that("Configuration: SetInputExcelFile", {
  local_vars("inputExcelFile", envir = gpe)
  local_vars("ignoreGlobalConfigExcelFileSetting", envir = gpe)

  errorMessageRegex <- "Input file <.*> not found\\."

  testthat::expect_warning(pacehrh::SetInputExcelFile(NULL),
                           regexp = errorMessageRegex)

  testthat::expect_warning(pacehrh::SetInputExcelFile(""),
                           regexp = errorMessageRegex)

  testthat::expect_warning(pacehrh::SetInputExcelFile("notafile"),
                           regexp = errorMessageRegex)

  defaultFile <- "./config/model_inputs.xlsx"
  simpleFile <- "./simple_config/super_simple_inputs.xlsx"

  pacehrh::SetInputExcelFile()
  testthat::expect_equal(GPE$inputExcelFile, defaultFile)
  testthat::expect_true(GPE$ignoreGlobalConfigExcelFileSetting)

  pacehrh::SetInputExcelFile(simpleFile)
  testthat::expect_equal(GPE$inputExcelFile, simpleFile)
  testthat::expect_true(GPE$ignoreGlobalConfigExcelFileSetting)
})

test_that("Configuration: SetPerAgeStats", {
  local_vars("perAgeStats", envir = gpe)
  
  testthat::expect_equal(gpe$perAgeStats, "off")
  
  # Interrogate current value. Should be "off".
  value <- pacehrh::SetPerAgeStats()
  testthat::expect_equal(value, "off")

  # Check that trying to set a bad value doesn't overwrite existing
  value <- pacehrh::SetPerAgeStats("invalidvalue")
  testthat::expect_equal(value, "off")
  testthat::expect_equal(pacehrh::SetPerAgeStats(), "off")
  
  # Flip to "monthly"
  value <- pacehrh::SetPerAgeStats("mOnThLy")
  testthat::expect_equal(value, "off")
  testthat::expect_equal(pacehrh::SetPerAgeStats(), "monthly")
  
  # Flip to "annual"
  value <- pacehrh::SetPerAgeStats("aNnUaL")
  testthat::expect_equal(value, "monthly")
  testthat::expect_equal(pacehrh::SetPerAgeStats(), "annual")
})
