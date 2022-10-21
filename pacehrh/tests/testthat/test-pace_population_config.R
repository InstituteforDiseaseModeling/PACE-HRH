library(pacehrh)

withr::local_dir("..")

# This test loads and validates a simplified version of the input population data.
test_that("Population configuration: basic population", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")

  e <- pacehrh:::GPE
  local_vars("inputExcelFile", envir = e)

  e$inputExcelFile <- "./simple_config/Test Inputs.xlsx"
  pop <- pacehrh:::loadInitialPopulation(sheetName = "TEST_TotalPop")

  pseq <- seq(10000, 0, -100)
  testthat::expect_equal(pop$female@values, pseq)
  testthat::expect_equal(pop$male@values, pseq)
})

test_that("Population configuration: confirm cleanup 1", {
  testthat::expect_equal(pacehrh:::GPE$inputExcelFile, "./config/model_inputs.xlsx")
})

.validInitPopulation <- function(pop) {
  return(TRUE)
}

test_that("Population configuration: InitializePopulation()", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  testthat::expect_equal(e$inputExcelFile, "./config/model_inputs.xlsx")
  testthat::expect_true(file.exists("globalconfig.json"))

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = bve)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = bve)

  testthat::expect_false(e$globalConfigLoaded)
  testthat::expect_null(e$initialPopulation)

  testthat::expect_invisible(pacehrh::InitializePopulation())

  testthat::expect_true(e$globalConfigLoaded)
  testthat::expect_true(!is.null(bve$initialPopulation))
  testthat::expect_true(!is.null(bve$populationLabels))

  testthat::expect_true(.validInitPopulation(e$initialPopulation))
})

test_that("Population configuration: check labels", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  testthat::expect_equal(e$inputExcelFile, "./config/model_inputs.xlsx")
  testthat::expect_true(file.exists("globalconfig.json"))

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = bve)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = bve)

  testthat::expect_false(e$globalConfigLoaded)
  testthat::expect_null(e$initialPopulation)

  testthat::expect_invisible(pacehrh::InitializePopulation())

  testthat::expect_true(e$globalConfigLoaded)
  testthat::expect_true(!is.null(bve$initialPopulation))
  testthat::expect_true(!is.null(bve$populationLabels))

  if (!is.null(bve$populationLabels)){
    df <- bve$populationLabels
    cols <- names(df)

    testthat::expect_equal(length(cols), 5)
    testthat::expect_true(all(cols %in% c("Labels", "Male", "Female", "Start", "End")))
  }
})

test_that("Population label matrices: clean case", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = bve)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = bve)

  e$inputExcelFile <- "./simple_config/super_simple_inputs.xlsx"

  lt <- pacehrh:::loadPopulationLabels(sheetName = "Lookup")
  lm <- pacehrh:::.computePopulationRanges(lt)

  testthat::expect_true(!is.null(lt))
  testthat::expect_true(!is.null(lm))

  names <- rownames(lm)

  for (i in seq_len(dim(lm)[1])){
    l <- strsplit(names[i], "-")

    tokens <- l[[1]]

    ok <- FALSE

    if (length(tokens) == 1){
      if (tokens[1] == ""){
        ok <- TRUE
        testthat::expect_equal(sum(lm[i,]), 0)
      }
    } else if (length(tokens) == 2){
      if ((tokens[1] != "") && (tokens[2] != "")){
        ok <- TRUE
        start <- as.integer(tokens[1])
        end <- as.integer(tokens[2])
        total <- end - start + 1
        testthat::expect_equal(sum(lm[i,]), total)
        testthat::expect_equal(sum(lm[i,(start+1):(end+1)]), total)
      }
    }

    if (!ok){
      cat(paste0("Malformed test label (", names[i], ")\n"))
    }
  }
})

test_that("Population label matrices: dirty case 1", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = bve)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = bve)

  e$inputExcelFile <- "./simple_config/super_simple_inputs.xlsx"

  testthat::expect_warning({
    lt <- pacehrh:::loadPopulationLabels(sheetName = "notasheet")
  })
  testthat::expect_null(lt)
})

test_that("Population label matrices: dirty case 2", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("inputExcelFile", envir = e)
  local_vars("initialPopulation", envir = bve)
  local_vars("globalConfigLoaded", envir = e)
  local_vars("populationLabels", envir = bve)

  e$inputExcelFile <- "./simple_config/super_simple_inputs.xlsx"

  testthat::expect_warning({
    lt <- pacehrh:::loadPopulationLabels(sheetName = "Bad_Lookup")
  })
  testthat::expect_null(lt)
})

test_that("Population label matrices: dirty case 3", {
  testthat::expect_null(pacehrh:::.computePopulationRanges(NULL))
})
