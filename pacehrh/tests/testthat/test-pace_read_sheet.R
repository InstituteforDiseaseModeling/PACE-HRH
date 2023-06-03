library(pacehrh)

withr::local_dir("..")

test_that("Sheet read: basic", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("inputExcelFile", envir = gpe)

  local_vars("traceState", envir = gpe)
  pacehrh::Trace(state = TRUE)

  gpe$inputExcelFile <- "./config/model_inputs.xlsx"
  data <- pacehrh:::readSheet(sheetName = "CadreRoles")

  testthat::expect_true(!is.null(data))
  testthat::expect_s3_class(data, c("tbl_df", "tbl", "data.frame"))

  cols <- names(data)
  schemaCols <- pacehrh:::.cadreRolesColumnNames
  testthat::expect_true(length(setdiff(schemaCols, cols)) == 0)
})

test_that("Sheet read: non-default intput file", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("inputExcelFile", envir = gpe)

  local_vars("traceState", envir = gpe)
  pacehrh::Trace(state = TRUE)

  gpe$inputExcelFile <-
  data <- pacehrh:::readSheet(path = "./simple_config/super_simple_inputs.xlsx",
                              sheetName = "Flat_Rates")

  testthat::expect_true(!is.null(data))
  testthat::expect_s3_class(data, c("tbl_df", "tbl", "data.frame"))

  cols <- names(data)
  schemaCols <- pacehrh:::.populationChangeRateColumnNames
  testthat::expect_true(length(setdiff(schemaCols, cols)) == 0)
})

test_that("Sheet read: bad file", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("inputExcelFile", envir = gpe)

  local_vars("traceState", envir = gpe)
  pacehrh::Trace(state = TRUE)

  gpe$inputExcelFile <- "./config/notafile"
  testthat::expect_message(data <-
                             pacehrh:::readSheet(sheetName = "CadreRoles"),
                           regexp = "Could not find model input file")

  testthat::expect_true(is.null(data))
})

test_that("Sheet read: bad sheet", {
  gpe <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("inputExcelFile", envir = gpe)

  local_vars("traceState", envir = gpe)
  pacehrh::Trace(state = TRUE)

  gpe$inputExcelFile <- "./config/model_inputs.xlsx"
  testthat::expect_message(data <-
                             pacehrh:::readSheet(sheetName = "notasheet"),
                           regexp = "Sheet .+ not found")

  testthat::expect_true(is.null(data))
})
