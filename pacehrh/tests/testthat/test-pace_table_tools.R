library(pacehrh)

withr::local_dir("..")

loadTableTests <- "simple_config/LoadTable Tests.xlsx"

test_that("Table tools: basic table read", {
  table <- pacehrh:::loadTable(loadTableTests,
                               "CadreRoles",
                               schema = pacehrh:::.cadreRolesMetaData)

  testthat::expect_true(!is.null(table))
})

.testSchemaColumnNames <-
  c(
    "Column-1",
    "Column-2",
    "Column-3",
    "Column-4"
  )

.testSchemaColumnTypes <-
  c(
    "character",
    "character",
    "double",
    "double"
  )

.testSchemaKeyColumns <-
  c(
    "Column-1"
  )

.testSchemaMetaData <-
  list(
    rcols = .testSchemaColumnNames,
    rtypes = .testSchemaColumnTypes,
    cols = .testSchemaColumnNames,
    types = .testSchemaColumnTypes,
    kcols = .testSchemaKeyColumns
  )

test_that("Table tools: error cases", {
  # Invalid sheet name
  table <- pacehrh:::loadTable(loadTableTests,
                               "notatable",
                               schema = .testSchemaMetaData)

  testthat::expect_true(is.null(table))

  # Wrong schema
  testthat::expect_warning(
    table <- pacehrh:::loadTable(loadTableTests,
                                 "CadreRoles",
                                 schema = .testSchemaMetaData),
    regexp = "Missing required columns"
  )

  testthat::expect_true(is.null(table))

  # Bad data - string in numeric column
  testthat::expect_warning(
    table <- pacehrh:::loadTable(loadTableTests,
                                 "CadreRoles_bad_data_1",
                                 schema = pacehrh:::.cadreRolesMetaData),
    regexp = "incorrect types"
  )

  testthat::expect_true(is.null(table))


  testthat::expect_warning(
    table <- pacehrh:::loadTable(loadTableTests,
                               "CadreRoles_bad_data_1",
                               schema = pacehrh:::.cadreRolesMetaData,
                               convertType = TRUE),
    regexp = "without introducing NAs"
  )

  testthat::expect_true(is.null(table))

  # Blank column returned as logical instead of numeric.
  testthat::expect_warning(
    table <- pacehrh:::loadTable(loadTableTests,
                               "TestSchema_blank_col",
                               schema = .testSchemaMetaData),
    regexp = "incorrect types"
  )

  testthat::expect_true(is.null(table))

  # Blank column successfully converted from logical to numeric
  table <- pacehrh:::loadTable(loadTableTests,
                               "TestSchema_blank_col",
                               schema = .testSchemaMetaData,
                               convertType = TRUE)

  testthat::expect_true(!is.null(table))
})
