library(pacehrh)

withr::local_dir("..")

# This test loads and validates a simplified version of the task data.
test_that("Table tools: basic table read", {
  table <- pacehrh:::loadTable(pacehrh:::GPE$inputExcelFile,
                               "CadreRoles",
                               schema = pacehrh:::.cadreRolesMetaData)

  testthat::expect_true(!is.null(table))
})
