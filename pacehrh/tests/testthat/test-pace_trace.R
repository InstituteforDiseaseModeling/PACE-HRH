library(pacehrh)
library(data.table)

withr::local_dir("..")

test_that("Save cadre data", {
  withr::defer(pacehrh::Trace(originalTraceState))
  originalTraceState <- pacehrh::Trace(TRUE)

  testthat::expect_message(pacehrh:::traceMessage("--- Trace Message ---"),
                           regexp = "Trace Message")

  pacehrh::Trace(FALSE)
  testthat::expect_no_message(pacehrh:::traceMessage("--- Trace Message ---"))

  testthat::expect_message(pacehrh:::raiseMessage("--- Raise Message ---"),
                           regexp = "Raise Message")
}
)
