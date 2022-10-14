library(pacehrh)

withr::local_dir("..")

test_that("Populations: read sub-ranges", {
  e <- pacehrh:::GPE
  bve <- pacehrh:::BVE

  local_vars("populationLabels", envir = bve)

  testPop <- data.frame(Range = pacehrh:::GPE$ages,
                        Female = seq(1, length(pacehrh:::GPE$ages), 1),
                        Male = seq(1, length(pacehrh:::GPE$ages), 1) + 100)

  # Range = 0,1,2, ... 100
  # Female = 1,2,3, ... 99,100,101
  # Male = 101,102,103, ... 199,200,201

  dt <- data.table::data.table(
    Labels = c("label_1", "label_2", "label_3", "label_4", "-", "all"),
    Male = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE),
    Female = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
    Start = c(0, 0, 15, 15, 0, NA),
    End = c(50, 100, 49, 49, 0, NA)
  )

  bve$populationLabels <- dt

  testthat::expect_false(is.null(bve$populationLabels))
  testthat::expect_warning(pacehrh:::.computeApplicablePopulation(testPop, "notalabel"))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "label_1"), sum(1:51) + sum(101:151))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "label_2"), sum(1:101) + sum(101:201))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "label_3"), sum(16:50))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "label_4"), sum(116:150))
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "-"), 0)
  testthat::expect_equal(pacehrh:::.computeApplicablePopulation(testPop, "all"), sum(1:101) + sum(101:201))

  dt <- data.table::data.table(
    Labels = c("dup", "dup"),
    Male = c(TRUE, TRUE),
    Female = c(TRUE, TRUE),
    Start = c(0, 0),
    End = c(50, 100)
  )

  bve$populationLabels <- dt
  testthat::expect_warning(n <- pacehrh:::.computeApplicablePopulation(testPop, "dup"))
  testthat::expect_equal(n, sum(1:51) + sum(101:151))
})
