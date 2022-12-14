library(pacehrh)

withr::local_dir("..")

# TESTS FOR POPULATION TREE STRUCTURES

test_that("Population tree: defaults", {
  popTree <- pacehrh:::.createPopulationTreeDf()

  testthat::expect_equal(names(popTree), c("Age", "Female", "Male", "Total"))

  testthat::expect_equal(class(popTree), c("tbl_df", "tbl", "data.frame"))
  testthat::expect_type(popTree$Age, "integer")
  testthat::expect_type(popTree$Female, "double")
  testthat::expect_type(popTree$Male, "double")
  testthat::expect_type(popTree$Total, "double")

  testthat::expect_identical(popTree$Age, 0:100L)
  testthat::expect_true(all(popTree$Female == 0.0))
  testthat::expect_true(all(popTree$Male == 0.0))
  testthat::expect_true(all(popTree$Total == 0.0))
})

test_that("Population tree: bad params", {
  testthat::expect_warning(popTree <- pacehrh:::.createPopulationTreeDf(ages = NULL))
  testthat::expect_warning(popTree <- pacehrh:::.createPopulationTreeDf(femalePop = integer()))
  testthat::expect_warning(popTree <- pacehrh:::.createPopulationTreeDf(ages = 1:10, femalePop = 1:10, malePop = 1:9))
})

test_that("Population tree: rounding", {
  gpe <- pacehrh:::GPE
  local_vars("roundingLaw", envir = gpe)

  baseSeq <- seq(0.0, 1.0, 0.1)

  SetRoundingLaw("none")

  popTree <- pacehrh:::.createPopulationTreeDf(ages = 0:10,
                                               femalePop = baseSeq,
                                               malePop = baseSeq)

  testthat::expect_true(!is.null(popTree))
  testthat::expect_identical(popTree$Female, baseSeq)
  testthat::expect_identical(popTree$Male, baseSeq)

  SetRoundingLaw("late")

  popTree <- pacehrh:::.createPopulationTreeDf(ages = 0:10,
                                               femalePop = baseSeq,
                                               malePop = baseSeq)

  testthat::expect_identical(popTree$Female, round(baseSeq, 0))
  testthat::expect_identical(popTree$Male, round(baseSeq, 0))
})
