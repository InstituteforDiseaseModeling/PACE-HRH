library(ehep)

withr::local_dir("..")

test_that("Populations: read sub-ranges", {
  testPop <- data.frame(Range = ehep:::GPE$ages,
                        Female = seq(1, length(ehep:::GPE$ages), 1),
                        Male = seq(1, length(ehep:::GPE$ages), 1) + 100)

  # Range = 0,1,2, ... 100
  # Female = 1,2,3, ... 99,100,101
  # Male = 101,102,103, ... 199,200,201

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "notalabel"), 0)
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "-"), 0)
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "births"), 102)

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "1-4"), sum(2:5) + sum(102:105))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "1 yo"), 104)
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "2 yo"), 106)

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "15 yo girls"), 16)
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "adults 18+"), sum(19:101) + sum(119:201))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "men 18+"), sum(119:201))

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "1-18"), sum(2:19) + sum(102:119))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "children 0-9"), sum(1:10) + sum(101:110))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "5-18"), sum(6:19) + sum(106:119))

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "all"), sum(1:101) + sum(101:201))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "50 yo adults"), 51 + 151)
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "adults 50+"), sum(51:101) + sum(151:201))

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "30 yo adults"), 31 + 131)
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "18 yo women"), 19)
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "women 15-49"), sum(16:50))

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "women 30-49"), sum(31:50))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "men 15-49"), sum(116:150))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "children 5-9"), sum(6:10) + sum(106:110))

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "children 10-14"), sum(11:15) + sum(111:115))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "adults 15-19"), sum(16:20) + sum(116:120))
  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "18 yo adults"), 19 + 119)

  testthat::expect_equal(ehep:::.computeApplicablePopulation(testPop, "15 yo"), 16 + 116)
})
