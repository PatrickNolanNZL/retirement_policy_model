project_root <- nzs_test_project_root()
source(file.path(project_root, "app/R/ui/formatting.R"))

testthat::test_that("formatted dollar inputs parse to numeric dollars", {
  testthat::expect_equal(parse_dollar_input("$30,000"), 30000)
  testthat::expect_equal(parse_dollar_input("10,500"), 10500)
})

testthat::test_that("formatted percentage inputs parse to rates", {
  testthat::expect_equal(parse_percent_point_input("4.3%"), 0.043)
  testthat::expect_equal(parse_percent_point_input("25"), 0.25)
})
