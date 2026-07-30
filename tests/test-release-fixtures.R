source("helper-model.R")
nzs_source_model()
source("release-fixtures.R")

testthat::test_that("approved release fixtures remain numerically stable", {
  actual <- nzs_release_fixture_results()
  expected <- data.frame(
    fixture = c("eligibility_age_only", "cpi_indexation_only", "combined_reform"),
    fiscal_npv_billion = c(96.1774173937746, 170.12054149164, 283.504423598427),
    cohort_pct_loss = c(0.109297664556875, 0.190518923780758, 0.287314610933076),
    person_target_balance_real = c(68327.4195471203, 181459.29056582, 240414.234556388),
    person_required_matched_rate = c(0.0208312446716147, 0.0581827490302785, 0.077692459873084)
  )

  testthat::expect_equal(actual$fixture, expected$fixture)
  testthat::expect_equal(actual$fiscal_npv_billion, expected$fiscal_npv_billion, tolerance = 1e-6)
  testthat::expect_equal(actual$cohort_pct_loss, expected$cohort_pct_loss, tolerance = 1e-8)
  testthat::expect_equal(actual$person_target_balance_real, expected$person_target_balance_real, tolerance = 1e-4)
  testthat::expect_equal(actual$person_required_matched_rate, expected$person_required_matched_rate, tolerance = 1e-8)
})
