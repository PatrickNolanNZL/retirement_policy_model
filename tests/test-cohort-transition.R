source("helper-model.R")
project_root <- nzs_source_model()
source(file.path(project_root, "app/R/ui/plotting.R"))
source(file.path(project_root, "app/R/ui/plotting_cohort.R"))

testthat::test_that("cohort transition defaults use birth years for current cohorts", {
  years <- nzs_default_cohort_transition_birth_years()
  testthat::expect_equal(min(years), 1956)
  testthat::expect_equal(max(years), 2006)
})

testthat::test_that("cohort transition is calculated before income testing", {
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$indexation_regime <- "B"
  policy$income_test_active <- FALSE
  without_it <- nzs_project_cohort_transition(policy, inputs, birth_years = 1960:1970)

  policy$income_test_active <- TRUE
  policy$income_test_threshold <- 0
  policy$income_test_abatement_rate <- 1
  policy$income_test_from_age <- 65
  policy$income_test_to_age <- 90
  with_it <- nzs_project_cohort_transition(policy, inputs, birth_years = 1960:1970)

  testthat::expect_equal(with_it$pct_loss, without_it$pct_loss)
})

testthat::test_that("CPI-only transition losses use the shared NZS rate path", {
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- FALSE
  policy$indexation_regime <- "B"
  policy$indexation_start_year <- 2027

  out <- nzs_project_cohort_transition(policy, inputs, birth_years = 1975)
  testthat::expect_gt(out$pct_loss, 0)
})

testthat::test_that("cohort deferral captures near-retirement eligibility-age delays", {
  inputs <- nzs_load_fiscal_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_start_year <- 2027
  policy$eligibility_age_new_age <- 67
  policy$eligibility_age_phase_in_months_per_year <- 0
  policy$eligibility_age_grandparenting <- FALSE

  out <- nzs_project_cohort_deferral(policy, inputs, ages = 63:64)
  testthat::expect_equal(out$years_deferred[out$current_age == 64], 2)
  testthat::expect_equal(out$years_deferred[out$current_age == 63], 2)
  testthat::expect_true(all(out$population > 0))
  testthat::expect_equal(out$deferred_person_years, out$population * out$years_deferred)
})

testthat::test_that("cohort deferral defaults to birth years around the EA reform window", {
  inputs <- nzs_load_fiscal_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_start_year <- 2030
  policy$eligibility_age_new_age <- 67
  policy$eligibility_age_phase_in_months_per_year <- 6

  out <- nzs_project_cohort_deferral(policy, inputs)
  testthat::expect_equal(min(out$birth_year), 1960)
  testthat::expect_equal(max(out$birth_year), 1973)
  testthat::expect_equal(out$current_age, 2026 - out$birth_year)
})

testthat::test_that("cohort deferral respects eligibility-age grandparenting", {
  inputs <- nzs_load_fiscal_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_start_year <- 2027
  policy$eligibility_age_new_age <- 67
  policy$eligibility_age_phase_in_months_per_year <- 0
  policy$eligibility_age_grandparenting <- TRUE

  out <- nzs_project_cohort_deferral(policy, inputs, ages = 63:64)
  testthat::expect_equal(out$years_deferred[out$current_age == 64], 0)
  testthat::expect_equal(out$years_deferred[out$current_age == 63], 2)
})

testthat::test_that("eligibility-age grandparenting tags protected cohorts", {
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$indexation_regime <- "A"
  policy$income_test_active <- FALSE
  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_start_year <- 2027
  policy$eligibility_age_new_age <- 67
  policy$eligibility_age_phase_in_months_per_year <- 0
  policy$eligibility_age_grandparenting <- TRUE

  out <- nzs_project_cohort_transition(policy, inputs, birth_years = 1959:1963)
  protected <- out[out$birth_year <= 1962, ]
  testthat::expect_true(all(protected$protected))
  testthat::expect_true(all(protected$status == "Grandparented"))
  testthat::expect_true(out$status[out$birth_year == 1963] == "Fully affected")
})

testthat::test_that("eligibility-age phase-in tags transition cohorts", {
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$indexation_regime <- "A"
  policy$income_test_active <- FALSE
  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_start_year <- 2027
  policy$eligibility_age_new_age <- 67
  policy$eligibility_age_phase_in_months_per_year <- 6
  policy$eligibility_age_grandparenting <- FALSE

  out <- nzs_project_cohort_transition(policy, inputs, birth_years = 1962:1968)
  testthat::expect_true(any(out$status == "Phase-in"))
  testthat::expect_true(any(out$status == "Fully affected"))
  annotations <- attr(out, "annotations", exact = TRUE)
  testthat::expect_equal(annotations$eligibility_age_marker, 1962)
  testthat::expect_true(is.finite(annotations$phase_in_birth_year_min))
  testthat::expect_true(is.finite(annotations$phase_in_birth_year_max))
})

testthat::test_that("eligibility-age phase-in annotations include base-year start boundary", {
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$indexation_regime <- "A"
  policy$income_test_active <- FALSE
  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_start_year <- 2026
  policy$eligibility_age_new_age <- 67
  policy$eligibility_age_phase_in_months_per_year <- nzs_phase_in_months_per_year(67, 2)

  out <- nzs_project_cohort_transition(policy, inputs, birth_years = 1959:1964)
  annotations <- attr(out, "annotations", exact = TRUE)
  testthat::expect_equal(annotations$eligibility_age_marker, 1961)
  testthat::expect_equal(annotations$phase_in_birth_year_min, 1961)
  testthat::expect_equal(annotations$phase_in_birth_year_max, 1962)
})

testthat::test_that("indexation reform affects post-start payments for existing retirees unless grandparented", {
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- FALSE
  policy$indexation_regime <- "B"
  policy$indexation_start_year <- 2040
  policy$indexation_grandparenting <- FALSE

  out <- nzs_project_cohort_transition(policy, inputs, birth_years = 1970:1976)
  testthat::expect_true(all(out$pct_loss > 0))
  testthat::expect_true(all(out$indexation_loss_any))

  policy$indexation_grandparenting <- TRUE
  protected <- nzs_project_cohort_transition(policy, inputs, birth_years = 1970:1976)
  testthat::expect_equal(protected$pct_loss[protected$birth_year <= 1975], rep(0, 6))
  testthat::expect_true(protected$pct_loss[protected$birth_year == 1976] > 0)
})

testthat::test_that("cohort transition uses selected nominal discount rate with CPI-adjusted real discounting", {
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- FALSE
  policy$indexation_regime <- "B"
  policy$indexation_start_year <- 2027

  low_discount <- nzs_project_cohort_transition(policy, inputs, birth_years = 1975, nominal_discount_rate = 0)
  high_discount <- nzs_project_cohort_transition(policy, inputs, birth_years = 1975, nominal_discount_rate = 0.1)

  testthat::expect_gt(low_discount$pv_loss, high_discount$pv_loss)
  annotations <- attr(high_discount, "annotations", exact = TRUE)
  testthat::expect_equal(annotations$nominal_discount_rate, 0.1)
  testthat::expect_equal(annotations$discount_basis, "real_2026_cpi_adjusted")
})

testthat::test_that("cohort transition plot renders without error", {
  inputs <- nzs_load_person_impacts_inputs()
  out <- nzs_project_cohort_transition(nzs_default_fiscal_policy(), inputs, birth_years = 1960:1970)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  testthat::expect_no_error(plot_cohort_transition_staircase(out))
})

testthat::test_that("cohort deferral plot renders without error", {
  inputs <- nzs_load_fiscal_inputs()
  out <- nzs_project_cohort_deferral(nzs_default_fiscal_policy(), inputs)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  testthat::expect_no_error(plot_cohort_deferral_by_age(out))
})

testthat::test_that("income-test incidence plot renders without error", {
  inputs <- nzs_load_fiscal_inputs()
  out <- nzs_current_income_test_incidence(nzs_default_fiscal_policy(), inputs)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  testthat::expect_no_error(plot_income_test_incidence(out, metric = "people"))
  testthat::expect_no_error(plot_income_test_incidence(out, metric = "share"))
  testthat::expect_no_error(plot_income_test_incidence(out, metric = "percent_loss"))
})
