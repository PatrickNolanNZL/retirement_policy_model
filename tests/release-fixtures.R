nzs_release_fixture_policy <- function(name) {
  policy <- nzs_default_fiscal_policy()

  if (identical(name, "eligibility_age_only")) {
    policy$indexation_regime <- "A"
    policy$income_test_active <- FALSE
    policy$eligibility_age_start_year <- 2027
    policy$eligibility_age_new_age <- 67
    policy$eligibility_age_phase_in_months_per_year <- 0
  } else if (identical(name, "cpi_indexation_only")) {
    policy$eligibility_age_active <- FALSE
    policy$income_test_active <- FALSE
    policy$indexation_regime <- "B"
    policy$indexation_start_year <- 2027
  } else if (identical(name, "combined_reform")) {
    policy$eligibility_age_start_year <- 2027
    policy$eligibility_age_new_age <- 67
    policy$eligibility_age_phase_in_months_per_year <- nzs_phase_in_months_per_year(67, 4)
    policy$income_test_active <- TRUE
    policy$income_test_start_year <- 2027
    policy$income_test_from_age <- 65
    policy$income_test_to_age <- 70
    policy$income_test_threshold <- 10000
    policy$income_test_abatement_rate <- 0.25
    policy$indexation_regime <- "B"
    policy$indexation_start_year <- 2027
  } else {
    stop("Unknown release fixture: ", name, call. = FALSE)
  }

  policy
}

nzs_release_fixture_results <- function() {
  fiscal_inputs <- nzs_load_fiscal_inputs()
  person_inputs <- nzs_load_person_impacts_inputs()
  fixture_names <- c("eligibility_age_only", "cpi_indexation_only", "combined_reform")

  do.call(rbind, lapply(fixture_names, function(name) {
    policy <- nzs_release_fixture_policy(name)
    fiscal_path <- nzs_project_fiscal_impacts(policy, fiscal_inputs)
    fiscal_summary <- nzs_summarise_fiscal_impacts(
      fiscal_path,
      discount_rate = policy$discount_rate,
      base_year = policy$npv_base_year
    )
    cohort <- nzs_project_cohort_transition(
      fiscal_policy = policy,
      inputs = person_inputs,
      birth_years = 1975,
      nominal_discount_rate = policy$discount_rate
    )
    person <- nzs_project_person_impacts(
      fiscal_policy = policy,
      current_age = 45,
      earnings_archetype = "median",
      retirement_income_archetype = "medium",
      living_arrangement = "Single living alone",
      employee_rate = 0.04,
      employer_rate = 0.04,
      accumulation_return = nzs_kiwisaver_fund_returns()[["Balanced"]],
      drawdown_return = 0.025,
      current_balance = 0,
      inputs = person_inputs
    )

    data.frame(
      fixture = name,
      fiscal_npv_billion = fiscal_summary$value[fiscal_summary$metric == "Net fiscal saving NPV"],
      cohort_pct_loss = cohort$pct_loss,
      person_target_balance_real = person$target_balance_real,
      person_required_matched_rate = person$required_matched_rate,
      stringsAsFactors = FALSE
    )
  }))
}
