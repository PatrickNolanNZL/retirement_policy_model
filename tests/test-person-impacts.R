testthat::test_that("ESCT bands and net employer contributions are calculated", {
  nzs_source_model()

  testthat::expect_equal(nzs_esct_rate(18000), 0.105)
  testthat::expect_equal(nzs_esct_rate(50000), 0.175)
  testthat::expect_equal(nzs_esct_rate(80000), 0.30)
  testthat::expect_equal(nzs_esct_rate(120000), 0.33)
  testthat::expect_equal(nzs_esct_rate(250000), 0.39)

  inputs <- nzs_load_person_impacts_inputs()
  path <- nzs_project_kiwisaver_contributions(
    current_age = 64,
    earnings_archetype = "median",
    employee_rate = 0.03,
    employer_rate = 0.03,
    accumulation_return = 0,
    inputs = inputs
  )

  testthat::expect_equal(nrow(path), 1)
  testthat::expect_equal(
    path$employer_net_contribution,
    path$employer_gross_contribution * (1 - path$esct_rate)
  )
})

testthat::test_that("government contribution applies cap, income cap, and age cutoff", {
  nzs_source_model()

  testthat::expect_equal(nzs_government_kiwisaver_contribution(1000, 60000, 30), 250)
  testthat::expect_equal(nzs_government_kiwisaver_contribution(2000, 60000, 30), 260.72)
  testthat::expect_equal(nzs_government_kiwisaver_contribution(2000, 190000, 30), 0)
  testthat::expect_equal(nzs_government_kiwisaver_contribution(2000, 60000, 65), 0)
})

testthat::test_that("contribution accumulation runs from current age through 64", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()

  path <- nzs_project_kiwisaver_contributions(
    current_age = 62,
    earnings_archetype = "median",
    employee_rate = 0.03,
    employer_rate = 0.03,
    accumulation_return = 0.035,
    inputs = inputs
  )
  testthat::expect_equal(path$age, 62:64)
  testthat::expect_true(all(path$government_contribution > 0))

  none <- nzs_project_kiwisaver_contributions(
    current_age = 65,
    earnings_archetype = "median",
    employee_rate = 0.03,
    employer_rate = 0.03,
    accumulation_return = 0.035,
    inputs = inputs
  )
  testthat::expect_equal(nrow(none), 0)
})

testthat::test_that("fast age-65 balance matches detailed contribution path", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()

  path <- nzs_project_kiwisaver_contributions(
    current_age = 35,
    earnings_archetype = "median",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    inputs = inputs,
    initial_balance = 12345
  )
  fast_balance <- nzs_kiwisaver_balance_at_65(
    current_age = 35,
    earnings_archetype = "median",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    inputs = inputs,
    initial_balance = 12345
  )

  testthat::expect_equal(fast_balance, path$closing_balance[nrow(path)])
})

testthat::test_that("KiwiSaver earnings are CPI-uprated from the real archetype profile", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  path <- nzs_project_kiwisaver_contributions(
    current_age = 45,
    earnings_archetype = "median",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    inputs = inputs
  )
  base_age57 <- inputs$earnings$median_annual_linear[inputs$earnings$age == 57]
  row57 <- path[path$age == 57, ]

  testthat::expect_equal(row57$annual_earnings / row57$cpi_factor, base_age57)
  testthat::expect_equal(row57$employee_contribution / row57$cpi_factor, 0.035 * base_age57)
})

testthat::test_that("archetype snapshot uses selected single-year inputs", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()

  snapshot <- nzs_person_archetype_snapshot(
    inputs = inputs,
    current_age = 42,
    earnings_archetype = "median",
    retirement_income_archetype = "medium"
  )

  testthat::expect_equal(snapshot$current_age_earnings_real, 81120)
  testthat::expect_equal(snapshot$age65_retirement_income_real, 16431.42)
  testthat::expect_equal(snapshot$earnings_archetype_label, "Median")
  testthat::expect_equal(snapshot$retirement_income_archetype_label, "Medium")
})

testthat::test_that("archetype guide is derived from loaded app data", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()

  guide <- nzs_person_archetype_guide_data(inputs)

  testthat::expect_equal(guide$archetype, c("Low", "Median / Medium", "High"))
  testthat::expect_equal(guide$peak_age, c(47, 47, 47))
  testthat::expect_equal(
    guide$age64_earnings_real,
    c(49192, 73788, 110682)
  )
  testthat::expect_equal(
    round(guide$average_retirement_income_65_69_real),
    c(410, 13631, 55631)
  )
})

testthat::test_that("survival weights are anchored and non-increasing", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()

  testthat::expect_equal(inputs$survival$survival_probability[inputs$survival$age == 65], 1)
  testthat::expect_true(all(diff(inputs$survival$survival_probability) <= 0))
})

testthat::test_that("EA exclusion removes income-test exposure", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$indexation_regime <- "A"
  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_start_year <- 2027
  policy$eligibility_age_new_age <- 67
  policy$eligibility_age_phase_in_months_per_year <- 0
  policy$income_test_active <- TRUE
  policy$income_test_start_year <- 2027
  policy$income_test_from_age <- 65
  policy$income_test_to_age <- 70

  losses <- nzs_project_adequacy_nzs_losses(
    fiscal_policy = policy,
    current_age = 64,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    inputs = inputs
  )

  excluded <- losses[losses$age %in% c(65, 66), ]
  testthat::expect_true(all(excluded$ea_loss > 0))
  testthat::expect_true(all(excluded$income_test_loss == 0))
  testthat::expect_true(all(excluded$indexation_loss == 0))
})

testthat::test_that("retirement-income archetype is independent of working-life earnings", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$indexation_regime <- "A"
  policy$income_test_active <- TRUE
  policy$income_test_threshold <- 10000
  policy$income_test_abatement_rate <- 0.25

  low <- nzs_project_adequacy_nzs_losses(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    retirement_income_archetype = "low",
    living_arrangement = "Single living alone",
    inputs = inputs
  )
  high <- nzs_project_adequacy_nzs_losses(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    retirement_income_archetype = "high",
    living_arrangement = "Single living alone",
    inputs = inputs
  )

  testthat::expect_equal(low$retirement_income[low$age == 75] / low$cpi_factor[low$age == 75], 0)
  testthat::expect_gt(high$retirement_income[high$age == 65], low$retirement_income[low$age == 65])
  testthat::expect_gt(sum(high$income_test_loss), sum(low$income_test_loss))
})

testthat::test_that("grandparenting protects existing recipients for income testing", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$indexation_regime <- "A"
  policy$income_test_active <- TRUE
  policy$income_test_start_year <- 2027
  policy$income_test_from_age <- 65
  policy$income_test_to_age <- 70
  policy$income_test_grandparenting <- TRUE

  protected <- nzs_project_adequacy_nzs_losses(
    fiscal_policy = policy,
    current_age = 64,
    earnings_archetype = "high",
    living_arrangement = "Single living alone",
    inputs = inputs
  )
  testthat::expect_true(all(protected$income_test_loss == 0))

  unprotected <- nzs_project_adequacy_nzs_losses(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "high",
    living_arrangement = "Single living alone",
    inputs = inputs
  )
  testthat::expect_true(any(unprotected$income_test_loss > 0))
})

testthat::test_that("adequacy balances expose real 2026 display values", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- TRUE
  policy$indexation_regime <- "A"

  result <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    employee_rate = 0.03,
    employer_rate = 0.03,
    accumulation_return = 0.035,
    inputs = inputs
  )

  testthat::expect_true(result$retirement_cpi_factor > 1)
  testthat::expect_equal(result$target_balance_real, result$target_balance / result$retirement_cpi_factor)
  testthat::expect_equal(result$projected_balance_real, result$projected_balance / result$retirement_cpi_factor)
  testthat::expect_true("real_closing_balance" %in% names(result$contributions))
})

testthat::test_that("current balance affects projected adequacy without changing NZS target", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()

  zero <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    current_balance = 0,
    inputs = inputs
  )
  with_balance <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    current_balance = 50000,
    inputs = inputs
  )

  testthat::expect_equal(with_balance$target_balance, zero$target_balance)
  testthat::expect_gt(with_balance$projected_balance, zero$projected_balance)
  testthat::expect_lte(with_balance$required_matched_rate, zero$required_matched_rate)
})

testthat::test_that("working-life profile flags context and matches active contribution engine", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  profile <- nzs_kiwisaver_working_life_profile(
    current_age = 45,
    earnings_archetype = "median",
    employee_rate = 0.035,
    employer_rate = 0.035,
    inputs = inputs
  )
  contribution_path <- nzs_project_kiwisaver_contributions(
    current_age = 45,
    earnings_archetype = "median",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0,
    inputs = inputs
  )

  testthat::expect_true(all(!profile$active[profile$age < 45]))
  testthat::expect_true(all(profile$active[profile$age >= 45]))
  active <- profile[profile$age == 45, ]
  contribution <- contribution_path[contribution_path$age == 45, ]
  testthat::expect_equal(active$real_employee_contribution, contribution$employee_contribution / contribution$cpi_factor)
  testthat::expect_equal(active$real_employer_net_contribution, contribution$employer_net_contribution / contribution$cpi_factor)
})

testthat::test_that("retirement-income decomposition reconciles NZS and excludes drawdown from income testing", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$income_test_active <- TRUE
  policy$income_test_threshold <- 0
  policy$income_test_abatement_rate <- 0.25

  result <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "high",
    living_arrangement = "Single living alone",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    current_balance = 200000,
    inputs = inputs
  )
  retirement <- result$retirement_income
  losses <- result$losses

  testthat::expect_equal(retirement$status_quo_nzs_real - retirement$reform_nzs_real, losses$real_total_loss)
  testthat::expect_true(any(retirement$kiwisaver_drawdown_real > 0))
  abatement_from_retirement_income <- pmin(
    losses$reform_annual + losses$income_test_loss,
    policy$income_test_abatement_rate * pmax(0, losses$retirement_income)
  )
  tested <- losses$income_test_loss > 0
  testthat::expect_equal(losses$income_test_loss[tested], abatement_from_retirement_income[tested])
})

testthat::test_that("statutory KiwiSaver default schedule rises to 4% in 2028", {
  testthat::expect_equal(
    nzs_statutory_kiwisaver_default_rate(c(2026, 2027, 2028, 2030)),
    c(0.035, 0.035, 0.04, 0.04)
  )
})

testthat::test_that("retirement-income status quo line uses the enacted KiwiSaver contribution schedule", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()

  result <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    employee_rate = 0.08,
    employer_rate = 0.08,
    accumulation_return = 0.035,
    current_balance = 10000,
    inputs = inputs
  )
  default_years <- 2026 + 45:64 - 45
  default_path <- nzs_project_kiwisaver_contributions(
    current_age = 45,
    earnings_archetype = "median",
    employee_rate = nzs_statutory_kiwisaver_default_rate(default_years),
    employer_rate = nzs_statutory_kiwisaver_default_rate(default_years),
    accumulation_return = 0.035,
    inputs = inputs,
    initial_balance = 10000
  )
  testthat::expect_equal(default_path$employee_rate[default_path$year == 2026], 0.035)
  testthat::expect_equal(default_path$employer_rate[default_path$year == 2027], 0.035)
  testthat::expect_equal(default_path$employee_rate[default_path$year == 2028], 0.04)
  expected_default_balance <- default_path$closing_balance[nrow(default_path)]
  expected_status_quo_drawdown <- nzs_constant_real_kiwisaver_drawdown(
    result$losses,
    expected_default_balance,
    0.025
  )

  testthat::expect_gt(result$retirement_income$kiwisaver_drawdown_real[[1]], result$retirement_income$status_quo_kiwisaver_drawdown_real[[1]])
  testthat::expect_equal(unique(result$retirement_income$status_quo_kiwisaver_drawdown_real), expected_status_quo_drawdown)
})

testthat::test_that("constant real drawdown has intended survival-weighted PV", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  result <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    current_balance = 50000,
    inputs = inputs
  )
  drawdown <- unique(result$retirement_income$kiwisaver_drawdown_real)
  periods <- result$losses$age - 65
  pv <- sum(drawdown * result$losses$cpi_factor * result$losses$survival_probability / (1 + 0.025) ^ periods)

  testthat::expect_length(drawdown, 1)
  testthat::expect_equal(pv, result$projected_balance, tolerance = 1)
})

testthat::test_that("replacement rates use age-64 earnings and the default 65-69 window", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  result <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    inputs = inputs
  )
  denominator <- nzs_real_age64_earnings(45, "median", inputs)
  window <- result$retirement_income[result$retirement_income$age >= 65 & result$retirement_income$age <= 69, ]

  testthat::expect_equal(denominator, inputs$earnings$median_annual_linear[inputs$earnings$age == 64])
  testthat::expect_equal(unique(result$replacement_rates$age64_earnings_real), denominator)
  testthat::expect_equal(
    result$replacement_rates$replacement_rate[result$replacement_rates$metric == "Reform"],
    mean(window$reform_total_real) / denominator
  )
})

testthat::test_that("required-rate-by-age sensitivity uses zero current balance", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  by_age <- nzs_kiwisaver_required_rate_by_age(
    fiscal_policy = policy,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    inputs = inputs,
    ages = 45:46
  )
  selected_with_balance <- nzs_person_impacts_fund_sensitivity(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    current_balance = 100000,
    inputs = inputs
  )
  selected_zero <- nzs_person_impacts_fund_sensitivity(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    current_balance = 0,
    inputs = inputs
  )
  growth_age <- by_age[by_age$current_age == 45 & by_age$fund_type == "Growth", ]
  zero_growth <- selected_zero[selected_zero$fund_type == "Growth", ]
  with_balance_growth <- selected_with_balance[selected_with_balance$fund_type == "Growth", ]

  testthat::expect_equal(sort(unique(by_age$current_age)), 45:46)
  testthat::expect_equal(growth_age$required_matched_rate, zero_growth$required_matched_rate)
  testthat::expect_lte(with_balance_growth$required_matched_rate, zero_growth$required_matched_rate)
  finite <- by_age[is.finite(by_age$required_matched_rate) & by_age$current_age == 45, ]
  testthat::expect_true(all(diff(finite$required_matched_rate[order(finite$accumulation_return)]) <= 0))
})

testthat::test_that("matched-rate solver closes target balance", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  target <- 100000
  rate <- nzs_solve_matched_kiwisaver_rate(
    target_balance = target,
    current_age = 45,
    earnings_archetype = "median",
    accumulation_return = 0.035,
    inputs = inputs
  )
  path <- nzs_project_kiwisaver_contributions(
    current_age = 45,
    earnings_archetype = "median",
    employee_rate = rate,
    employer_rate = rate,
    accumulation_return = 0.035,
    inputs = inputs
  )
  testthat::expect_equal(path$closing_balance[nrow(path)], target, tolerance = 1)
})

testthat::test_that("inactive policies produce no adequacy NZS losses", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- FALSE
  policy$indexation_regime <- "A"

  losses <- nzs_project_adequacy_nzs_losses(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    inputs = inputs
  )
  testthat::expect_true(all(abs(losses$total_loss) < 1e-6))
})

testthat::test_that("CPI-only adequacy targets use the shared NZS rate path", {
  nzs_source_model()
  inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- FALSE
  policy$indexation_regime <- "B"
  policy$indexation_start_year <- 2027

  result <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 45,
    earnings_archetype = "median",
    living_arrangement = "Single living alone",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = 0.035,
    inputs = inputs
  )

  testthat::expect_gt(result$target_balance_real, 0)
  testthat::expect_gt(result$required_matched_rate, 0)
})
