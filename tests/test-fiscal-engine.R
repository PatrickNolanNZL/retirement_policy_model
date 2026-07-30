nzs_source_model()

testthat::test_that("fiscal inputs load with required tables", {
  inputs <- nzs_load_fiscal_inputs()

  testthat::expect_true(all(c("year", "nzs_expense_billion", "nominal_gdp_billion") %in% names(inputs$fiscal_baseline)))
  testthat::expect_true(all(c("year", "age", "sex", "population") %in% names(inputs$population)))
  testthat::expect_true(all(c("age_band", "living_arrangement", "payment_category", "weighted_mean_income", "weighted_n") %in% names(inputs$income_histogram)))

  histogram_totals <- stats::aggregate(weighted_n ~ age_band, inputs$income_histogram, sum)
  testthat::expect_true(all(abs(histogram_totals$weighted_n - 1) < 1e-8))

  testthat::expect_true(all(c("parameter", "value", "source") %in% names(inputs$benefit_assumptions)))
})

testthat::test_that("category rate paths apply selected regime columns", {
  rates <- data.frame(
    current_formula_annual = c(100, 200),
    cpi_annual = c(90, 180),
    wage_annual = c(110, 220)
  )

  testthat::expect_equal(nzs_selected_annual_rate(rates, "A"), c(100, 200))
  testthat::expect_equal(nzs_selected_annual_rate(rates, "B"), c(90, 180))
  testthat::expect_equal(nzs_selected_annual_rate(rates, "C"), c(110, 220))
  testthat::expect_error(nzs_selected_annual_rate(rates, "bad"), "Unknown indexation regime")
})

testthat::test_that("prepared category rates retain the published 2026 anchor", {
  inputs <- nzs_load_fiscal_inputs()
  defaults <- nzs_default_inputs()
  rates_2026 <- inputs$rate_paths[inputs$rate_paths$year == 2026, ]
  expected <- defaults$current_couple_net_weekly * c(
    single_living_alone = 0.65,
    single_sharing = 0.60,
    couple_each = 0.50
  ) * 52

  actual <- rates_2026$current_formula_annual[match(names(expected), rates_2026$rate_category)]
  testthat::expect_equal(actual, unname(expected))
  testthat::expect_equal(rates_2026$current_formula_annual, rates_2026$cpi_annual)
  testthat::expect_true(all(rates_2026$wage_annual < rates_2026$current_formula_annual))
})

testthat::test_that("prepared wage-floor rates exceed CPI-only rates under the LTFM path", {
  inputs <- nzs_load_fiscal_inputs()
  final_year <- max(inputs$rate_paths$year)
  final_rates <- inputs$rate_paths[inputs$rate_paths$year == final_year, ]

  testthat::expect_true(all(final_rates$wage_annual > final_rates$cpi_annual))
  testthat::expect_equal(final_rates$current_formula_annual, final_rates$wage_annual)
})

testthat::test_that("aggregate indexation uses the LTFM CPI-to-wage fiscal ratio", {
  testthat::expect_equal(nzs_aggregate_indexation_ratio("A", cum_cpi = 1.2, cum_wage = 1.5), 1)
  testthat::expect_equal(nzs_aggregate_indexation_ratio("B", cum_cpi = 1.2, cum_wage = 1.5), 0.8)
  testthat::expect_equal(nzs_aggregate_indexation_ratio("C", cum_cpi = 1.2, cum_wage = 1.5), 1)
  testthat::expect_error(nzs_aggregate_indexation_ratio("bad", 1.2, 1.5), "Unknown indexation regime")
})

testthat::test_that("fiscal cumulative growth starts from the model base year", {
  testthat::expect_equal(nzs_cumulative_growth_factor(c(0.5, 0.1, 0.2)), c(1, 1.1, 1.32))
})

testthat::test_that("aggregate indexation ratio respects implementation start year", {
  baseline <- data.frame(
    year = 2026:2028,
    cum_cpi = c(1, 1.1, 1.21),
    cum_wage = c(1, 1.2, 1.44)
  )

  testthat::expect_equal(nzs_started_aggregate_indexation_ratio(baseline, 1, "B", 2027), 1)
  testthat::expect_equal(nzs_started_aggregate_indexation_ratio(baseline, 2, "B", 2028), 1)
  testthat::expect_equal(nzs_started_aggregate_indexation_ratio(baseline, 3, "B", 2028), (1.21 / 1.1) / (1.44 / 1.2))
})

testthat::test_that("fiscal policy validates supported CPI-threshold conventions", {
  policy <- nzs_default_fiscal_policy()
  testthat::expect_silent(nzs_validate_fiscal_policy(policy))

  policy$income_test_to_age <- policy$income_test_from_age
  testthat::expect_error(nzs_validate_fiscal_policy(policy), "end age")

  policy <- nzs_default_fiscal_policy()
  policy$indexation_regime <- "unsupported"
  testthat::expect_error(nzs_validate_fiscal_policy(policy), "unknown indexation regime")
})

testthat::test_that("income histogram abatement respects threshold, rate, and category cap", {
  histogram <- data.frame(weighted_mean_income = c(5000, 20000, 100000))

  out <- nzs_income_test_abatement_from_histogram(
    histogram,
    threshold = 10000,
    rate = 0.25,
    cap = c(10000, 10000, 15000)
  )

  testthat::expect_equal(out, c(0, 2500, 15000))
})

testthat::test_that("current income-test incidence aggregates public histogram bands", {
  inputs <- nzs_load_fiscal_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$income_test_active <- TRUE
  policy$income_test_from_age <- 65
  policy$income_test_to_age <- 70
  policy$income_test_threshold <- 10000
  policy$income_test_abatement_rate <- 0.25

  out <- nzs_current_income_test_incidence(policy, inputs)
  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(c(
    "estimated_people",
    "estimated_people_affected",
    "share_affected",
    "affected_population_share",
    "average_percent_loss_affected"
  ) %in% names(out)))
  testthat::expect_true(any(out$estimated_people_affected > 0))
  testthat::expect_true(all(out$share_affected >= 0 & out$share_affected <= 1))
  testthat::expect_true(all(out$affected_population_share >= 0 & out$affected_population_share <= 1))
  testthat::expect_gt(sum(out$affected_population_share), 0)
  testthat::expect_equal(
    sum(out$affected_population_share),
    sum(out$estimated_people_affected) / sum(out$estimated_people)
  )

  policy$income_test_active <- FALSE
  inactive <- nzs_current_income_test_incidence(policy, inputs)
  testthat::expect_equal(nrow(inactive), 0)
})

testthat::test_that("eligibility-age phase-in handles inactive, immediate, and phased paths", {
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  testthat::expect_equal(nzs_effective_eligibility_age(2027, policy), 65)

  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_phase_in_months_per_year <- 0
  policy$eligibility_age_new_age <- 67
  testthat::expect_equal(nzs_effective_eligibility_age(2027, policy), 67)

  policy$eligibility_age_phase_in_months_per_year <- 12
  testthat::expect_equal(nzs_effective_eligibility_age(2027, policy), 66)
  testthat::expect_equal(nzs_effective_eligibility_age(2028, policy), 67)
})

testthat::test_that("phase-in period converts to eligibility-age months per year", {
  testthat::expect_equal(nzs_phase_in_months_per_year(new_age = 67, phase_in_years = 0), 0)
  testthat::expect_equal(nzs_phase_in_months_per_year(new_age = 67, phase_in_years = 2), 12)
  testthat::expect_equal(nzs_phase_in_months_per_year(new_age = 67, phase_in_years = 4), 6)
  testthat::expect_equal(nzs_phase_in_months_per_year(new_age = 67, phase_in_years = 8), 3)
})

testthat::test_that("eligibility-age exposure prorates fractional effective ages", {
  testthat::expect_equal(nzs_age_excluded_share(c(65, 66, 67), 65), c(0, 0, 0))
  testthat::expect_equal(nzs_age_excluded_share(c(65, 66, 67), 65.5), c(0.5, 0, 0))
  testthat::expect_equal(nzs_age_excluded_share(c(65, 66, 67), 66.25), c(1, 0.25, 0))
  testthat::expect_equal(nzs_age_excluded_share(c(65, 66, 67), 67), c(1, 1, 0))
})

testthat::test_that("EA grandparenting protects people aged 65 plus at policy start", {
  ages_2027 <- c(64, 65, 66)
  protected_2027 <- nzs_ea_protected_share(2027, ages_2027, start_year = 2027, grandparenting = TRUE)
  testthat::expect_equal(protected_2027, c(0, 1, 1))

  ages_2028 <- c(65, 66, 67)
  protected_2028 <- nzs_ea_protected_share(2028, ages_2028, start_year = 2027, grandparenting = TRUE)
  testthat::expect_equal(protected_2028, c(0, 1, 1))

  no_gp <- nzs_ea_protected_share(2028, ages_2028, start_year = 2027, grandparenting = FALSE)
  testthat::expect_equal(no_gp, c(0, 0, 0))
})

testthat::test_that("eligibility-age exposure table carries excluded and remaining shares", {
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_start_year <- 2027
  policy$eligibility_age_new_age <- 67
  policy$eligibility_age_phase_in_months_per_year <- 6
  policy$eligibility_age_grandparenting <- FALSE

  pop <- data.frame(
    year = 2027,
    age = c(64, 65, 66, 67),
    sex = "female",
    population = 100
  )
  out <- nzs_apply_eligibility_age_exposure(pop, 2027, policy)

  testthat::expect_equal(out$age, c(65, 66, 67))
  testthat::expect_equal(out$ea_excluded_share, c(0.5, 0, 0))
  testthat::expect_equal(out$remaining_share, c(0.5, 1, 1))
})

testthat::test_that("slower eligibility-age phase-in delays early fiscal savings", {
  inputs <- nzs_load_fiscal_inputs()
  immediate <- nzs_default_fiscal_policy()
  immediate$income_test_active <- FALSE
  immediate$indexation_regime <- "A"
  immediate$eligibility_age_phase_in_months_per_year <- 0

  phased <- immediate
  phased$eligibility_age_phase_in_months_per_year <- 6

  immediate_path <- nzs_project_fiscal_impacts(immediate, inputs)
  phased_path <- nzs_project_fiscal_impacts(phased, inputs)

  early_year <- 2027
  testthat::expect_lt(
    phased_path$ea_gross_saving_billion[phased_path$year == early_year],
    immediate_path$ea_gross_saving_billion[immediate_path$year == early_year]
  )
})

testthat::test_that("EA grandparenting reduces eligibility-age fiscal savings", {
  inputs <- nzs_load_fiscal_inputs()
  no_gp <- nzs_default_fiscal_policy()
  no_gp$income_test_active <- FALSE
  no_gp$indexation_regime <- "A"
  no_gp$eligibility_age_start_year <- 2027
  no_gp$eligibility_age_phase_in_months_per_year <- 0
  no_gp$eligibility_age_new_age <- 67
  no_gp$eligibility_age_grandparenting <- FALSE

  gp <- no_gp
  gp$eligibility_age_grandparenting <- TRUE

  no_gp_path <- nzs_project_fiscal_impacts(no_gp, inputs)
  gp_path <- nzs_project_fiscal_impacts(gp, inputs)

  year <- 2027
  testthat::expect_gt(no_gp_path$ea_gross_saving_billion[no_gp_path$year == year], 0)
  testthat::expect_equal(gp_path$ea_gross_saving_billion[gp_path$year == year], 0)

  later_year <- 2028
  testthat::expect_lt(
    gp_path$ea_gross_saving_billion[gp_path$year == later_year],
    no_gp_path$ea_gross_saving_billion[no_gp_path$year == later_year]
  )
})

testthat::test_that("grandparenting creates an entrant-cohort ramp", {
  ages <- 65:70

  no_gp <- nzs_post_reform_share(2028, ages, start_year = 2027, grandparenting = FALSE)
  gp <- nzs_post_reform_share(2028, ages, start_year = 2027, grandparenting = TRUE)

  testthat::expect_equal(no_gp, rep(1, length(ages)))
  testthat::expect_equal(gp, as.numeric(ages < 66))
  testthat::expect_lt(sum(gp), sum(no_gp))
})

nzs_test_income_inputs <- function() {
  list(
    rate_paths = data.frame(
      year = rep(2026:2028, each = 1),
      living_arrangement = "single_living_alone",
      current_formula_annual = c(100, 100, 100),
      cpi_annual = c(100, 50, 50),
      wage_annual = c(100, 100, 100)
    )
  )
}

nzs_test_income_histogram <- function(income = 1000) {
  data.frame(
    age_band = "65-69",
    age_from = 65,
    age_to = 70,
    living_arrangement = "single_living_alone",
    payment_category = "single_living_alone",
    income_lower = 0,
    income_upper = Inf,
    weighted_mean_income = income,
    weighted_n = 1
  )
}

nzs_test_income_policy <- function() {
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$indexation_regime <- "A"
  policy$income_test_active <- TRUE
  policy$income_test_start_year <- 2027
  policy$income_test_from_age <- 65
  policy$income_test_to_age <- 70
  policy$income_test_threshold <- 0
  policy$income_test_abatement_rate <- 1
  policy$income_test_grandparenting <- FALSE
  policy
}

testthat::test_that("income testing is zero before its own start year", {
  out <- nzs_income_test_saving_for_year(
    year = 2026,
    policy = nzs_test_income_policy(),
    inputs = nzs_test_income_inputs(),
    population = data.frame(age = 65, sex = "female", population = 100, remaining_share = 1),
    histogram = nzs_test_income_histogram(),
    rate_paths = data.frame(),
    income_factor = 1,
    cpi_factor = 1
  )

  testthat::expect_equal(out, 0)
})

testthat::test_that("income-test grandparenting protects people aged 65 plus at policy start", {
  policy <- nzs_test_income_policy()
  policy$income_test_grandparenting <- TRUE

  protected <- nzs_income_test_saving_for_year(
    year = 2027,
    policy = policy,
    inputs = nzs_test_income_inputs(),
    population = data.frame(age = 65, sex = "female", population = 100, remaining_share = 1),
    histogram = nzs_test_income_histogram(),
    rate_paths = data.frame(),
    income_factor = 1,
    cpi_factor = 1
  )
  entrant <- nzs_income_test_saving_for_year(
    year = 2028,
    policy = policy,
    inputs = nzs_test_income_inputs(),
    population = data.frame(age = 65, sex = "female", population = 100, remaining_share = 1),
    histogram = nzs_test_income_histogram(),
    rate_paths = data.frame(),
    income_factor = 1,
    cpi_factor = 1
  )

  testthat::expect_equal(protected, 0)
  testthat::expect_equal(entrant, 100 * 100 / 1e9)
})

testthat::test_that("income-test caps use status quo before indexation start year", {
  policy <- nzs_test_income_policy()
  policy$indexation_regime <- "B"
  policy$indexation_start_year <- 2028

  out <- nzs_income_test_saving_for_year(
    year = 2027,
    policy = policy,
    inputs = nzs_test_income_inputs(),
    population = data.frame(age = 65, sex = "female", population = 100, remaining_share = 1),
    histogram = nzs_test_income_histogram(),
    rate_paths = data.frame(),
    income_factor = 1,
    cpi_factor = 1
  )

  testthat::expect_equal(out, 100 * 100 / 1e9)
})

testthat::test_that("indexation-grandparented people use status quo caps inside income testing", {
  policy <- nzs_test_income_policy()
  policy$indexation_regime <- "B"
  policy$indexation_start_year <- 2027
  policy$indexation_grandparenting <- TRUE

  out <- nzs_income_test_saving_for_year(
    year = 2028,
    policy = policy,
    inputs = nzs_test_income_inputs(),
    population = data.frame(age = c(65, 66), sex = "female", population = c(100, 100), remaining_share = 1),
    histogram = nzs_test_income_histogram(),
    rate_paths = data.frame(),
    income_factor = 1,
    cpi_factor = 1
  )

  testthat::expect_equal(out, (100 * 50 + 100 * 100) / 1e9)
})

testthat::test_that("public-source benefit offset share uses refreshed MSD central calibration", {
  inputs <- nzs_load_fiscal_inputs()
  values <- setNames(inputs$benefit_assumptions$value, inputs$benefit_assumptions$parameter)

  expected_main_blend <- sum(
    values[c("public_jss_mix_share", "public_slp_mix_share", "public_eb_mix_share", "public_sps_mix_share")] *
      values[c("public_jss_benefit_to_nzs", "public_slp_benefit_to_nzs", "public_eb_benefit_to_nzs", "public_sps_benefit_to_nzs")]
  )
  expected <- values[["public_main_benefit_takeup"]] * expected_main_blend +
    values[["public_as_delta_takeup"]] * values[["public_as_amount_to_nzs"]] +
    values[["public_da_delta_takeup"]] * values[["public_da_amount_to_nzs"]] +
    values[["public_tas_delta_takeup"]] * values[["public_tas_amount_to_nzs"]]

  testthat::expect_equal(nzs_benefit_offset_share(inputs), expected, tolerance = 1e-10)
})

testthat::test_that("benefit offset uses the fixed central treatment without a WEP switch", {
  inputs <- nzs_load_fiscal_inputs()
  testthat::expect_false("wep_to_nzs" %in% inputs$benefit_assumptions$parameter)
  testthat::expect_true(is.finite(nzs_benefit_offset_share(inputs)))
})

testthat::test_that("eligibility-age removal precedes income-test exposure", {
  inputs <- nzs_load_fiscal_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$indexation_regime <- "A"
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- TRUE
  no_ea <- nzs_project_fiscal_impacts(policy, inputs)

  policy$eligibility_age_active <- TRUE
  policy$eligibility_age_phase_in_months_per_year <- 0
  policy$eligibility_age_new_age <- 70
  with_ea <- nzs_project_fiscal_impacts(policy, inputs)

  year <- 2027
  testthat::expect_gt(no_ea$income_test_saving_billion[no_ea$year == year], 0)
  testthat::expect_equal(with_ea$income_test_saving_billion[with_ea$year == year], 0)
})

testthat::test_that("fractional eligibility-age exposure partially reduces income-test exposure", {
  inputs <- nzs_load_fiscal_inputs()
  no_ea <- nzs_default_fiscal_policy()
  no_ea$indexation_regime <- "A"
  no_ea$eligibility_age_active <- FALSE
  no_ea$income_test_active <- TRUE
  no_ea$income_test_from_age <- 65
  no_ea$income_test_to_age <- 66

  fractional_ea <- no_ea
  fractional_ea$eligibility_age_active <- TRUE
  fractional_ea$eligibility_age_start_year <- 2027
  fractional_ea$eligibility_age_new_age <- 67
  fractional_ea$eligibility_age_phase_in_months_per_year <- 6

  no_ea_path <- nzs_project_fiscal_impacts(no_ea, inputs)
  fractional_path <- nzs_project_fiscal_impacts(fractional_ea, inputs)

  year <- 2027
  testthat::expect_gt(no_ea_path$income_test_saving_billion[no_ea_path$year == year], 0)
  testthat::expect_lt(
    fractional_path$income_test_saving_billion[fractional_path$year == year],
    no_ea_path$income_test_saving_billion[no_ea_path$year == year]
  )
  testthat::expect_gt(fractional_path$income_test_saving_billion[fractional_path$year == year], 0)
})

testthat::test_that("wage-uprated income with CPI-indexed thresholds increases later exposure", {
  inputs <- nzs_load_fiscal_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$indexation_regime <- "A"
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- TRUE
  path <- nzs_project_fiscal_impacts(policy, inputs)
  final_year <- max(path$year)
  testthat::expect_gt(
    path$income_test_saving_billion[path$year == final_year],
    path$income_test_saving_billion[path$year == min(path$year)]
  )
})

testthat::test_that("combined fiscal path preserves policy-channel accounting", {
  inputs <- nzs_load_fiscal_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$indexation_regime <- "B"
  policy$indexation_start_year <- 2027
  path <- nzs_project_fiscal_impacts(policy, inputs)

  testthat::expect_true(any(path$ea_gross_saving_billion > 0))
  testthat::expect_true(any(path$income_test_saving_billion > 0))
  testthat::expect_true(any(path$indexation_saving_billion > 0))
  testthat::expect_equal(
    path$net_saving_billion,
    path$ea_gross_saving_billion +
      path$income_test_saving_billion +
      path$indexation_saving_billion -
      path$ea_offset_billion
  )
})

testthat::test_that("inactive policy components produce zero fiscal savings", {
  inputs <- nzs_load_fiscal_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_active <- FALSE
  policy$income_test_active <- FALSE
  policy$indexation_regime <- "A"

  path <- nzs_project_fiscal_impacts(policy, inputs)
  testthat::expect_equal(path$ea_gross_saving_billion, rep(0, nrow(path)))
  testthat::expect_equal(path$income_test_saving_billion, rep(0, nrow(path)))
  testthat::expect_equal(path$indexation_saving_billion, rep(0, nrow(path)))
  testthat::expect_equal(path$ea_offset_billion, rep(0, nrow(path)))
  testthat::expect_equal(path$net_saving_billion, rep(0, nrow(path)))
})

testthat::test_that("fiscal summary discounts from selected base year", {
  path <- data.frame(
    year = c(2026, 2027),
    net_saving_billion = c(1, 1),
    ea_net_saving_billion = c(0, 0),
    income_test_saving_billion = c(1, 1),
    indexation_saving_billion = c(0, 0),
    ea_offset_billion = c(0, 0),
    status_quo_percent_gdp = c(0.05, 0.05),
    package_percent_gdp = c(0.04, 0.04)
  )

  out <- nzs_summarise_fiscal_impacts(path, discount_rate = 0.1, base_year = 2026)

  testthat::expect_equal(
    out$value[out$metric == "Net fiscal saving NPV"],
    1 + 1 / 1.1,
    tolerance = 1e-10
  )
})
