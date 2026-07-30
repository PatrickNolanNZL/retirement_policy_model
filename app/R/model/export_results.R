nzs_export_git_sha <- function() {
  root <- nzs_project_root()
  if (is.null(root)) {
    return(NA_character_)
  }
  out <- tryCatch(
    suppressWarnings(
      system2(
        "git",
        c("rev-parse", "--short", "HEAD"),
        stdout = TRUE,
        stderr = FALSE
      )
    ),
    error = function(e) character()
  )
  if (length(out) == 0 || !nzchar(out[[1]])) {
    return(NA_character_)
  }
  out[[1]]
}

nzs_export_metadata <- function(exported_at = Sys.time(), git_sha = nzs_export_git_sha()) {
  data.frame(
    field = c("Model", "Model version", "Exported at", "Model status"),
    value = c(
      "NZ retirement policy model",
      paste0("v", nzs_model_version()),
      format(exported_at, "%Y-%m-%d %H:%M:%S %Z"),
      "Scenario model prototype"
    ),
    stringsAsFactors = FALSE
  )
}
nzs_policy_settings_table <- function(policy, person_settings = list(), exported_at = Sys.time()) {
  base_settings <- data.frame(
    section = c(
      rep("Eligibility age", 5),
      rep("Indexation", 3),
      rep("Income testing", 7),
      rep("Other settings", 2)
    ),
    setting = c(
      "Active",
      "Start year",
      "New eligibility age",
      "Phase-in period (years)",
      "Grandparent existing recipients",
      "Regime",
      "Start year",
      "Grandparent existing recipients",
      "Active",
      "Start year",
      "From age",
      "To age",
      "Threshold ($ nominal)",
      "Income-test threshold indexation",
      "Abatement rate",
      "Discount rate (nominal)",
      "NPV base year"
    ),
    value = c(
      policy$eligibility_age_active,
      policy$eligibility_age_start_year,
      policy$eligibility_age_new_age,
      policy$eligibility_age_phase_in_years %||% NA_real_,
      policy$eligibility_age_grandparenting,
      policy$indexation_regime,
      policy$indexation_start_year,
      policy$indexation_grandparenting,
      policy$income_test_active,
      policy$income_test_start_year,
      policy$income_test_from_age,
      policy$income_test_to_age,
      policy$income_test_threshold,
      "CPI-indexed (model convention)",
      policy$income_test_abatement_rate,
      policy$discount_rate,
      policy$npv_base_year
    ),
    stringsAsFactors = FALSE
  )

  if (length(person_settings) > 0) {
    person <- data.frame(
      section = "Person impacts",
      setting = names(person_settings),
      value = unlist(person_settings, use.names = FALSE),
      stringsAsFactors = FALSE
    )
    base_settings <- rbind(base_settings, person)
  }

  rbind(
    data.frame(
      section = "Export",
      setting = "Exported at",
      value = format(exported_at, "%Y-%m-%d %H:%M:%S %Z"),
      stringsAsFactors = FALSE
    ),
    base_settings
  )
}

nzs_export_fiscal_headline <- function(fiscal_summary) {
  data.frame(
    metric = fiscal_summary$metric,
    value = fiscal_summary$value,
    stringsAsFactors = FALSE
  )
}

nzs_export_fiscal_path <- function(fiscal_path) {
  data.frame(
    year = fiscal_path$year,
    `status quo NZS cost ($bn nominal)` = fiscal_path$status_quo_cost_billion,
    `reform NZS cost ($bn nominal)` = fiscal_path$package_cost_billion,
    `nominal GDP ($bn)` = fiscal_path$nominal_gdp_billion,
    `status quo NZS cost (% GDP)` = fiscal_path$status_quo_percent_gdp,
    `reform NZS cost (% GDP)` = fiscal_path$package_percent_gdp,
    check.names = FALSE
  )
}

nzs_export_fiscal_decomposition <- function(fiscal_path) {
  data.frame(
    year = fiscal_path$year,
    `eligibility age gross saving ($bn nominal)` = fiscal_path$ea_gross_saving_billion,
    `benefit takeup offset ($bn nominal)` = fiscal_path$ea_offset_billion,
    `eligibility age net saving ($bn nominal)` = fiscal_path$ea_net_saving_billion,
    `income-test saving ($bn nominal)` = fiscal_path$income_test_saving_billion,
    `indexation saving ($bn nominal)` = fiscal_path$indexation_saving_billion,
    `net saving ($bn nominal)` = fiscal_path$net_saving_billion,
    check.names = FALSE
  )
}

nzs_export_person_summary <- function(person_impacts) {
  data.frame(
    metric = c(
      "KiwiSaver balance at 65",
      "Age-65 balance to offset NZS reform",
      "KiwiSaver surplus / shortfall",
      "Required matched KiwiSaver contribution rate"
    ),
    value = c(
      person_impacts$projected_balance_real,
      person_impacts$target_balance_real,
      -1 * person_impacts$shortfall_real,
      person_impacts$required_matched_rate
    ),
    units = c("$ real 2026", "$ real 2026", "$ real 2026", "%"),
    stringsAsFactors = FALSE
  )
}

nzs_export_person_contributions <- function(contributions, working_life) {
  active <- working_life
  data.frame(
    age = active$age,
    year = active$year,
    active = active$active,
    `annual earnings ($ real 2026)` = active$real_annual_earnings,
    `employee contribution ($ real 2026)` = active$real_employee_contribution,
    `net employer contribution ($ real 2026)` = active$real_employer_net_contribution,
    check.names = FALSE
  )
}

nzs_export_person_balance <- function(balance_profile) {
  data.frame(
    age = balance_profile$age,
    `KiwiSaver balance ($ real 2026)` = balance_profile$real_balance,
    point = balance_profile$point,
    check.names = FALSE
  )
}

nzs_export_person_retirement_income <- function(retirement_income) {
  data.frame(
    age = retirement_income$age,
    year = retirement_income$year,
    `non-NZS income ($ real 2026)` = retirement_income$non_nzs_income_real,
    `status quo NZS ($ real 2026)` = retirement_income$status_quo_nzs_real,
    `reform NZS ($ real 2026)` = retirement_income$reform_nzs_real,
    `KiwiSaver drawdown ($ real 2026)` = retirement_income$kiwisaver_drawdown_real,
    `status quo total ($ real 2026)` = retirement_income$status_quo_total_real,
    `reform total ($ real 2026)` = retirement_income$reform_total_real,
    check.names = FALSE
  )
}

nzs_export_person_replacement <- function(replacement_rates) {
  data.frame(
    scenario = replacement_rates$metric,
    `replacement rate (%)` = replacement_rates$replacement_rate,
    `age-64 earnings ($ real 2026)` = replacement_rates$age64_earnings_real,
    `window start age` = replacement_rates$window_start_age,
    `window end age` = replacement_rates$window_end_age,
    check.names = FALSE
  )
}

nzs_export_person_sensitivity <- function(sensitivity) {
  data.frame(
    `fund type` = sensitivity$fund_type,
    `return (nominal %)` = sensitivity$accumulation_return,
    `required matched rate (%)` = sensitivity$required_matched_rate,
    `target balance ($ nominal)` = sensitivity$target_balance,
    check.names = FALSE
  )
}

nzs_export_cohort_transition <- function(cohort_transition) {
  data.frame(
    birth_year = cohort_transition$birth_year,
    current_age = cohort_transition$current_age,
    `PV status quo NZS ($ real 2026)` = cohort_transition$pv_status_quo,
    `PV loss ($ real 2026)` = cohort_transition$pv_loss,
    `lifetime loss under reform (%)` = cohort_transition$pct_loss,
    status = cohort_transition$status,
    protected = cohort_transition$protected,
    phase_in_cohort = cohort_transition$phase_in_cohort,
    ea_loss_any = cohort_transition$ea_loss_any,
    indexation_loss_any = cohort_transition$indexation_loss_any,
    check.names = FALSE
  )
}

nzs_export_cohort_deferral <- function(cohort_deferral) {
  data.frame(
    current_age = cohort_deferral$current_age,
    birth_year = cohort_deferral$birth_year,
    year_at_65 = cohort_deferral$year_at_65,
    effective_eligibility_age = cohort_deferral$effective_eligibility_age,
    years_deferred = cohort_deferral$years_deferred,
    population = cohort_deferral$population,
    affected_population = cohort_deferral$affected_population,
    deferred_person_years = cohort_deferral$deferred_person_years,
    protected = cohort_deferral$protected,
    check.names = FALSE
  )
}

nzs_export_cohort_income_test_incidence <- function(incidence) {
  data.frame(
    income_band = incidence$display_bin,
    income_lower = incidence$income_lower,
    income_upper = incidence$income_upper,
    estimated_people = incidence$estimated_people,
    estimated_people_affected = incidence$estimated_people_affected,
    `share of 65+ population affected (%)` = incidence$affected_population_share,
    `share affected within band (%)` = incidence$share_affected,
    `average annual reduction affected ($ nominal)` = incidence$average_annual_reduction_affected,
    `average weekly reduction affected ($ nominal)` = incidence$average_weekly_reduction_affected,
    `average NZS loss relative to status quo (%)` = incidence$average_percent_loss_affected,
    check.names = FALSE
  )
}

nzs_build_export_results <- function(
    policy,
    fiscal_path,
    fiscal_summary,
    cohort_transition,
    cohort_deferral,
    cohort_income_test_incidence,
    person_impacts,
    person_fund_sensitivity,
    person_settings = list(),
    exported_at = Sys.time()) {
  list(
    metadata = nzs_export_metadata(exported_at = exported_at),
    readme = data.frame(
      note = c(
        "This workbook contains model outputs for the app settings selected at the time of export.",
        "The app is a scenario model prototype. Outputs are scenario-based model results, not forecasts.",
        "Dollar values are labelled as nominal or real 2026 dollars in the relevant table headers.",
        "Fiscal income-test savings use public-source-derived non-NZS taxable income distributions.",
        "The Person impacts status quo comparison uses the enacted KiwiSaver default contribution schedule: 3.5% employee and employer contributions in 2026-27, rising to 4.0% from 2028.",
        "NZ Super rate paths retain the published 2026 payment anchor, calculate base-year net AOTWE from QES earnings, standard tax and ACC, and use the LTFM wage and CPI paths thereafter; future tax and ACC settings are not projected."
      ),
      stringsAsFactors = FALSE
    ),
    settings = nzs_policy_settings_table(policy, person_settings, exported_at),
    fiscal = list(
      headline_summary = nzs_export_fiscal_headline(fiscal_summary),
      annual_fiscal_path = nzs_export_fiscal_path(fiscal_path),
      annual_saving_decomposition = nzs_export_fiscal_decomposition(fiscal_path)
    ),
    cohort = list(
      transition_impact_by_cohort = nzs_export_cohort_transition(cohort_transition),
      ea_deferral_by_cohort = nzs_export_cohort_deferral(cohort_deferral),
      income_test_incidence = nzs_export_cohort_income_test_incidence(cohort_income_test_incidence)
    ),
    person = list(
      offset_summary = nzs_export_person_summary(person_impacts),
      working_life_earnings_and_contributions = nzs_export_person_contributions(
        person_impacts$contributions,
        person_impacts$working_life
      ),
      kiwisaver_balance_path = nzs_export_person_balance(person_impacts$balance_profile),
      retirement_income_composition = nzs_export_person_retirement_income(person_impacts$retirement_income),
      replacement_rate = nzs_export_person_replacement(person_impacts$replacement_rates),
      fund_sensitivity = nzs_export_person_sensitivity(person_fund_sensitivity)
    )
  )
}
