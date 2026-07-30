nzs_living_arrangements <- function() {
  c(
    "Single living alone" = 0.65,
    "Single sharing" = 0.60,
    "Couple both qualify (each)" = 0.50
  )
}

nzs_default_inputs <- function() {
  list(
    start_age = 65,
    current_couple_net_weekly = 854.08,
    base_gross_aotwe_weekly = 1681.06,
    base_standard_tax_thresholds_annual = c(0, 15600, 53500, 78100),
    base_standard_tax_rates = c(0.105, 0.175, 0.30, 0.33),
    base_acc_earners_levy_rate = 0.0175,
    base_acc_earnings_cap_annual = 156641,
    nzc_floor = 0.66,
    nzc_ceiling = 0.725,
    discount_rate = 0.043,
    npv_base_year = nzs_model_base_year()
  )
}

nzs_policy_regimes <- function() {
  c(
    "Current NZS formula" = "A",
    "CPI-only indexation" = "B",
    "Wage-indexed floor" = "C"
  )
}

nzs_policy_regime_label <- function(regime) {
  labels <- names(nzs_policy_regimes())
  values <- unname(nzs_policy_regimes())
  labels[match(regime, values)]
}

nzs_phase_in_periods <- function() {
  c(
    "Immediate" = 0,
    "2 years" = 2,
    "4 years" = 4,
    "8 years" = 8,
    "10 years" = 10
  )
}
