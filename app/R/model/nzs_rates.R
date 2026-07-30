nzs_base_net_aotwe_weekly <- function(defaults = nzs_default_inputs()) {
  # MSD's rate-setting method truncates the published QES weekly wage to whole dollars.
  gross_weekly <- floor(defaults$base_gross_aotwe_weekly)
  thresholds_weekly <- defaults$base_standard_tax_thresholds_annual / 52
  upper_bounds_weekly <- c(thresholds_weekly[-1], Inf)
  taxable_amounts <- pmax(
    0,
    pmin(gross_weekly, upper_bounds_weekly) - thresholds_weekly
  )
  standard_tax <- sum(taxable_amounts * defaults$base_standard_tax_rates)
  acc_levy <- min(
    gross_weekly,
    defaults$base_acc_earnings_cap_annual / 52
  ) * defaults$base_acc_earners_levy_rate
  deductions <- floor((standard_tax + acc_levy) * 100) / 100

  gross_weekly - deductions
}

nzs_rate_path <- function(economic_path = nzs_economic_path_from_ltfm(), defaults = nzs_default_inputs()) {
  path <- economic_path
  n <- nrow(path)

  net_aotwe_weekly <- numeric(n)
  couple_cpi <- numeric(n)
  couple_wage <- numeric(n)
  couple_current_law <- numeric(n)

  # The base net-AOTWE level uses the QES, M-tax and ACC construction; future values use LTFM wage growth.
  net_aotwe_weekly[1] <- nzs_base_net_aotwe_weekly(defaults)
  couple_cpi[1] <- defaults$current_couple_net_weekly
  couple_wage[1] <- defaults$nzc_floor * net_aotwe_weekly[1]
  couple_current_law[1] <- defaults$current_couple_net_weekly

  if (n > 1) {
    for (i in 2:n) {
      net_aotwe_weekly[i] <- net_aotwe_weekly[i - 1] * (1 + path$ltfm_wage_growth_projection[i])
      couple_cpi[i] <- couple_cpi[i - 1] * (1 + path$ltfm_cpi_growth_projection[i])
      couple_wage[i] <- defaults$nzc_floor * net_aotwe_weekly[i]
      couple_current_law[i] <- min(
        defaults$nzc_ceiling * net_aotwe_weekly[i],
        max(defaults$nzc_floor * net_aotwe_weekly[i], couple_current_law[i - 1] * (1 + path$ltfm_cpi_growth_projection[i]))
      )
    }
  }

  transform(
    path,
    net_aotwe_weekly = net_aotwe_weekly,
    couple_current_law = couple_current_law,
    couple_cpi = couple_cpi,
    couple_wage = couple_wage
  )
}

nzs_selected_regime <- function(rate_path, regime) {
  switch(
    regime,
    A = rate_path$couple_current_law,
    B = rate_path$couple_cpi,
    C = rate_path$couple_wage,
    stop("Unknown indexation regime: ", regime, call. = FALSE)
  )
}
