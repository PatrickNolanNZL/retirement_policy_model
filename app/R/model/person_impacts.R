nzs_kiwisaver_fund_returns <- function() {
  c(
    "Defensive" = 0.015,
    "Conservative" = 0.025,
    "Balanced" = 0.035,
    "Growth" = 0.045,
    "Aggressive" = 0.055
  )
}

nzs_statutory_kiwisaver_default_rate <- function(year) {
  ifelse(year >= 2028, 0.04, 0.035)
}

nzs_kiwisaver_rate_path <- function(rate, years, label) {
  if (length(rate) == 1L) {
    rate <- rep(rate, length(years))
  } else if (length(rate) != length(years)) {
    stop(label, " must be a single rate or one rate for each projection year.", call. = FALSE)
  }

  if (any(!is.finite(rate)) || any(rate < 0)) {
    stop(label, " must contain finite, non-negative values.", call. = FALSE)
  }

  as.numeric(rate)
}

nzs_esct_rate <- function(esct_income) {
  ifelse(
    esct_income <= 18720,
    0.105,
    ifelse(
      esct_income <= 64200,
      0.175,
      ifelse(esct_income <= 93720, 0.30, ifelse(esct_income <= 216000, 0.33, 0.39))
    )
  )
}

nzs_government_kiwisaver_contribution <- function(
    employee_contribution,
    annual_income,
    age,
    match_rate = 0.25,
    cap = 260.72,
    income_cap = 180000) {
  eligible <- age >= 16 & age < 65 & annual_income <= income_cap
  ifelse(eligible, pmin(match_rate * employee_contribution, cap), 0)
}

nzs_load_person_impacts_inputs <- function() {
  inputs <- list(
    ltfm = nzs_load_ltfm_projections(),
    earnings = utils::read.csv(nzs_app_data_file("kiwisaver-earnings-archetypes.csv"), check.names = FALSE),
    retirement_income = utils::read.csv(nzs_app_data_file("kiwisaver-retirement-income.csv"), check.names = FALSE),
    survival = utils::read.csv(nzs_app_data_file("kiwisaver-survival-total.csv"), check.names = FALSE)
  )

  nzs_validate_columns(inputs$earnings, c(
    "age",
    "lower_annual_linear",
    "median_annual_linear",
    "upper_annual_linear",
    "core_kiwisaver_age"
  ), "kiwisaver-earnings-archetypes.csv")
  nzs_validate_columns(inputs$retirement_income, c(
    "age",
    "retirement_income",
    "non_nzs_taxable_income_annual"
  ), "kiwisaver-retirement-income.csv")
  nzs_validate_columns(inputs$survival, c("age", "survival_probability"), "kiwisaver-survival-total.csv")
  nzs_validate_person_impacts_inputs(inputs)

  inputs
}

nzs_validate_person_impacts_inputs <- function(inputs) {
  ltfm <- inputs$ltfm
  earnings <- inputs$earnings
  retirement_income <- inputs$retirement_income
  survival <- inputs$survival

  required_earnings_ages <- 20:64
  required_retirement_ages <- 65:100
  required_survival_ages <- 65:105

  if (anyDuplicated(ltfm$year) || any(diff(ltfm$year) != 1) || any(!is.finite(as.matrix(ltfm[, c("ltfm_cpi_growth_projection", "ltfm_wage_growth_projection")]))) || min(ltfm$year) != nzs_model_base_year()) {
    stop("LTFM projections must be complete annual model-base-year paths.", call. = FALSE)
  }
  if (anyDuplicated(earnings$age) || !all(required_earnings_ages %in% earnings$age) || any(!is.finite(as.matrix(earnings[, c("lower_annual_linear", "median_annual_linear", "upper_annual_linear")]))) || any(earnings$median_annual_linear < 0)) {
    stop("Working-life earnings profiles are incomplete or invalid.", call. = FALSE)
  }
  retirement_key <- paste(retirement_income$age, retirement_income$retirement_income, sep = "|")
  expected_retirement_key <- as.vector(outer(required_retirement_ages, c("low", "medium", "high"), paste, sep = "|"))
  if (anyDuplicated(retirement_key) || !all(expected_retirement_key %in% retirement_key) || any(!is.finite(retirement_income$non_nzs_taxable_income_annual)) || any(retirement_income$non_nzs_taxable_income_annual < 0)) {
    stop("Retirement-income profiles are incomplete or invalid.", call. = FALSE)
  }
  if (anyDuplicated(survival$age) || !all(required_survival_ages %in% survival$age) || any(!is.finite(survival$survival_probability)) || any(survival$survival_probability < 0 | survival$survival_probability > 1) || survival$survival_probability[survival$age == 65] != 1 || any(diff(survival$survival_probability[order(survival$age)]) > 1e-12)) {
    stop("Total-population survival probabilities are incomplete or invalid.", call. = FALSE)
  }
  invisible(inputs)
}

nzs_extend_ltfm_projections <- function(projections, max_year) {
  projections <- projections[order(projections$year), ]
  if (max_year <= max(projections$year)) {
    return(projections[projections$year <= max_year, ])
  }

  last <- projections[nrow(projections), ]
  extra_years <- seq(max(projections$year) + 1, max_year)
  extra <- data.frame(
    year = extra_years,
    ltfm_cpi_growth_projection = last$ltfm_cpi_growth_projection,
    ltfm_wage_growth_projection = last$ltfm_wage_growth_projection
  )
  rbind(projections, extra)
}

nzs_adequacy_factors <- function(projections) {
  out <- projections[order(projections$year), ]
  out$cpi_factor <- cumprod(1 + out$ltfm_cpi_growth_projection)
  out$wage_factor <- cumprod(1 + out$ltfm_wage_growth_projection)
  out
}

nzs_earnings_column <- function(archetype) {
  switch(
    archetype,
    low = "lower_annual_linear",
    median = "median_annual_linear",
    high = "upper_annual_linear",
    stop("Unknown earnings archetype: ", archetype, call. = FALSE)
  )
}

nzs_earnings_archetype_label <- function(archetype) {
  switch(
    archetype,
    low = "Low",
    median = "Median",
    high = "High",
    stop("Unknown earnings archetype: ", archetype, call. = FALSE)
  )
}

nzs_retirement_income_archetype_label <- function(archetype) {
  switch(
    archetype,
    low = "Low",
    medium = "Medium",
    high = "High",
    stop("Unknown retirement-income archetype: ", archetype, call. = FALSE)
  )
}

nzs_core_kiwisaver_ages <- function(earnings) {
  if ("core_kiwisaver_age" %in% names(earnings)) {
    core <- earnings$core_kiwisaver_age
    if (is.logical(core)) {
      return(core)
    }
    return(tolower(as.character(core)) == "true")
  }
  earnings$age >= 20 & earnings$age <= 64
}

nzs_person_archetype_snapshot <- function(
    inputs,
    current_age,
    earnings_archetype,
    retirement_income_archetype) {
  earnings_col <- nzs_earnings_column(earnings_archetype)
  earnings_row <- inputs$earnings[inputs$earnings$age == current_age, ]
  if (nrow(earnings_row) == 0) {
    stop("Missing earnings row for age: ", current_age, call. = FALSE)
  }

  data.frame(
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    earnings_archetype_label = nzs_earnings_archetype_label(earnings_archetype),
    current_age_earnings_real = earnings_row[[earnings_col]],
    retirement_income_archetype = retirement_income_archetype,
    retirement_income_archetype_label = nzs_retirement_income_archetype_label(retirement_income_archetype),
    age65_retirement_income_real = nzs_retirement_income_for_age(inputs, 65, retirement_income_archetype),
    stringsAsFactors = FALSE
  )
}

nzs_person_archetype_guide_data <- function(inputs) {
  earnings_map <- data.frame(
    archetype = c("low", "median", "high"),
    retirement_income = c("low", "medium", "high"),
    label = c("Low", "Median / Medium", "High"),
    stringsAsFactors = FALSE
  )
  core_rows <- nzs_core_kiwisaver_ages(inputs$earnings)

  rows <- vector("list", nrow(earnings_map))
  for (i in seq_len(nrow(earnings_map))) {
    earnings_col <- nzs_earnings_column(earnings_map$archetype[[i]])
    values <- inputs$earnings[core_rows, c("age", earnings_col)]
    names(values) <- c("age", "earnings")
    peak <- values[which.max(values$earnings), ]
    age64 <- values$earnings[values$age == 64]
    retirement_rows <- inputs$retirement_income[
      inputs$retirement_income$retirement_income == earnings_map$retirement_income[[i]],
    ]

    rows[[i]] <- data.frame(
      archetype = earnings_map$label[[i]],
      peak_age = peak$age,
      peak_earnings_real = peak$earnings,
      age64_earnings_real = age64,
      average_retirement_income_65_69_real = mean(retirement_rows$non_nzs_taxable_income_annual[
        retirement_rows$age >= 65 & retirement_rows$age <= 69
      ]),
      age75_plus_retirement_income_real = nzs_retirement_income_for_age(
        inputs,
        75,
        earnings_map$retirement_income[[i]]
      ),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

nzs_project_kiwisaver_contributions <- function(
    current_age,
    earnings_archetype,
    employee_rate,
    employer_rate,
    accumulation_return,
    inputs,
    initial_balance = 0,
    base_year = nzs_model_base_year(),
    retirement_age = 65) {
  if (current_age >= retirement_age) {
    return(data.frame())
  }

  ages <- current_age:(retirement_age - 1)
  years <- base_year + ages - current_age
  employee_rates <- nzs_kiwisaver_rate_path(employee_rate, years, "employee_rate")
  employer_rates <- nzs_kiwisaver_rate_path(employer_rate, years, "employer_rate")
  macro <- nzs_adequacy_factors(nzs_extend_ltfm_projections(inputs$ltfm, max(years)))
  earnings <- inputs$earnings
  earnings_col <- nzs_earnings_column(earnings_archetype)

  rows <- vector("list", length(ages))
  balance <- initial_balance
  for (i in seq_along(ages)) {
    age <- ages[[i]]
    year <- years[[i]]
    macro_row <- macro[macro$year == year, ]
    base_earnings <- earnings[[earnings_col]][match(age, earnings$age)]
    annual_earnings <- base_earnings * macro_row$cpi_factor

    employee <- employee_rates[[i]] * annual_earnings
    employer_gross <- employer_rates[[i]] * annual_earnings
    esct_rate <- nzs_esct_rate(annual_earnings + employer_gross)
    employer_net <- employer_gross * (1 - esct_rate)
    government <- nzs_government_kiwisaver_contribution(employee, annual_earnings, age)

    opening_balance <- balance
    investment_return <- opening_balance * accumulation_return
    total_contribution <- employee + employer_net + government
    balance <- opening_balance + investment_return + total_contribution

    rows[[i]] <- data.frame(
      year = year,
      age = age,
      annual_earnings = annual_earnings,
      employee_rate = employee_rates[[i]],
      employer_rate = employer_rates[[i]],
      employee_contribution = employee,
      employer_gross_contribution = employer_gross,
      esct_rate = esct_rate,
      employer_net_contribution = employer_net,
      government_contribution = government,
      opening_balance = opening_balance,
      investment_return = investment_return,
      total_contribution = total_contribution,
      closing_balance = balance,
      cpi_factor = macro_row$cpi_factor,
      real_closing_balance = balance / macro_row$cpi_factor
    )
  }

  do.call(rbind, rows)
}

nzs_kiwisaver_balance_at_65 <- function(
    current_age,
    earnings_archetype,
    employee_rate,
    employer_rate,
    accumulation_return,
    inputs,
    initial_balance = 0,
    base_year = nzs_model_base_year(),
    retirement_age = 65) {
  if (current_age >= retirement_age) {
    return(initial_balance)
  }

  ages <- current_age:(retirement_age - 1)
  years <- base_year + ages - current_age
  employee_rates <- nzs_kiwisaver_rate_path(employee_rate, years, "employee_rate")
  employer_rates <- nzs_kiwisaver_rate_path(employer_rate, years, "employer_rate")
  macro <- nzs_adequacy_factors(nzs_extend_ltfm_projections(inputs$ltfm, max(years)))
  cpi_factors <- macro$cpi_factor[match(years, macro$year)]
  earnings_col <- nzs_earnings_column(earnings_archetype)
  base_earnings <- inputs$earnings[[earnings_col]][match(ages, inputs$earnings$age)]

  annual_earnings <- base_earnings * cpi_factors
  employee <- employee_rates * annual_earnings
  employer_gross <- employer_rates * annual_earnings
  esct_rate <- nzs_esct_rate(annual_earnings + employer_gross)
  employer_net <- employer_gross * (1 - esct_rate)
  government <- nzs_government_kiwisaver_contribution(employee, annual_earnings, ages)
  total_contribution <- employee + employer_net + government

  balance <- initial_balance
  for (contribution in total_contribution) {
    balance <- balance * (1 + accumulation_return) + contribution
  }
  balance
}

nzs_adequacy_person_protected <- function(current_age, start_year, base_year = nzs_model_base_year()) {
  current_age + (start_year - base_year) >= 65
}

nzs_adequacy_component_active <- function(year, current_age, start_year, grandparenting, base_year = nzs_model_base_year()) {
  year >= start_year && !(isTRUE(grandparenting) && nzs_adequacy_person_protected(current_age, start_year, base_year))
}

nzs_retirement_income_for_age <- function(inputs, age, retirement_income_archetype = "medium") {
  rows <- inputs$retirement_income[
    inputs$retirement_income$retirement_income == retirement_income_archetype,
  ]
  if (nrow(rows) == 0) {
    stop("Missing retirement-income rows for archetype: ", retirement_income_archetype, call. = FALSE)
  }
  rows$non_nzs_taxable_income_annual[match(pmin(age, max(rows$age)), rows$age)]
}

nzs_project_adequacy_nzs_losses <- function(
    fiscal_policy,
    current_age,
    earnings_archetype,
    living_arrangement,
    retirement_income_archetype = "medium",
    inputs,
    base_year = nzs_model_base_year()) {
  if (current_age > 64) {
    stop("Person impacts current_age must be 64 or younger.", call. = FALSE)
  }

  max_age <- max(inputs$survival$age, na.rm = TRUE)
  max_year <- base_year + max_age - current_age
  macro <- nzs_adequacy_factors(nzs_extend_ltfm_projections(inputs$ltfm, max_year))
  rate_path <- nzs_rate_path(
    nzs_economic_path_from_ltfm(macro[, c("year", "ltfm_cpi_growth_projection", "ltfm_wage_growth_projection")])
  )
  living_factors <- nzs_living_arrangements()
  rate_factor <- unname(living_factors[[living_arrangement]])

  ages <- 65:max_age
  rows <- vector("list", length(ages))
  for (i in seq_along(ages)) {
    age <- ages[[i]]
    year <- base_year + age - current_age
    rate_row <- rate_path[rate_path$year == year, ]
    macro_row <- macro[macro$year == year, ]
    survival <- inputs$survival$survival_probability[match(age, inputs$survival$age)]

    status_quo <- rate_row$couple_current_law * rate_factor * 52
    ea_active <- nzs_adequacy_component_active(
      year,
      current_age,
      fiscal_policy$eligibility_age_start_year,
      fiscal_policy$eligibility_age_grandparenting,
      base_year
    )
    effective_age <- if (isTRUE(fiscal_policy$eligibility_age_active) && ea_active) {
      nzs_effective_eligibility_age(year, fiscal_policy)
    } else {
      65
    }
    ea_excluded <- age < effective_age
    ea_loss <- if (ea_excluded) status_quo else 0

    index_active <- nzs_adequacy_component_active(
      year,
      current_age,
      fiscal_policy$indexation_start_year,
      fiscal_policy$indexation_grandparenting,
      base_year
    ) && !identical(fiscal_policy$indexation_regime, "A")
    selected_weekly <- if (index_active) {
      nzs_selected_regime(rate_row, fiscal_policy$indexation_regime)
    } else {
      rate_row$couple_current_law
    }
    pre_test_reform <- if (ea_excluded) 0 else selected_weekly * rate_factor * 52
    indexation_loss <- if (ea_excluded) 0 else status_quo - pre_test_reform

    income_test_active <- nzs_adequacy_component_active(
      year,
      current_age,
      fiscal_policy$income_test_start_year,
      fiscal_policy$income_test_grandparenting,
      base_year
    ) &&
      isTRUE(fiscal_policy$income_test_active) &&
      age >= fiscal_policy$income_test_from_age &&
      age < fiscal_policy$income_test_to_age &&
      !ea_excluded

    retirement_income <- nzs_retirement_income_for_age(inputs, age, retirement_income_archetype) * macro_row$cpi_factor
    threshold <- fiscal_policy$income_test_threshold * macro_row$cpi_factor
    abatement <- if (income_test_active) {
      min(pre_test_reform, fiscal_policy$income_test_abatement_rate * max(0, retirement_income - threshold))
    } else {
      0
    }
    reform <- pre_test_reform - abatement
    total_loss <- status_quo - reform

    rows[[i]] <- data.frame(
      year = year,
      age = age,
      survival_probability = survival,
      cpi_factor = macro_row$cpi_factor,
      status_quo_annual = status_quo,
      reform_annual = reform,
      retirement_income = retirement_income,
      eligibility_age_effective = effective_age,
      ea_loss = ea_loss,
      indexation_loss = indexation_loss,
      income_test_loss = abatement,
      total_loss = total_loss,
      survival_weighted_loss = total_loss * survival,
      real_total_loss = total_loss / macro_row$cpi_factor,
      real_survival_weighted_loss = total_loss * survival / macro_row$cpi_factor,
      real_ea_loss = ea_loss * survival / macro_row$cpi_factor,
      real_indexation_loss = indexation_loss * survival / macro_row$cpi_factor,
      real_income_test_loss = abatement * survival / macro_row$cpi_factor
    )
  }

  do.call(rbind, rows)
}

nzs_kiwisaver_target_balance_from_losses <- function(loss_path, drawdown_return = 0.025) {
  periods <- loss_path$age - 65
  sum(loss_path$survival_weighted_loss / (1 + drawdown_return) ^ periods, na.rm = TRUE)
}

nzs_solve_matched_kiwisaver_rate <- function(
    target_balance,
    current_age,
    earnings_archetype,
    accumulation_return,
    inputs,
    initial_balance = 0,
    max_rate = 1) {
  if (target_balance <= 0 || current_age >= 65) {
    return(0)
  }

  balance_at_rate <- function(rate) {
    nzs_kiwisaver_balance_at_65(
      current_age = current_age,
      earnings_archetype = earnings_archetype,
      employee_rate = rate,
      employer_rate = rate,
      accumulation_return = accumulation_return,
      inputs = inputs,
      initial_balance = initial_balance
    )
  }

  if (balance_at_rate(0) >= target_balance) {
    return(0)
  }
  if (balance_at_rate(max_rate) < target_balance) {
    return(NA_real_)
  }

  stats::uniroot(function(rate) balance_at_rate(rate) - target_balance, interval = c(0, max_rate))$root
}

nzs_kiwisaver_working_life_profile <- function(
    current_age,
    earnings_archetype,
    employee_rate,
    employer_rate,
    inputs,
    base_year = nzs_model_base_year()) {
  core_ages <- inputs$earnings$core_kiwisaver_age
  if (!is.logical(core_ages)) {
    core_ages <- tolower(core_ages) == "true"
  }
  ages <- inputs$earnings$age[core_ages]
  earnings_col <- nzs_earnings_column(earnings_archetype)
  rows <- lapply(ages, function(age) {
    base_earnings <- inputs$earnings[[earnings_col]][match(age, inputs$earnings$age)]
    employee <- employee_rate * base_earnings
    employer_gross <- employer_rate * base_earnings
    esct_rate <- nzs_esct_rate(base_earnings + employer_gross)
    data.frame(
      age = age,
      year = base_year + age - current_age,
      active = age >= current_age,
      real_annual_earnings = base_earnings,
      real_employee_contribution = employee,
      real_employer_net_contribution = employer_gross * (1 - esct_rate)
    )
  })
  profile <- do.call(rbind, rows)
  active_contributions <- nzs_project_kiwisaver_contributions(
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    employee_rate = employee_rate,
    employer_rate = employer_rate,
    accumulation_return = 0,
    inputs = inputs
  )
  for (i in seq_len(nrow(active_contributions))) {
    age <- active_contributions$age[[i]]
    row <- profile$age == age
    profile$real_annual_earnings[row] <- active_contributions$annual_earnings[[i]] / active_contributions$cpi_factor[[i]]
    profile$real_employee_contribution[row] <- active_contributions$employee_contribution[[i]] / active_contributions$cpi_factor[[i]]
    profile$real_employer_net_contribution[row] <- active_contributions$employer_net_contribution[[i]] / active_contributions$cpi_factor[[i]]
  }
  profile
}

nzs_kiwisaver_balance_profile <- function(contributions, current_age, current_balance = 0) {
  start <- data.frame(
    age = current_age,
    real_balance = current_balance,
    point = "current_balance"
  )
  if (nrow(contributions) == 0) {
    return(start)
  }
  path <- data.frame(
    age = contributions$age + 1,
    real_balance = contributions$real_closing_balance,
    point = "year_end"
  )
  rbind(start, path)
}

nzs_project_person_impacts <- function(
    fiscal_policy,
    current_age,
    earnings_archetype,
    living_arrangement,
    retirement_income_archetype = "medium",
    employee_rate,
    employer_rate,
    accumulation_return,
    drawdown_return = 0.025,
    current_balance = 0,
    inputs = nzs_load_person_impacts_inputs()) {
  losses <- nzs_project_adequacy_nzs_losses(
    fiscal_policy = fiscal_policy,
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    retirement_income_archetype = retirement_income_archetype,
    living_arrangement = living_arrangement,
    inputs = inputs
  )
  target_balance <- nzs_kiwisaver_target_balance_from_losses(losses, drawdown_return)
  contributions <- nzs_project_kiwisaver_contributions(
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    employee_rate = employee_rate,
    employer_rate = employer_rate,
    accumulation_return = accumulation_return,
    inputs = inputs,
    initial_balance = current_balance
  )
  projected_balance <- if (nrow(contributions) == 0) 0 else contributions$closing_balance[nrow(contributions)]
  status_quo_years <- 2026 + current_age:64 - current_age
  status_quo_rates <- nzs_statutory_kiwisaver_default_rate(status_quo_years)
  status_quo_contributions <- nzs_project_kiwisaver_contributions(
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    employee_rate = status_quo_rates,
    employer_rate = status_quo_rates,
    accumulation_return = accumulation_return,
    inputs = inputs,
    initial_balance = current_balance
  )
  status_quo_projected_balance <- if (nrow(status_quo_contributions) == 0) {
    current_balance
  } else {
    status_quo_contributions$closing_balance[nrow(status_quo_contributions)]
  }
  retirement_year <- 2026 + 65 - current_age
  cpi_path <- nzs_adequacy_factors(nzs_extend_ltfm_projections(inputs$ltfm, retirement_year))
  retirement_cpi_factor <- cpi_path$cpi_factor[cpi_path$year == retirement_year]
  target_balance_real <- target_balance / retirement_cpi_factor
  projected_balance_real <- projected_balance / retirement_cpi_factor
  required_matched_rate <- nzs_solve_matched_kiwisaver_rate(
    target_balance = target_balance,
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    accumulation_return = accumulation_return,
    inputs = inputs,
    initial_balance = current_balance
  )
  working_life <- nzs_kiwisaver_working_life_profile(
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    employee_rate = employee_rate,
    employer_rate = employer_rate,
    inputs = inputs
  )
  balance_profile <- nzs_kiwisaver_balance_profile(contributions, current_age, current_balance)
  retirement_income <- nzs_kiwisaver_retirement_income_profile(
    losses = losses,
    projected_balance = projected_balance,
    status_quo_projected_balance = status_quo_projected_balance,
    drawdown_return = drawdown_return
  )
  replacement_rates <- nzs_kiwisaver_replacement_rates(
    retirement_income = retirement_income,
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    inputs = inputs
  )

  list(
    losses = losses,
    contributions = contributions,
    working_life = working_life,
    balance_profile = balance_profile,
    retirement_income = retirement_income,
    replacement_rates = replacement_rates,
    target_balance = target_balance,
    target_balance_real = target_balance_real,
    projected_balance = projected_balance,
    projected_balance_real = projected_balance_real,
    status_quo_projected_balance = status_quo_projected_balance,
    shortfall = target_balance - projected_balance,
    shortfall_real = target_balance_real - projected_balance_real,
    retirement_cpi_factor = retirement_cpi_factor,
    required_matched_rate = required_matched_rate
  )
}

nzs_person_impacts_fund_sensitivity <- function(
    fiscal_policy,
    current_age,
    earnings_archetype,
    living_arrangement,
    retirement_income_archetype = "medium",
    drawdown_return = 0.025,
    current_balance = 0,
    inputs = nzs_load_person_impacts_inputs()) {
  returns <- nzs_kiwisaver_fund_returns()
  losses <- nzs_project_adequacy_nzs_losses(
    fiscal_policy = fiscal_policy,
    current_age = current_age,
    earnings_archetype = earnings_archetype,
    retirement_income_archetype = retirement_income_archetype,
    living_arrangement = living_arrangement,
    inputs = inputs
  )
  target_balance <- nzs_kiwisaver_target_balance_from_losses(losses, drawdown_return)
  rows <- lapply(names(returns), function(label) {
    required_matched_rate <- nzs_solve_matched_kiwisaver_rate(
      target_balance = target_balance,
      current_age = current_age,
      earnings_archetype = earnings_archetype,
      accumulation_return = unname(returns[[label]]),
      inputs = inputs,
      initial_balance = current_balance
    )
    data.frame(
      fund_type = label,
      accumulation_return = unname(returns[[label]]),
      required_matched_rate = required_matched_rate,
      target_balance = target_balance
    )
  })
  do.call(rbind, rows)
}

nzs_constant_real_kiwisaver_drawdown <- function(loss_path, projected_balance, drawdown_return = 0.025) {
  if (projected_balance <= 0) {
    return(0)
  }
  periods <- loss_path$age - 65
  annuity_factor <- sum(
    loss_path$cpi_factor * loss_path$survival_probability / (1 + drawdown_return) ^ periods,
    na.rm = TRUE
  )
  if (annuity_factor <= 0) {
    return(0)
  }
  projected_balance / annuity_factor
}

nzs_kiwisaver_retirement_income_profile <- function(
    losses,
    projected_balance,
    status_quo_projected_balance = projected_balance,
    drawdown_return = 0.025) {
  drawdown_real <- nzs_constant_real_kiwisaver_drawdown(losses, projected_balance, drawdown_return)
  status_quo_drawdown_real <- nzs_constant_real_kiwisaver_drawdown(
    losses,
    status_quo_projected_balance,
    drawdown_return
  )
  out <- data.frame(
    age = losses$age,
    year = losses$year,
    non_nzs_income_real = losses$retirement_income / losses$cpi_factor,
    status_quo_nzs_real = losses$status_quo_annual / losses$cpi_factor,
    reform_nzs_real = losses$reform_annual / losses$cpi_factor,
    kiwisaver_drawdown_real = drawdown_real,
    status_quo_kiwisaver_drawdown_real = status_quo_drawdown_real
  )
  out$status_quo_total_real <- out$non_nzs_income_real + out$status_quo_nzs_real + out$status_quo_kiwisaver_drawdown_real
  out$reform_total_real <- out$non_nzs_income_real + out$reform_nzs_real + out$kiwisaver_drawdown_real
  out
}

nzs_real_age64_earnings <- function(current_age, earnings_archetype, inputs, base_year = nzs_model_base_year()) {
  earnings_col <- nzs_earnings_column(earnings_archetype)
  inputs$earnings[[earnings_col]][match(64, inputs$earnings$age)]
}

nzs_kiwisaver_replacement_rates <- function(
    retirement_income,
    current_age,
    earnings_archetype,
    inputs,
    window = c(65, 69)) {
  denominator <- nzs_real_age64_earnings(current_age, earnings_archetype, inputs)
  rows <- retirement_income[retirement_income$age >= window[[1]] & retirement_income$age <= window[[2]], ]
  data.frame(
    metric = c("Status quo", "Reform"),
    replacement_rate = c(
      mean(rows$status_quo_total_real, na.rm = TRUE) / denominator,
      mean(rows$reform_total_real, na.rm = TRUE) / denominator
    ),
    age64_earnings_real = denominator,
    window_start_age = window[[1]],
    window_end_age = window[[2]]
  )
}

nzs_kiwisaver_required_rate_by_age <- function(
    fiscal_policy,
    earnings_archetype,
    living_arrangement,
    retirement_income_archetype = "medium",
    drawdown_return = 0.025,
    inputs = nzs_load_person_impacts_inputs(),
    ages = 20:64) {
  returns <- nzs_kiwisaver_fund_returns()
  rows <- list()
  index <- 1
  for (age in ages) {
    for (label in names(returns)) {
      result <- nzs_project_person_impacts(
        fiscal_policy = fiscal_policy,
        current_age = age,
        earnings_archetype = earnings_archetype,
        retirement_income_archetype = retirement_income_archetype,
        living_arrangement = living_arrangement,
        employee_rate = 0,
        employer_rate = 0,
        accumulation_return = unname(returns[[label]]),
        drawdown_return = drawdown_return,
        current_balance = 0,
        inputs = inputs
      )
      rows[[index]] <- data.frame(
        current_age = age,
        fund_type = label,
        accumulation_return = unname(returns[[label]]),
        required_matched_rate = result$required_matched_rate
      )
      index <- index + 1
    }
  }
  do.call(rbind, rows)
}
