nzs_default_fiscal_policy <- function() {
  list(
    indexation_regime = "A",
    indexation_start_year = 2027,
    indexation_grandparenting = FALSE,
    eligibility_age_active = TRUE,
    eligibility_age_start_year = 2027,
    eligibility_age_new_age = 67,
    eligibility_age_phase_in_years = 2,
    eligibility_age_phase_in_months_per_year = 12,
    eligibility_age_grandparenting = FALSE,
    income_test_active = TRUE,
    income_test_start_year = 2027,
    income_test_from_age = 65,
    income_test_to_age = 70,
    income_test_threshold = 10000,
    income_test_abatement_rate = 0.25,
    income_test_grandparenting = FALSE,
    discount_rate = 0.043,
    npv_base_year = nzs_model_base_year()
  )
}

nzs_validate_fiscal_policy <- function(policy) {
  required <- names(nzs_default_fiscal_policy())
  missing <- setdiff(required, names(policy))
  if (length(missing) > 0) {
    stop("Fiscal policy is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  scalar_number <- function(name, lower = -Inf, upper = Inf, integer = FALSE) {
    value <- policy[[name]]
    valid <- length(value) == 1L && is.finite(value) && value >= lower && value <= upper
    if (integer) valid <- valid && value == as.integer(value)
    if (!valid) {
      stop("Fiscal policy field '", name, "' is invalid.", call. = FALSE)
    }
  }
  scalar_flag <- function(name) {
    value <- policy[[name]]
    if (length(value) != 1L || is.na(value) || !is.logical(value)) {
      stop("Fiscal policy field '", name, "' must be TRUE or FALSE.", call. = FALSE)
    }
  }

  if (!policy$indexation_regime %in% unname(nzs_policy_regimes())) {
    stop("Fiscal policy has an unknown indexation regime.", call. = FALSE)
  }
  for (name in c("indexation_start_year", "eligibility_age_start_year", "income_test_start_year", "npv_base_year")) {
    scalar_number(name, lower = 1900, upper = 2200, integer = TRUE)
  }
  scalar_number("eligibility_age_new_age", lower = 65, upper = 100)
  scalar_number("eligibility_age_phase_in_years", lower = 0, upper = 100)
  scalar_number("eligibility_age_phase_in_months_per_year", lower = 0, upper = 120)
  scalar_number("income_test_from_age", lower = 0, upper = 120)
  scalar_number("income_test_to_age", lower = 0, upper = 121)
  scalar_number("income_test_threshold", lower = 0)
  scalar_number("income_test_abatement_rate", lower = 0, upper = 1)
  scalar_number("discount_rate", lower = -0.99, upper = 1)
  if (policy$income_test_to_age <= policy$income_test_from_age) {
    stop("Fiscal policy income-test end age must be above the start age.", call. = FALSE)
  }
  for (name in c(
    "indexation_grandparenting",
    "eligibility_age_active",
    "eligibility_age_grandparenting",
    "income_test_active",
    "income_test_grandparenting"
  )) {
    scalar_flag(name)
  }

  invisible(policy)
}

nzs_load_fiscal_inputs <- function() {
  inputs <- list(
    fiscal_baseline = utils::read.csv(nzs_app_data_file("fiscal-baseline.csv"), check.names = FALSE),
    population = utils::read.csv(nzs_app_data_file("population-by-age-sex.csv"), check.names = FALSE),
    rate_paths = utils::read.csv(nzs_app_data_file("nzs-rate-paths-by-category.csv"), check.names = FALSE),
    income_histogram = utils::read.csv(nzs_app_data_file("income-histogram.csv"), check.names = FALSE),
    benefit_assumptions = utils::read.csv(nzs_app_data_file("benefit-assumptions.csv"), check.names = FALSE)
  )

  nzs_validate_columns(inputs$fiscal_baseline, c(
    "year",
    "nzs_expense_billion",
    "nominal_gdp_billion",
    "ltfm_cpi_growth_projection",
    "ltfm_wage_growth_projection"
  ), "fiscal-baseline.csv")
  nzs_validate_columns(inputs$population, c("year", "age", "sex", "population"), "population-by-age-sex.csv")
  nzs_validate_columns(inputs$rate_paths, c(
    "year",
    "living_arrangement",
    "current_formula_annual",
    "cpi_annual",
    "wage_annual"
  ), "nzs-rate-paths-by-category.csv")
  nzs_validate_columns(inputs$income_histogram, c(
    "age_from",
    "age_to",
    "living_arrangement",
    "payment_category",
    "income_lower",
    "income_upper",
    "weighted_mean_income",
    "weighted_n"
  ), "income-histogram.csv")
  nzs_validate_columns(inputs$benefit_assumptions, c("parameter", "value"), "benefit-assumptions.csv")
  nzs_validate_fiscal_inputs(inputs)

  inputs
}

nzs_validate_columns <- function(data, required, name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      "Missing required columns in ",
      name,
      ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

nzs_validate_fiscal_inputs <- function(inputs) {
  baseline <- inputs$fiscal_baseline
  population <- inputs$population
  rate_paths <- inputs$rate_paths
  histogram <- inputs$income_histogram

  if (anyDuplicated(baseline$year) || any(diff(baseline$year) != 1) || min(baseline$year) != nzs_model_base_year() || any(!is.finite(as.matrix(baseline[, c("nzs_expense_billion", "nominal_gdp_billion", "ltfm_cpi_growth_projection", "ltfm_wage_growth_projection")]))) || any(baseline$nzs_expense_billion < 0) || any(baseline$nominal_gdp_billion <= 0)) {
    stop("Fiscal baseline must contain complete, finite annual projections.", call. = FALSE)
  }
  population_key <- paste(population$year, population$age, population$sex, sep = "|")
  if (anyDuplicated(population_key) || any(!population$year %in% baseline$year) || any(!is.finite(population$population)) || any(population$population < 0)) {
    stop("Population input has invalid keys or values.", call. = FALSE)
  }
  rate_key <- paste(rate_paths$year, rate_paths$living_arrangement, sep = "|")
  rate_columns <- c("current_formula_annual", "cpi_annual", "wage_annual")
  expected_arrangements <- c("couple", "single_sharing", "single_living_alone")
  if (anyDuplicated(rate_key) || !setequal(unique(rate_paths$year), baseline$year) || !setequal(unique(rate_paths$living_arrangement), expected_arrangements) || any(!is.finite(as.matrix(rate_paths[, rate_columns]))) || any(as.matrix(rate_paths[, rate_columns]) < 0)) {
    stop("NZS rate paths are incomplete or invalid.", call. = FALSE)
  }
  histogram_key <- paste(histogram$age_band, histogram$living_arrangement, histogram$income_lower, histogram$income_upper, sep = "|")
  histogram_totals <- stats::aggregate(weighted_n ~ age_band, data = histogram, sum)
  expected_age_bands <- c("65-69", "70-74", "75+")
  if (anyDuplicated(histogram_key) || !setequal(unique(histogram$age_band), expected_age_bands) || !setequal(unique(histogram$living_arrangement), expected_arrangements) || any(!is.finite(histogram$weighted_n)) || any(histogram$weighted_n < 0) || any(abs(histogram_totals$weighted_n - 1) > 1e-8)) {
    stop("Income histogram weights must form valid age-band distributions.", call. = FALSE)
  }
  invisible(inputs)
}

nzs_selected_annual_rate <- function(rate_paths, regime) {
  switch(
    regime,
    A = rate_paths$current_formula_annual,
    B = rate_paths$cpi_annual,
    C = rate_paths$wage_annual,
    stop("Unknown indexation regime: ", regime, call. = FALSE)
  )
}

nzs_age_band_for_age <- function(age) {
  cut(
    age,
    breaks = c(65, 70, 75, Inf),
    right = FALSE,
    labels = c("65-69", "70-74", "75+")
  )
}

nzs_effective_eligibility_age <- function(year, policy) {
  if (!isTRUE(policy$eligibility_age_active) || year < policy$eligibility_age_start_year) {
    return(65)
  }
  if (policy$eligibility_age_phase_in_months_per_year <= 0) {
    return(policy$eligibility_age_new_age)
  }
  elapsed_years <- year - policy$eligibility_age_start_year + 1
  phase_years <- elapsed_years * policy$eligibility_age_phase_in_months_per_year / 12
  min(policy$eligibility_age_new_age, 65 + phase_years)
}

nzs_phase_in_months_per_year <- function(new_age, phase_in_years, base_age = 65) {
  if (phase_in_years <= 0) {
    return(0)
  }
  ((new_age - base_age) * 12) / phase_in_years
}

nzs_post_reform_share <- function(year, age, start_year, grandparenting) {
  if (year < start_year) {
    return(rep(0, length(age)))
  }
  if (!isTRUE(grandparenting)) {
    return(rep(1, length(age)))
  }
  age_at_start <- age - (year - start_year)
  as.numeric(age_at_start < 65)
}

nzs_age_excluded_share <- function(age, effective_age, base_age = 65) {
  pmin(1, pmax(0, effective_age - pmax(age, base_age)))
}

nzs_ea_protected_share <- function(year, age, start_year, grandparenting) {
  if (year < start_year || !isTRUE(grandparenting)) {
    return(rep(0, length(age)))
  }
  age_at_start <- age - (year - start_year)
  as.numeric(age_at_start >= 65)
}

nzs_apply_eligibility_age_exposure <- function(population, year, policy) {
  out <- population[population$age >= 65, ]
  if (nrow(out) == 0) {
    out$ea_excluded_share <- numeric()
    out$remaining_share <- numeric()
    return(out)
  }

  effective_age <- nzs_effective_eligibility_age(year, policy)
  protected_share <- nzs_ea_protected_share(
    year,
    out$age,
    policy$eligibility_age_start_year,
    policy$eligibility_age_grandparenting
  )
  out$ea_excluded_share <- nzs_age_excluded_share(out$age, effective_age) * (1 - protected_share)
  out$remaining_share <- 1 - out$ea_excluded_share
  out
}

nzs_income_test_abatement_from_histogram <- function(histogram, threshold, rate, cap) {
  pmin(cap, rate * pmax(0, histogram$weighted_mean_income - threshold))
}

nzs_benefit_assumption <- function(inputs, parameter) {
  value <- inputs$benefit_assumptions$value[inputs$benefit_assumptions$parameter == parameter]
  if (length(value) == 0) {
    stop("Missing benefit assumption: ", parameter, call. = FALSE)
  }
  as.numeric(value[[1]])
}

nzs_public_benefit_assumption <- function(inputs, parameter) {
  nzs_benefit_assumption(inputs, paste0("public_", parameter))
}

nzs_public_main_benefit_offset_share <- function(inputs) {
  takeup <- nzs_public_benefit_assumption(inputs, "main_benefit_takeup")
  blend <- (
    nzs_public_benefit_assumption(inputs, "jss_mix_share") *
      nzs_public_benefit_assumption(inputs, "jss_benefit_to_nzs") +
      nzs_public_benefit_assumption(inputs, "slp_mix_share") *
      nzs_public_benefit_assumption(inputs, "slp_benefit_to_nzs") +
      nzs_public_benefit_assumption(inputs, "eb_mix_share") *
      nzs_public_benefit_assumption(inputs, "eb_benefit_to_nzs") +
      nzs_public_benefit_assumption(inputs, "sps_mix_share") *
      nzs_public_benefit_assumption(inputs, "sps_benefit_to_nzs")
  )

  takeup * blend
}

nzs_benefit_offset_share <- function(inputs) {
  main_share <- nzs_public_main_benefit_offset_share(inputs)
  as_delta <- nzs_public_benefit_assumption(inputs, "as_delta_takeup") *
    nzs_public_benefit_assumption(inputs, "as_amount_to_nzs")
  da_delta <- nzs_public_benefit_assumption(inputs, "da_delta_takeup") *
    nzs_public_benefit_assumption(inputs, "da_amount_to_nzs")
  tas_delta <- nzs_public_benefit_assumption(inputs, "tas_delta_takeup") *
    nzs_public_benefit_assumption(inputs, "tas_amount_to_nzs")
  supp_share <- as_delta + da_delta + tas_delta
  main_share + supp_share
}

nzs_project_fiscal_impacts <- function(policy = nzs_default_fiscal_policy(), inputs = nzs_load_fiscal_inputs()) {
  nzs_validate_fiscal_policy(policy)
  baseline <- inputs$fiscal_baseline
  population <- inputs$population
  histogram <- inputs$income_histogram
  rate_paths <- inputs$rate_paths

  baseline$cum_wage <- nzs_cumulative_growth_factor(baseline$ltfm_wage_growth_projection)
  baseline$cum_cpi <- nzs_cumulative_growth_factor(baseline$ltfm_cpi_growth_projection)

  years <- baseline$year
  rows <- vector("list", length(years))
  offset_share <- nzs_benefit_offset_share(inputs)

  for (i in seq_along(years)) {
    year <- years[[i]]
    year_pop <- population[population$year == year, ]
    pop_65plus <- sum(year_pop$population[year_pop$age >= 65], na.rm = TRUE)
    per_capita_nzs <- baseline$nzs_expense_billion[[i]] * 1e9 / pop_65plus

    effective_age <- nzs_effective_eligibility_age(year, policy)
    exposed_pop <- nzs_apply_eligibility_age_exposure(year_pop, year, policy)
    excluded_population <- sum(exposed_pop$population * exposed_pop$ea_excluded_share, na.rm = TRUE)
    ea_gross <- excluded_population * per_capita_nzs / 1e9

    eligible_pop <- exposed_pop[exposed_pop$remaining_share > 0, ]
    eligible_population <- sum(eligible_pop$population * eligible_pop$remaining_share, na.rm = TRUE)

    rate_year <- rate_paths[rate_paths$year == year, ]
    regime_ratio <- nzs_started_aggregate_indexation_ratio(
      baseline,
      row_index = i,
      regime = policy$indexation_regime,
      start_year = policy$indexation_start_year
    )
    index_active <- year >= policy$indexation_start_year && !identical(policy$indexation_regime, "A")
    index_gp <- nzs_post_reform_share(
      year,
      eligible_pop$age,
      policy$indexation_start_year,
      policy$indexation_grandparenting
    )
    indexation_population <- sum(eligible_pop$population * eligible_pop$remaining_share * index_gp, na.rm = TRUE)
    indexation_saving <- if (index_active) {
      indexation_population * per_capita_nzs * (1 - regime_ratio) / 1e9
    } else {
      0
    }

    income_test_saving <- nzs_income_test_saving_for_year(
      year = year,
      policy = policy,
      inputs = inputs,
      population = eligible_pop,
      histogram = histogram,
      rate_paths = rate_year,
      income_factor = baseline$cum_wage[[i]],
      cpi_factor = baseline$cum_cpi[[i]]
    )

    ea_offset <- ea_gross * offset_share
    gross_saving <- ea_gross + indexation_saving + income_test_saving
    net_saving <- gross_saving - ea_offset
    package_cost <- baseline$nzs_expense_billion[[i]] - net_saving

    rows[[i]] <- data.frame(
      year = year,
      status_quo_cost_billion = baseline$nzs_expense_billion[[i]],
      package_cost_billion = package_cost,
      nominal_gdp_billion = baseline$nominal_gdp_billion[[i]],
      population_65plus = pop_65plus,
      excluded_population = excluded_population,
      eligibility_age_effective = effective_age,
      ea_gross_saving_billion = ea_gross,
      ea_offset_billion = ea_offset,
      ea_net_saving_billion = ea_gross - ea_offset,
      indexation_saving_billion = indexation_saving,
      income_test_saving_billion = income_test_saving,
      gross_saving_billion = gross_saving,
      net_saving_billion = net_saving,
      status_quo_percent_gdp = baseline$nzs_expense_billion[[i]] / baseline$nominal_gdp_billion[[i]],
      package_percent_gdp = package_cost / baseline$nominal_gdp_billion[[i]]
    )
  }

  do.call(rbind, rows)
}

nzs_cumulative_growth_factor <- function(growth) {
  out <- rep(1, length(growth))
  if (length(growth) > 1) {
    out[-1] <- cumprod(1 + growth[-1])
  }
  out
}

nzs_aggregate_indexation_ratio <- function(regime, cum_cpi, cum_wage) {
  switch(
    regime,
    A = 1,
    B = cum_cpi / cum_wage,
    C = 1,
    stop("Unknown indexation regime: ", regime, call. = FALSE)
  )
}

nzs_started_aggregate_indexation_ratio <- function(baseline, row_index, regime, start_year) {
  year <- baseline$year[[row_index]]
  if (year < start_year || identical(regime, "A")) {
    return(1)
  }
  anchor_year <- max(min(baseline$year), start_year - 1)
  anchor_index <- match(anchor_year, baseline$year)
  if (is.na(anchor_index)) {
    stop("Could not find indexation anchor year: ", anchor_year, call. = FALSE)
  }
  cpi_growth <- baseline$cum_cpi[[row_index]] / baseline$cum_cpi[[anchor_index]]
  wage_growth <- baseline$cum_wage[[row_index]] / baseline$cum_wage[[anchor_index]]
  nzs_aggregate_indexation_ratio(regime, cpi_growth, wage_growth)
}


nzs_started_category_rate <- function(rate_paths, living_arrangement, regime, start_year, year) {
  rows <- rate_paths[rate_paths$living_arrangement == living_arrangement, ]
  if (nrow(rows) == 0) {
    return(NA_real_)
  }
  current <- rows$current_formula_annual[rows$year == year]
  if (length(current) == 0 || year < start_year || identical(regime, "A")) {
    return(if (length(current) == 0) NA_real_ else current[[1]])
  }

  selected <- nzs_selected_annual_rate(rows, regime)
  selected_year <- selected[rows$year == year]
  anchor_year <- max(min(rows$year), start_year - 1)
  current_anchor <- rows$current_formula_annual[rows$year == anchor_year]
  selected_anchor <- selected[rows$year == anchor_year]
  if (length(selected_year) == 0 || length(current_anchor) == 0 || length(selected_anchor) == 0) {
    return(NA_real_)
  }
  current_anchor[[1]] * selected_year[[1]] / selected_anchor[[1]]
}

nzs_income_test_saving_for_year <- function(year, policy, inputs, population, histogram, rate_paths, income_factor, cpi_factor) {
  if (!isTRUE(policy$income_test_active) || year < policy$income_test_start_year || nrow(population) == 0) {
    return(0)
  }

  pop <- population[
    population$age >= policy$income_test_from_age &
      population$age < policy$income_test_to_age,
  ]
  if (nrow(pop) == 0) {
    return(0)
  }

  pop$age_band <- as.character(nzs_age_band_for_age(pop$age))
  pop$income_test_reform_share <- nzs_post_reform_share(
    year,
    pop$age,
    policy$income_test_start_year,
    policy$income_test_grandparenting
  )
  pop$indexation_reform_share <- if (year >= policy$indexation_start_year && !identical(policy$indexation_regime, "A")) {
    nzs_post_reform_share(
      year,
      pop$age,
      policy$indexation_start_year,
      policy$indexation_grandparenting
    )
  } else {
    rep(0, nrow(pop))
  }
  remaining_share <- pop$remaining_share %||% rep(1, nrow(pop))
  pop$exposed_population_status_quo_cap <- pop$population *
    remaining_share *
    pop$income_test_reform_share *
    (1 - pop$indexation_reform_share)
  pop$exposed_population_reform_cap <- pop$population *
    remaining_share *
    pop$income_test_reform_share *
    pop$indexation_reform_share

  pop_long <- rbind(
    data.frame(
      age_band = pop$age_band,
      cap_regime = "A",
      exposed_population = pop$exposed_population_status_quo_cap
    ),
    data.frame(
      age_band = pop$age_band,
      cap_regime = policy$indexation_regime,
      exposed_population = pop$exposed_population_reform_cap
    )
  )
  pop_long <- stats::aggregate(
    exposed_population ~ age_band + cap_regime,
    data = pop_long,
    FUN = sum,
    na.rm = TRUE
  )
  pop_long <- pop_long[pop_long$exposed_population > 0, ]
  if (nrow(pop_long) == 0) {
    return(0)
  }

  histogram$cell_total_weight <- ave(
    histogram$weighted_n,
    histogram$age_from,
    histogram$age_to,
    FUN = sum
  )
  histogram$cell_share <- ifelse(histogram$cell_total_weight > 0, histogram$weighted_n / histogram$cell_total_weight, 0)

  merged <- merge(
    pop_long,
    histogram,
    by = "age_band",
    all.x = FALSE,
    all.y = FALSE
  )
  if (nrow(merged) == 0) {
    return(0)
  }

  merged$cap <- mapply(
    function(living_arrangement, cap_regime) {
      nzs_started_category_rate(
        inputs$rate_paths,
        living_arrangement = living_arrangement,
        regime = cap_regime,
        start_year = policy$indexation_start_year,
        year = year
      )
    },
    merged$living_arrangement,
    merged$cap_regime
  )
  merged$weighted_mean_income <- merged$weighted_mean_income * income_factor
  threshold <- policy$income_test_threshold * cpi_factor
  merged$abatement <- nzs_income_test_abatement_from_histogram(
    merged,
    threshold = threshold,
    rate = policy$income_test_abatement_rate,
    cap = merged$cap
  )
  sum(merged$exposed_population * merged$cell_share * merged$abatement, na.rm = TRUE) / 1e9
}

nzs_current_income_test_incidence <- function(policy, inputs, base_year = nzs_model_base_year()) {
  histogram <- inputs$income_histogram
  population <- inputs$population
  rate_paths <- inputs$rate_paths[inputs$rate_paths$year == base_year, ]

  if (!isTRUE(policy$income_test_active) || policy$income_test_abatement_rate <= 0) {
    return(data.frame())
  }

  pop <- population[
    population$year == base_year &
      population$age >= policy$income_test_from_age &
      population$age < policy$income_test_to_age,
  ]
  if (nrow(pop) == 0) {
    return(data.frame())
  }
  pop$age_band <- as.character(nzs_age_band_for_age(pop$age))
  pop_by_age_band <- stats::aggregate(
    population ~ age_band,
    data = pop,
    FUN = sum,
    na.rm = TRUE
  )

  histogram$cell_total_weight <- ave(
    histogram$weighted_n,
    histogram$age_from,
    histogram$age_to,
    FUN = sum
  )
  histogram$cell_share <- ifelse(histogram$cell_total_weight > 0, histogram$weighted_n / histogram$cell_total_weight, 0)

  merged <- merge(pop_by_age_band, histogram, by = "age_band", all.x = FALSE, all.y = FALSE)
  if (nrow(merged) == 0) {
    return(data.frame())
  }

  merged$cap <- mapply(
    function(living_arrangement) {
      nzs_started_category_rate(
        rate_paths,
        living_arrangement = living_arrangement,
        regime = "A",
        start_year = base_year,
        year = base_year
      )
    },
    merged$living_arrangement
  )
  merged$threshold <- policy$income_test_threshold
  merged$abatement <- nzs_income_test_abatement_from_histogram(
    merged,
    threshold = merged$threshold,
    rate = policy$income_test_abatement_rate,
    cap = merged$cap
  )
  merged$estimated_people <- merged$population * merged$cell_share
  merged$estimated_people_affected <- ifelse(merged$abatement > 0, merged$estimated_people, 0)
  merged$total_abatement <- merged$estimated_people * merged$abatement
  merged$weighted_status_quo_payment <- merged$estimated_people * merged$cap
  merged$weighted_status_quo_payment_affected <- merged$estimated_people_affected * merged$cap

  out <- stats::aggregate(
    cbind(
      estimated_people,
      estimated_people_affected,
      total_abatement,
      weighted_status_quo_payment,
      weighted_status_quo_payment_affected
    ) ~ income_lower + income_upper,
    data = merged,
    FUN = sum,
    na.rm = TRUE
  )

  display_breaks <- c(0, 10000, 20000, 30000, 50000, 80000, 120000, Inf)
  out$display_bin <- cut(
    out$income_lower,
    breaks = display_breaks,
    right = FALSE,
    include.lowest = TRUE,
    labels = FALSE
  )
  out$display_income_lower <- display_breaks[out$display_bin]
  out$display_income_upper <- display_breaks[out$display_bin + 1]
  out <- stats::aggregate(
    cbind(
      estimated_people,
      estimated_people_affected,
      total_abatement,
      weighted_status_quo_payment,
      weighted_status_quo_payment_affected
    ) ~ display_bin + display_income_lower + display_income_upper,
    data = out,
    FUN = sum,
    na.rm = TRUE
  )
  names(out)[names(out) == "display_income_lower"] <- "income_lower"
  names(out)[names(out) == "display_income_upper"] <- "income_upper"
  out$share_affected <- ifelse(out$estimated_people > 0, out$estimated_people_affected / out$estimated_people, 0)
  total_population <- sum(out$estimated_people, na.rm = TRUE)
  if (total_population > 0) {
    out$affected_population_share <- out$estimated_people_affected / total_population
  } else {
    out$affected_population_share <- rep(0, nrow(out))
  }
  out$average_annual_reduction_affected <- ifelse(
    out$estimated_people_affected > 0,
    out$total_abatement / out$estimated_people_affected,
    0
  )
  out$average_weekly_reduction_affected <- out$average_annual_reduction_affected / 52
  out$status_quo_payment <- ifelse(out$estimated_people > 0, out$weighted_status_quo_payment / out$estimated_people, 0)
  out$status_quo_payment_affected <- ifelse(
    out$estimated_people_affected > 0,
    out$weighted_status_quo_payment_affected / out$estimated_people_affected,
    0
  )
  out$average_percent_loss_affected <- ifelse(
    out$estimated_people_affected > 0 & out$status_quo_payment_affected > 0,
    out$average_annual_reduction_affected / out$status_quo_payment_affected,
    0
  )
  out[order(out$display_bin), ]
}

nzs_summarise_fiscal_impacts <- function(fiscal_path, discount_rate = 0.043, base_year = nzs_model_base_year()) {
  discount_factor <- 1 / (1 + discount_rate) ^ pmax(0, fiscal_path$year - base_year)
  data.frame(
    metric = c(
      "Net fiscal saving NPV",
      "EA net saving NPV",
      "Income-test saving NPV",
      "Indexation saving NPV",
      "EA offset NPV",
      "Peak annual saving",
      "2065 status quo NZS % GDP",
      "2065 package NZS % GDP"
    ),
    value = c(
      sum(fiscal_path$net_saving_billion * discount_factor),
      sum(fiscal_path$ea_net_saving_billion * discount_factor),
      sum(fiscal_path$income_test_saving_billion * discount_factor),
      sum(fiscal_path$indexation_saving_billion * discount_factor),
      sum(fiscal_path$ea_offset_billion * discount_factor),
      max(fiscal_path$net_saving_billion, na.rm = TRUE),
      fiscal_path$status_quo_percent_gdp[fiscal_path$year == max(fiscal_path$year)],
      fiscal_path$package_percent_gdp[fiscal_path$year == max(fiscal_path$year)]
    )
  )
}
