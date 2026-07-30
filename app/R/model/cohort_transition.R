nzs_default_cohort_transition_birth_years <- function(base_year = nzs_model_base_year()) {
  (base_year - 70):(base_year - 20)
}

nzs_default_cohort_deferral_birth_years <- function(policy, context_years = 5) {
  start_birth_year <- policy$eligibility_age_start_year - 65
  if (isTRUE(policy$eligibility_age_active) &&
      policy$eligibility_age_phase_in_months_per_year > 0) {
    years_to_target <- ceiling(
      (policy$eligibility_age_new_age - 65) * 12 /
        policy$eligibility_age_phase_in_months_per_year
    )
  } else {
    years_to_target <- 1
  }
  end_birth_year <- policy$eligibility_age_start_year + years_to_target - 1 - 65
  (start_birth_year - context_years):(end_birth_year + context_years)
}

nzs_project_cohort_deferral <- function(
    fiscal_policy,
    inputs,
    birth_years = NULL,
    ages = NULL,
    base_year = nzs_model_base_year()) {
  if (is.null(birth_years)) {
    if (is.null(ages)) {
      birth_years <- nzs_default_cohort_deferral_birth_years(fiscal_policy)
    } else {
      birth_years <- base_year - ages
    }
  }
  ages <- base_year - birth_years
  base_population <- inputs$population[
    inputs$population$year == base_year & inputs$population$age %in% ages,
  ]
  population_by_age <- stats::aggregate(
    population ~ age,
    data = base_population,
    FUN = sum,
    na.rm = TRUE
  )

  rows <- vector("list", length(ages))
  for (i in seq_along(ages)) {
    age <- ages[[i]]
    birth_year <- birth_years[[i]]
    year_at_65 <- birth_year + 65
    effective_age <- if (isTRUE(fiscal_policy$eligibility_age_active)) {
      nzs_effective_eligibility_age(year_at_65, fiscal_policy)
    } else {
      65
    }
    protected <- isTRUE(fiscal_policy$eligibility_age_active) &&
      isTRUE(fiscal_policy$eligibility_age_grandparenting) &&
      age + (fiscal_policy$eligibility_age_start_year - base_year) >= 65
    years_deferred <- if (protected) {
      0
    } else {
      max(0, effective_age - 65)
    }
    population <- population_by_age$population[population_by_age$age == age]
    if (length(population) == 0) {
      population <- NA_real_
    }

    rows[[i]] <- data.frame(
      current_age = age,
      birth_year = birth_year,
      year_at_65 = year_at_65,
      effective_eligibility_age = effective_age,
      years_deferred = years_deferred,
      population = population,
      affected_population = population * as.numeric(years_deferred > 0),
      deferred_person_years = population * years_deferred,
      protected = protected
    )
  }

  do.call(rbind, rows)
}

nzs_project_cohort_transition <- function(
    fiscal_policy,
    inputs,
    birth_years = NULL,
    base_year = nzs_model_base_year(),
    nominal_discount_rate = 0.043) {
  if (is.null(birth_years)) {
    birth_years <- nzs_default_cohort_transition_birth_years(base_year)
  }

  max_age <- max(inputs$survival$age, na.rm = TRUE)
  min_current_age <- min(base_year - birth_years)
  max_year <- base_year + max_age - min_current_age
  macro <- nzs_adequacy_factors(nzs_extend_ltfm_projections(inputs$ltfm, max_year))
  rate_path <- nzs_rate_path(
    nzs_economic_path_from_ltfm(macro[, c("year", "ltfm_cpi_growth_projection", "ltfm_wage_growth_projection")])
  )

  rows <- vector("list", length(birth_years))
  for (i in seq_along(birth_years)) {
    birth_year <- birth_years[[i]]
    current_age <- base_year - birth_year
    first_age <- max(65, current_age)
    ages <- first_age:max_age
    start_survival <- inputs$survival$survival_probability[match(first_age, inputs$survival$age)]
    conditional_survival <- inputs$survival$survival_probability[match(ages, inputs$survival$age)] / start_survival
    years <- birth_year + ages

    pv_status_quo <- 0
    pv_loss <- 0
    ea_loss_any <- FALSE
    indexation_loss_any <- FALSE
    phase_in_cohort <- FALSE

    for (j in seq_along(ages)) {
      age <- ages[[j]]
      year <- years[[j]]
      rate_row <- rate_path[rate_path$year == year, ]
      macro_row <- macro[macro$year == year, ]
      period <- age - first_age
      real_discount_factor <- macro_row$cpi_factor / (1 + nominal_discount_rate) ^ period
      discount_factor <- conditional_survival[[j]] * real_discount_factor

      status_quo_real <- rate_row$couple_current_law * 52 / macro_row$cpi_factor
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
      ea_excluded_share <- nzs_age_excluded_share(age, effective_age)
      ea_loss_any <- ea_loss_any || ea_excluded_share > 0

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
      reform_real <- selected_weekly * 52 * (1 - ea_excluded_share) / macro_row$cpi_factor
      annual_loss <- status_quo_real - reform_real
      indexation_loss_any <- indexation_loss_any || (index_active && ea_excluded_share < 1 && abs(annual_loss) > 1)

      pv_status_quo <- pv_status_quo + status_quo_real * discount_factor
      pv_loss <- pv_loss + annual_loss * discount_factor
    }

    age_at_ea_start <- current_age + (fiscal_policy$eligibility_age_start_year - base_year)
    age_at_index_start <- current_age + (fiscal_policy$indexation_start_year - base_year)
    protected <- (
      isTRUE(fiscal_policy$eligibility_age_active) &&
        isTRUE(fiscal_policy$eligibility_age_grandparenting) &&
        age_at_ea_start >= 65
    ) || (
      !identical(fiscal_policy$indexation_regime, "A") &&
        isTRUE(fiscal_policy$indexation_grandparenting) &&
        age_at_index_start >= 65
    )

    if (isTRUE(fiscal_policy$eligibility_age_active) && current_age < 65) {
      age65_year <- birth_year + 65
      effective_at_65 <- nzs_effective_eligibility_age(age65_year, fiscal_policy)
      phase_in_cohort <- effective_at_65 > 65 && effective_at_65 < fiscal_policy$eligibility_age_new_age
    }

    pct_loss <- if (pv_status_quo > 0) pv_loss / pv_status_quo else NA_real_
    status <- if (protected && abs(pct_loss) < 0.0001) {
      "Grandparented"
    } else if (phase_in_cohort) {
      "Phase-in"
    } else if (abs(pct_loss) < 0.0001) {
      "Unaffected"
    } else {
      "Fully affected"
    }

    rows[[i]] <- data.frame(
      birth_year = birth_year,
      current_age = current_age,
      pv_status_quo = pv_status_quo,
      pv_loss = pv_loss,
      pct_loss = pct_loss,
      status = status,
      protected = protected,
      phase_in_cohort = phase_in_cohort,
      ea_loss_any = ea_loss_any,
      indexation_loss_any = indexation_loss_any
    )
  }

  out <- do.call(rbind, rows)
  ea_phase_in_birth_year_min <- NA_real_
  ea_phase_in_birth_year_max <- NA_real_
  if (isTRUE(fiscal_policy$eligibility_age_active) &&
      fiscal_policy$eligibility_age_phase_in_months_per_year > 0) {
    phase_years <- ceiling(
      (fiscal_policy$eligibility_age_new_age - 65) * 12 /
        fiscal_policy$eligibility_age_phase_in_months_per_year
    )
    ea_phase_in_birth_year_min <- fiscal_policy$eligibility_age_start_year - 65
    ea_phase_in_birth_year_max <- fiscal_policy$eligibility_age_start_year + phase_years - 1 - 65
  }
  attr(out, "annotations") <- list(
    eligibility_age_marker = if (isTRUE(fiscal_policy$eligibility_age_active)) {
      fiscal_policy$eligibility_age_start_year - 65
    } else {
      NA_real_
    },
    indexation_marker = if (!identical(fiscal_policy$indexation_regime, "A")) {
      fiscal_policy$indexation_start_year - 65
    } else {
      NA_real_
    },
    phase_in_birth_year_min = ea_phase_in_birth_year_min,
    phase_in_birth_year_max = ea_phase_in_birth_year_max,
    nominal_discount_rate = nominal_discount_rate,
    discount_basis = "real_2026_cpi_adjusted"
  )
  out
}
