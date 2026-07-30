nzs_source_model()

testthat::test_that("prepared fiscal inputs have complete, unique public-source contracts", {
  inputs <- nzs_load_fiscal_inputs()

  testthat::expect_true(all(diff(inputs$fiscal_baseline$year) == 1))
  testthat::expect_equal(anyDuplicated(inputs$fiscal_baseline$year), 0)
  testthat::expect_true(all(is.finite(as.matrix(inputs$fiscal_baseline[, c(
    "nzs_expense_billion",
    "nominal_gdp_billion",
    "ltfm_cpi_growth_projection",
    "ltfm_wage_growth_projection"
  )]))))

  population_key <- with(inputs$population, paste(year, age, sex, sep = "|"))
  testthat::expect_equal(anyDuplicated(population_key), 0)
  testthat::expect_true(all(inputs$population$year %in% inputs$fiscal_baseline$year))
  testthat::expect_true(all(inputs$population$population >= 0))

  histogram_totals <- stats::aggregate(weighted_n ~ age_band, inputs$income_histogram, sum)
  category_totals <- stats::aggregate(
    weighted_n ~ age_band + payment_category,
    inputs$income_histogram,
    sum
  )
  testthat::expect_equal(histogram_totals$weighted_n, rep(1, nrow(histogram_totals)), tolerance = 1e-10)
  testthat::expect_true(all(category_totals$weighted_n > 0))
  testthat::expect_false(any(grepl("^stage1_", inputs$benefit_assumptions$parameter)))
})

testthat::test_that("prepared rate paths reconcile to the shared NZS rate engine", {
  fiscal_inputs <- nzs_load_fiscal_inputs()
  economic_path <- nzs_economic_path_from_ltfm(nzs_load_ltfm_projections())
  shared_path <- nzs_rate_path(economic_path)
  rate_paths <- fiscal_inputs$rate_paths
  factors <- c(
    single_living_alone = 0.65,
    single_sharing = 0.60,
    couple = 0.50
  )

  testthat::expect_equal(sort(unique(rate_paths$year)), shared_path$year)
  testthat::expect_equal(
    as.integer(table(rate_paths$year)),
    rep(length(factors), nrow(shared_path))
  )
  testthat::expect_setequal(unique(rate_paths$living_arrangement), names(factors))

  factor <- unname(factors[rate_paths$living_arrangement])
  shared_row <- match(rate_paths$year, shared_path$year)
  testthat::expect_equal(rate_paths$current_formula_annual, shared_path$couple_current_law[shared_row] * factor * 52)
  testthat::expect_equal(rate_paths$cpi_annual, shared_path$couple_cpi[shared_row] * factor * 52)
  testthat::expect_equal(rate_paths$wage_annual, shared_path$couple_wage[shared_row] * factor * 52)
})

testthat::test_that("prepared Person inputs cover the complete modelling age ranges", {
  inputs <- nzs_load_person_impacts_inputs()
  earnings <- inputs$earnings
  retirement_income <- inputs$retirement_income
  survival <- inputs$survival

  testthat::expect_setequal(earnings$age, 15:64)
  core_age <- tolower(as.character(earnings$core_kiwisaver_age)) == "true"
  testthat::expect_setequal(earnings$age[core_age], 20:64)
  testthat::expect_equal(anyDuplicated(earnings$age), 0)
  testthat::expect_true(all(is.finite(as.matrix(earnings[, c(
    "lower_annual_linear", "median_annual_linear", "upper_annual_linear"
  )]))))

  testthat::expect_setequal(unique(retirement_income$retirement_income), c("low", "medium", "high"))
  testthat::expect_setequal(retirement_income$age, 65:100)
  retirement_key <- with(retirement_income, paste(age, retirement_income, sep = "|"))
  testthat::expect_equal(anyDuplicated(retirement_key), 0)
  testthat::expect_true(all(is.finite(retirement_income$non_nzs_taxable_income_annual)))

  testthat::expect_setequal(survival$age, 65:105)
  testthat::expect_equal(survival$survival_probability[survival$age == 65], 1)
  testthat::expect_true(all(diff(survival$survival_probability) <= 0))
  testthat::expect_true(all(survival$survival_probability >= 0 & survival$survival_probability <= 1))
})
