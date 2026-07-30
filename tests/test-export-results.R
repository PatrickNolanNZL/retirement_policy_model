nzs_test_export_results <- function() {
  nzs_source_model()
  fiscal_inputs <- nzs_load_fiscal_inputs()
  person_inputs <- nzs_load_person_impacts_inputs()
  policy <- nzs_default_fiscal_policy()
  policy$eligibility_age_phase_in_years <- 2
  policy$eligibility_age_phase_in_months_per_year <- nzs_phase_in_months_per_year(
    policy$eligibility_age_new_age,
    policy$eligibility_age_phase_in_years
  )

  fiscal_path <- nzs_project_fiscal_impacts(policy, fiscal_inputs)
  fiscal_summary <- nzs_summarise_fiscal_impacts(
    fiscal_path,
    discount_rate = policy$discount_rate,
    base_year = policy$npv_base_year
  )
  person_impacts <- nzs_project_person_impacts(
    fiscal_policy = policy,
    current_age = 35,
    earnings_archetype = "median",
    retirement_income_archetype = "medium",
    living_arrangement = "Single living alone",
    employee_rate = 0.035,
    employer_rate = 0.035,
    accumulation_return = nzs_kiwisaver_fund_returns()[["Balanced"]],
    drawdown_return = 0.025,
    current_balance = 0,
    inputs = person_inputs
  )
  person_fund_sensitivity <- nzs_person_impacts_fund_sensitivity(
    fiscal_policy = policy,
    current_age = 35,
    earnings_archetype = "median",
    retirement_income_archetype = "medium",
    living_arrangement = "Single living alone",
    drawdown_return = 0.025,
    current_balance = 0,
    inputs = person_inputs
  )

  nzs_build_export_results(
    policy = policy,
    fiscal_path = fiscal_path,
    fiscal_summary = fiscal_summary,
    cohort_transition = nzs_project_cohort_transition(
      fiscal_policy = policy,
      inputs = person_inputs,
      nominal_discount_rate = policy$discount_rate
    ),
    cohort_deferral = nzs_project_cohort_deferral(policy, fiscal_inputs),
    cohort_income_test_incidence = nzs_current_income_test_incidence(policy, fiscal_inputs),
    person_impacts = person_impacts,
    person_fund_sensitivity = person_fund_sensitivity,
    person_settings = list(
      `Current age in 2026` = 35,
      `Working-life earnings` = "median",
      `Retirement income` = "medium"
    ),
    exported_at = as.POSIXct("2026-07-27 12:00:00", tz = "UTC")
  )
}

testthat::test_that("export assembly contains expected tables and columns", {
  export_results <- nzs_test_export_results()

  testthat::expect_named(export_results, c("metadata", "readme", "settings", "fiscal", "cohort", "person"))
  testthat::expect_named(export_results$fiscal, c("headline_summary", "annual_fiscal_path", "annual_saving_decomposition"))
  testthat::expect_named(export_results$cohort, c("transition_impact_by_cohort", "ea_deferral_by_cohort", "income_test_incidence"))
  testthat::expect_named(export_results$person, c(
    "offset_summary",
    "working_life_earnings_and_contributions",
    "kiwisaver_balance_path",
    "retirement_income_composition",
    "replacement_rate",
    "fund_sensitivity"
  ))

  testthat::expect_true("status quo NZS cost ($bn nominal)" %in% names(export_results$fiscal$annual_fiscal_path))
  testthat::expect_true("lifetime loss under reform (%)" %in% names(export_results$cohort$transition_impact_by_cohort))
  testthat::expect_true("KiwiSaver balance ($ real 2026)" %in% names(export_results$person$kiwisaver_balance_path))
  testthat::expect_true(any(export_results$settings$setting == "Current age in 2026"))
})

testthat::test_that("export column formats distinguish years from counts", {
  testthat::expect_equal(nzs_export_column_numfmt("year"), '0;(0);"-"')
  testthat::expect_equal(nzs_export_column_numfmt("birth_year"), '0;(0);"-"')
  testthat::expect_equal(nzs_export_column_numfmt("year_at_65"), '0;(0);"-"')
  testthat::expect_equal(nzs_export_column_numfmt("years_deferred"), '0.0;(0.0);"-"')
  testthat::expect_equal(nzs_export_column_numfmt("deferred_person_years"), '#,##0;(#,##0);"-"')
  testthat::expect_equal(nzs_export_column_numfmt("people affected"), '#,##0;(#,##0);"-"')
})

testthat::test_that("export workbook writes expected sheets and tables", {
  export_results <- nzs_test_export_results()
  path <- tempfile(fileext = ".xlsx")
  nzs_write_export_workbook(export_results, path)

  testthat::expect_true(file.exists(path))
  wb <- openxlsx2::wb_load(path)
  testthat::expect_equal(
    unname(openxlsx2::wb_get_sheet_names(wb)),
    c("Cover", "Readme", "Settings", "Fiscal impacts", "Cohort impacts", "Person impacts")
  )
  testthat::expect_false("style-guide" %in% unname(openxlsx2::wb_get_sheet_names(wb)))

  fiscal_tables <- openxlsx2::wb_get_tables(wb, sheet = "Fiscal impacts")
  cohort_tables <- openxlsx2::wb_get_tables(wb, sheet = "Cohort impacts")
  person_tables <- openxlsx2::wb_get_tables(wb, sheet = "Person impacts")
  testthat::expect_true("tbl_fiscal_impacts_02_annual_fiscal_path" %in% fiscal_tables$tab_name)
  testthat::expect_true("tbl_cohort_impacts_03_income_test_incidence" %in% cohort_tables$tab_name)
  testthat::expect_true("tbl_person_impacts_04_retirement_income_composition" %in% person_tables$tab_name)

  fiscal_path <- openxlsx2::wb_to_df(path, sheet = "Fiscal impacts", rows = 19:59, cols = 2:7)
  testthat::expect_true("year" %in% names(fiscal_path))
  testthat::expect_true(all(!is.na(suppressWarnings(as.integer(fiscal_path$year)))))

  cover <- openxlsx2::wb_to_df(
    path,
    sheet = "Cover",
    col_names = FALSE,
    skip_empty_rows = FALSE,
    skip_empty_cols = FALSE,
    check_names = FALSE
  )
  testthat::expect_true("HEUSER | WHITTINGTON" %in% unlist(cover, use.names = FALSE))
  testthat::expect_true("www.heuserwhittington.com" %in% unlist(cover, use.names = FALSE))
  testthat::expect_true("Model version" %in% unlist(cover, use.names = FALSE))
  testthat::expect_true("v1.0.0" %in% unlist(cover, use.names = FALSE))
  testthat::expect_true("Model status" %in% unlist(cover, use.names = FALSE))
  testthat::expect_true(length(openxlsx2::styles_on_sheet(wb, "Fiscal impacts")) > 1)
})

testthat::test_that("model version matches released package metadata", {
  project_root <- nzs_test_project_root()
  package_version <- read.dcf(file.path(project_root, "DESCRIPTION"))[1, "Version"]

  testthat::expect_equal(nzs_model_version(), unname(package_version))
})

testthat::test_that("static workbook export declares its zip runtime dependency", {
  project_root <- nzs_test_project_root()
  imports <- trimws(unlist(strsplit(read.dcf(file.path(project_root, "DESCRIPTION"))[1, "Imports"], ",")))
  app_entry <- paste(readLines(file.path(project_root, "app", "app.R"), warn = FALSE), collapse = "\n")

  testthat::expect_true("zip" %in% imports)
  testthat::expect_match(app_entry, "library\\(zip\\)")
})
