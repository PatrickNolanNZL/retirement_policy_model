extract_ltfm_economic_path <- function(
    xlsx_path,
    start_year = 2026,
    end_year = 2065) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Run renv::restore() or renv::install('readxl').", call. = FALSE)
  }

  ltfm <- readxl::read_excel(
    xlsx_path,
    sheet = "Long-Term Fiscal Model",
    col_names = FALSE,
    .name_repair = "minimal"
  )

  get_row <- function(row_number) {
    unlist(ltfm[row_number, ], use.names = FALSE)
  }

  years <- suppressWarnings(as.integer(get_row(6)))
  keep <- !is.na(years) &
    years >= start_year &
    years <= end_year

  ltfm_cpi_growth_projection <- as.numeric(get_row(29)[keep])
  ltfm_wage_growth_projection <- as.numeric(get_row(26)[keep])

  ltfm_cpi_growth_projection[years[keep] == start_year] <- 0
  ltfm_wage_growth_projection[years[keep] == start_year] <- 0

  data.frame(
    year = years[keep],
    ltfm_cpi_growth_projection = ltfm_cpi_growth_projection,
    ltfm_wage_growth_projection = ltfm_wage_growth_projection
  )
}

extract_ltfm_fiscal_baseline <- function(
    xlsx_path,
    start_year = 2026,
    end_year = 2065) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Run renv::restore() or renv::install('readxl').", call. = FALSE)
  }

  ltfm <- readxl::read_excel(
    xlsx_path,
    sheet = "Long-Term Fiscal Model",
    col_names = FALSE,
    .name_repair = "minimal"
  )

  get_row <- function(row_number) {
    unlist(ltfm[row_number, ], use.names = FALSE)
  }

  years <- suppressWarnings(as.integer(get_row(6)))
  keep <- !is.na(years) &
    years >= start_year &
    years <= end_year

  cpi_growth <- as.numeric(get_row(29)[keep])
  wage_growth <- as.numeric(get_row(26)[keep])
  cpi_growth[years[keep] == start_year] <- 0
  wage_growth[years[keep] == start_year] <- 0

  data.frame(
    year = years[keep],
    nzs_expense_billion = as.numeric(get_row(180)[keep]),
    nominal_gdp_billion = as.numeric(get_row(13)[keep]),
    ltfm_cpi_growth_projection = cpi_growth,
    ltfm_wage_growth_projection = wage_growth
  )
}

extract_ltfm_population_by_age_sex <- function(
    xlsx_path,
    start_year = 2026,
    end_year = 2065,
    max_age = 95) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Run renv::restore() or renv::install('readxl').", call. = FALSE)
  }

  pop <- readxl::read_excel(
    xlsx_path,
    sheet = "Population Treasury",
    col_names = FALSE,
    .name_repair = "minimal"
  )

  years <- suppressWarnings(as.integer(unlist(pop[3, ], use.names = FALSE)))
  year_cols <- which(!is.na(years) & years >= start_year & years <= end_year)

  read_block <- function(start_row, end_row, sex) {
    age_values <- unlist(pop[start_row:end_row, 1], use.names = FALSE)
    ages <- ifelse(age_values == "95 & over", max_age, suppressWarnings(as.integer(age_values)))
    out <- do.call(
      rbind,
      lapply(seq_along(year_cols), function(i) {
        col <- year_cols[[i]]
        data.frame(
          year = years[[col]],
          age = ages,
          sex = sex,
          population = as.numeric(unlist(pop[start_row:end_row, col], use.names = FALSE))
        )
      })
    )
    out[!is.na(out$age), ]
  }

  rbind(
    read_block(15, 110, "female"),
    read_block(115, 210, "male")
  )
}
