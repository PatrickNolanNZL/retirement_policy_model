# Public-source preparation functions for the NZ retirement policy model.
#
# This file deliberately accepts explicit source and output directories. Raw
# sources are a maintenance input, not part of the released application.

nzs_public_source_files <- function(source_dir) {
  relative_paths <- c(
    ltfm = "treasury-ltfm/ltfm-htm-sep25.xlsx",
    ird_income = "ird-income-distributions/taxable-income-distribution-of-individuals-2025.xlsx",
    household = "stats-nz-family-household-projections/statsnz-poppr-fhh-001-mediumb-older-living-arrangement.csv",
    hlfs = "stats-nz-hlfs-main-job-earnings/stats-nz-hlfs-main-job-earnings-by-occupation-age-sex-ethnicity-2009-2025.csv",
    life_tables = "stats-nz-life-tables/new-zealand-period-life-tables-2022-2024.xlsx",
    qes = "stats-nz-qes/quarterly-employment-survey-december-2025-quarter.xlsx"
  )
  stats::setNames(file.path(source_dir, unname(relative_paths)), names(relative_paths))
}

nzs_require_public_sources <- function(source_dir) {
  paths <- nzs_public_source_files(source_dir)
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(
      "Missing required public source files:\n", paste("-", missing, collapse = "\n"),
      call. = FALSE
    )
  }
  paths
}

nzs_public_payment_categories <- function() {
  data.frame(
    payment_category = c("couple", "single_sharing", "single_living_alone"),
    living_arrangement = c("couple", "single_sharing", "single_living_alone"),
    gross_nzs_rate = c(22869.08, 27686.36, 30090.84),
    msd_share = c(0.6007253867853451, 0.14086170343244616, 0.25841290978220876),
    stringsAsFactors = FALSE
  )
}

nzs_prepare_ltfm_inputs <- function(ltfm_path) {
  projections <- extract_ltfm_economic_path(ltfm_path)
  baseline <- extract_ltfm_fiscal_baseline(ltfm_path)
  population <- extract_ltfm_population_by_age_sex(ltfm_path)
  list(projections = projections, baseline = baseline, population = population)
}

nzs_read_base_gross_aotwe <- function(path) {
  table <- readxl::read_excel(path, sheet = "Table 7", col_names = FALSE, .name_repair = "minimal")
  series_row <- unlist(table[8, ], use.names = FALSE)
  series_col <- which(series_row == "SBSZ9A")
  if (length(series_col) != 1L) stop("Could not identify QES total ordinary-time earnings series SBSZ9A.", call. = FALSE)
  years <- as.character(table[[1]])
  quarters <- as.character(table[[3]])
  row <- which(years == "2025" & quarters == "Dec")
  if (length(row) != 1L) {
    year_for_row <- rep(NA_character_, nrow(table))
    current_year <- NA_character_
    for (i in seq_len(nrow(table))) {
      if (!is.na(years[[i]]) && grepl("^[0-9]{4}$", years[[i]])) current_year <- years[[i]]
      year_for_row[[i]] <- current_year
    }
    row <- which(year_for_row == "2025" & quarters == "Dec")
  }
  value <- suppressWarnings(as.numeric(table[[series_col]][row[[1]]]))
  if (length(value) != 1L || !is.finite(value) || value <= 0) stop("QES AOTWE base value is invalid.", call. = FALSE)
  value
}

nzs_prepare_rate_paths_by_category <- function(projections, base_gross_aotwe_weekly) {
  defaults <- nzs_default_inputs()
  defaults$base_gross_aotwe_weekly <- base_gross_aotwe_weekly
  rate_path <- nzs_rate_path(nzs_economic_path_from_ltfm(projections, defaults), defaults)
  categories <- data.frame(
    living_arrangement = c("single_living_alone", "single_sharing", "couple"),
    rate_category = c("single_living_alone", "single_sharing", "couple_each"),
    rate_factor = unname(nzs_living_arrangements()),
    stringsAsFactors = FALSE
  )

  do.call(rbind, lapply(seq_len(nrow(categories)), function(i) {
    category <- categories[i, ]
    data.frame(
      year = rate_path$year,
      rate_category = category$rate_category,
      living_arrangement = category$living_arrangement,
      current_formula_annual = rate_path$couple_current_law * category$rate_factor * 52,
      cpi_annual = rate_path$couple_cpi * category$rate_factor * 52,
      wage_annual = rate_path$couple_wage * category$rate_factor * 52,
      stringsAsFactors = FALSE
    )
  }))
}

nzs_parse_income_band <- function(label, top_code_midpoint = 250000) {
  text <- gsub(",", "", trimws(as.character(label)))
  if (tolower(text) == "nil") return(c(lower = 0, upper = 0, midpoint = 0))
  numbers <- as.numeric(regmatches(text, gregexpr("[0-9]+(?:\\.[0-9]+)?", text, perl = TRUE))[[1]])
  if (grepl("^over", text, ignore.case = TRUE) && length(numbers) >= 1) {
    return(c(lower = numbers[[1]], upper = Inf, midpoint = top_code_midpoint))
  }
  if (length(numbers) >= 2) return(c(lower = numbers[[1]], upper = numbers[[2]], midpoint = mean(numbers[1:2])))
  stop("Cannot parse IRD income band: ", label, call. = FALSE)
}

nzs_read_ird_income_distribution <- function(path) {
  raw <- readxl::read_excel(path, sheet = "Age by income band distribution", col_names = FALSE, .name_repair = "minimal")
  age_columns <- c("65-69" = 14L, "70-74" = 15L, "75+" = 16L)
  rows <- list()
  for (i in 6:nrow(raw)) {
    label <- raw[[1]][[i]]
    if (is.na(label) || !nzchar(trimws(as.character(label)))) next
    parsed <- tryCatch(nzs_parse_income_band(label), error = function(e) NULL)
    if (is.null(parsed)) next
    for (age_band in names(age_columns)) {
      count <- suppressWarnings(as.numeric(raw[[age_columns[[age_band]]]][[i]]))
      if (!is.na(count) && count > 0) {
        rows[[length(rows) + 1L]] <- data.frame(
          age_band = age_band,
          ird_income_band = as.character(label),
          taxable_lower = parsed[["lower"]],
          taxable_upper = parsed[["upper"]],
          taxable_midpoint = parsed[["midpoint"]],
          count = count,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}

nzs_household_proxy_shares <- function(path, year = 2023L) {
  raw <- utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  code <- raw[["LAT_POPPR_FHH_001"]]
  proxy <- ifelse(code %in% c("LAT1", "LAT3"), "couple",
    ifelse(code == "LAT10", "single_living_alone", "single_sharing")
  )
  raw$payment_category <- proxy
  raw$population <- suppressWarnings(as.numeric(raw[["OBS_VALUE"]]))
  raw <- raw[
    raw[["YEAR_POPPR_FHH_001"]] == year & raw[["Sex"]] == "Total people" &
      raw[["Age"]] %in% c("65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90 years and over"),
    , drop = FALSE
  ]
  raw$age_band <- ifelse(raw[["Age"]] == "65-69 years", "65-69",
    ifelse(raw[["Age"]] == "70-74 years", "70-74", "75+")
  )
  totals <- stats::aggregate(population ~ age_band, raw, sum)
  counts <- stats::aggregate(population ~ age_band + payment_category, raw, sum)
  counts <- merge(counts, totals, by = "age_band", suffixes = c("", "_total"))
  counts$share <- counts$population / counts$population_total
  counts
}

nzs_rake_payment_category_shares <- function(proxy_shares, categories = nzs_public_payment_categories()) {
  age_totals <- stats::aggregate(population_total ~ age_band, proxy_shares, unique)
  wide <- reshape(proxy_shares[, c("age_band", "payment_category", "share")],
    idvar = "age_band", timevar = "payment_category", direction = "wide"
  )
  names(wide) <- sub("share\\.", "", names(wide))
  categories_names <- categories$payment_category
  wide <- wide[, c("age_band", categories_names), drop = FALSE]
  counts <- as.matrix(wide[, categories_names, drop = FALSE]) * age_totals$population_total
  target <- categories$msd_share * sum(age_totals$population_total)
  for (i in seq_len(100L)) {
    counts <- counts * (age_totals$population_total / rowSums(counts))
    counts <- sweep(counts, 2, target / colSums(counts), "*")
  }
  shares <- counts / rowSums(counts)
  out <- data.frame(age_band = wide$age_band, shares, check.names = FALSE)
  out <- reshape(out, varying = categories_names, v.names = "payment_category_share",
    timevar = "payment_category", times = categories_names, direction = "long"
  )
  row.names(out) <- NULL
  out$payment_category <- as.character(out$payment_category)
  out$source_year <- 2023L
  out <- merge(out, age_totals, by = "age_band")
  merge(out, categories[, c("payment_category", "living_arrangement", "gross_nzs_rate")], by = "payment_category")
}

nzs_income_bins <- function() {
  data.frame(
    income_lower = c(0, 500, 1000, 2000, 5000, 10000, 15000, 20000, 30000, 50000, 75000, 100000, 150000, 250000),
    income_upper = c(500, 1000, 2000, 5000, 10000, 15000, 20000, 30000, 50000, 75000, 100000, 150000, 250000, Inf)
  )
}

nzs_prepare_income_histogram <- function(ird, payment_shares) {
  age_bounds <- data.frame(age_band = c("65-69", "70-74", "75+"), age_from = c(65, 70, 75), age_to = c(70, 75, 120))
  expanded <- merge(ird, payment_shares, by = "age_band")
  expanded$non_nzs_income <- pmax(0, expanded$taxable_midpoint - expanded$gross_nzs_rate)
  expanded$weight <- expanded$count * expanded$payment_category_share
  bins <- nzs_income_bins()
  out <- list()
  groups <- split(expanded, interaction(expanded$age_band, expanded$payment_category, drop = TRUE))
  for (group in groups) {
    category_weight <- sum(group$weight)
    for (i in seq_len(nrow(bins))) {
      lower <- bins$income_lower[[i]]
      upper <- bins$income_upper[[i]]
      keep <- if (is.infinite(upper)) group$non_nzs_income >= lower else group$non_nzs_income >= lower & group$non_nzs_income < upper
      cell <- group[keep, , drop = FALSE]
      raw_weight <- sum(cell$weight)
      out[[length(out) + 1L]] <- data.frame(
        source = "IRD taxable income, MSD OIA payment mix, Stats NZ living-arrangement age gradient",
        variant = "statsnz_age_msd_calibrated_clipped",
        age_band = group$age_band[[1]],
        living_arrangement = group$living_arrangement[[1]],
        payment_category = group$payment_category[[1]],
        payment_category_share = group$payment_category_share[[1]],
        income_lower = lower,
        income_upper = upper,
        weighted_mean_income = if (raw_weight > 0) stats::weighted.mean(cell$non_nzs_income, cell$weight) else 0,
        income_bin_share = if (category_weight > 0) raw_weight / category_weight else 0,
        weighted_n = group$payment_category_share[[1]] * if (category_weight > 0) raw_weight / category_weight else 0,
        base_income_year = 2024L,
        income_growth_default = "wage",
        caveat_flags = "public_source_proxy;taxable_income;payment_category_independence;negative_residuals_clipped",
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, out)
  merge(out, age_bounds, by = "age_band", all.x = TRUE)[, c(
    "source", "variant", "age_band", "age_from", "age_to", "living_arrangement", "payment_category", "payment_category_share",
    "income_lower", "income_upper", "weighted_mean_income", "income_bin_share", "weighted_n", "base_income_year", "income_growth_default", "caveat_flags"
  )]
}

nzs_weighted_quantile <- function(values, weights, probability) {
  order <- order(values)
  values <- values[order]
  weights <- weights[order]
  values[[which(cumsum(weights) / sum(weights) >= probability)[[1]]]]
}

nzs_prepare_retirement_income_profiles <- function(ird, categories = nzs_public_payment_categories()) {
  rows <- list()
  for (age_band in unique(ird$age_band)) {
    part <- ird[ird$age_band == age_band, , drop = FALSE]
    for (i in seq_len(nrow(categories))) {
      value <- pmax(0, part$taxable_midpoint - categories$gross_nzs_rate[[i]])
      rows[[length(rows) + 1L]] <- data.frame(
        age_band = age_band,
        income = value,
        weight = part$count * categories$msd_share[[i]],
        stringsAsFactors = FALSE
      )
    }
  }
  pooled <- do.call(rbind, rows)
  specifications <- data.frame(retirement_income = c("low", "medium", "high"), probability = c(0.25, 0.5, 0.75))
  anchors <- do.call(rbind, lapply(seq_len(nrow(specifications)), function(i) {
    probability <- specifications$probability[[i]]
    data.frame(
      retirement_income = specifications$retirement_income[[i]],
      age_band = unique(ird$age_band),
      value = vapply(unique(ird$age_band), function(age_band) {
        part <- pooled[pooled$age_band == age_band, , drop = FALSE]
        nzs_weighted_quantile(part$income, part$weight, probability)
      }, numeric(1)), stringsAsFactors = FALSE
    )
  }))
  ages <- 65:100
  do.call(rbind, lapply(unique(anchors$retirement_income), function(archetype) {
    values <- setNames(anchors$value[anchors$retirement_income == archetype], anchors$age_band[anchors$retirement_income == archetype])
    data.frame(
      age = ages,
      retirement_income = archetype,
      source_variant = "ird_msd_fixed_mix_clipped",
      source_quantile = c(low = "p25", medium = "p50", high = "p75")[[archetype]],
      non_nzs_taxable_income_annual = pmax(0, ifelse(ages <= 72,
        values[["65-69"]] + (values[["70-74"]] - values[["65-69"]]) * (ages - 67) / 5,
        ifelse(ages < 75, values[["70-74"]] + (values[["75+"]] - values[["70-74"]]) * (ages - 72) / 3, values[["75+"]])
      )),
      profile_method = "linear_67_to_72_then_linear_72_to_75_flat_after_75",
      anchor_65_69 = values[["65-69"]],
      anchor_70_74 = values[["70-74"]],
      anchor_75_plus = values[["75+"]],
      stringsAsFactors = FALSE
    )
  }))
}

nzs_prepare_earnings_archetypes <- function(path) {
  raw <- utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  core <- raw[
    raw$Year == 2025 & raw$Occupation == "Total All Occupations" & raw$Sex == "Total Both Sexes" &
      raw[["Ethnic Group"]] == "Total Ethnic Groups" & raw[["Age Group"]] != "Total Age Groups",
    , drop = FALSE
  ]
  age_order <- c("15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64")
  midpoint <- c("15 to 19" = 17, "20 to 24" = 22, "25 to 29" = 27, "30 to 34" = 32, "35 to 39" = 37, "40 to 44" = 42, "45 to 49" = 47, "50 to 54" = 52, "55 to 59" = 57, "60 to 64" = 62)
  points <- core[core[["Age Group"]] %in% age_order, , drop = FALSE]
  measure_values <- function(measure) {
    selected <- points[points$MEASURE_INC_INC_004 == measure, c("Age Group", "OBS_VALUE")]
    selected <- selected[match(age_order, selected[["Age Group"]]), , drop = FALSE]
    as.numeric(selected$OBS_VALUE)
  }
  median_source <- measure_values("MED_WEEK_INC")
  average_source <- measure_values("AV_WEEK_INC")
  people_source <- measure_values("NO_PEOPLE")
  ages <- 15:64
  median_weekly_linear <- stats::approx(unname(midpoint), median_source, xout = ages, rule = 2)$y
  median_weekly_cubic <- stats::splinefun(unname(midpoint), median_source, method = "natural")(ages)
  median_weekly_cubic[ages < min(midpoint)] <- median_source[[1]]
  median_weekly_cubic[ages > max(midpoint)] <- median_source[[length(median_source)]]
  source_band <- age_order[pmin(10L, pmax(1L, floor((ages - 15) / 5) + 1L))]
  source_index <- match(source_band, age_order)
  out <- data.frame(
    age = ages,
    source_age_band = source_band,
    source_band_median_weekly = median_source[source_index],
    source_band_average_weekly = average_source[source_index],
    source_band_people_000 = people_source[source_index],
    median_weekly_linear = median_weekly_linear,
    median_weekly_cubic = median_weekly_cubic,
    lower_weekly_linear = median_weekly_linear * 2 / 3,
    upper_weekly_linear = median_weekly_linear * 1.5,
    lower_weekly_cubic = median_weekly_cubic * 2 / 3,
    upper_weekly_cubic = median_weekly_cubic * 1.5,
    median_annual_linear = median_weekly_linear * 52,
    median_annual_cubic = median_weekly_cubic * 52,
    lower_annual_linear = median_weekly_linear * 2 / 3 * 52,
    upper_annual_linear = median_weekly_linear * 1.5 * 52,
    lower_annual_cubic = median_weekly_cubic * 2 / 3 * 52,
    upper_annual_cubic = median_weekly_cubic * 1.5 * 52,
    core_kiwisaver_age = ifelse(ages >= 20 & ages <= 64, "True", "False"),
    stringsAsFactors = FALSE
  )
  numeric_columns <- vapply(out, is.numeric, logical(1))
  out[numeric_columns] <- lapply(out[numeric_columns], function(values) round(values, 2))
  out
}

nzs_prepare_total_survival <- function(path, population) {
  read_px <- function(sheet, sex) {
    raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
    data.frame(
      age = suppressWarnings(as.integer(raw[[1]][14:nrow(raw)])),
      sex = sex,
      px = suppressWarnings(as.numeric(raw[[12]][14:nrow(raw)]))
    )
  }
  px <- rbind(read_px("Table 7", "male"), read_px("Table 8", "female"))
  px <- px[!is.na(px$age) & !is.na(px$px), , drop = FALSE]
  survival <- do.call(rbind, lapply(split(px, px$sex), function(part) {
    part <- part[order(part$age), ]
    ages <- 65:105
    data.frame(age = ages, sex = part$sex[[1]], survival_probability = c(1, cumprod(part$px[match(65:104, part$age)])))
  }))
  weights <- population[population$year == 2026 & population$age == 65, c("sex", "population")]
  weights$share <- weights$population / sum(weights$population)
  merged <- merge(survival, weights[, c("sex", "share")], by = "sex")
  total <- stats::aggregate(I(survival_probability * share) ~ age, merged, sum)
  names(total)[[2]] <- "survival_probability"
  transform(total, sex_basis = "total_2026_age65_weighted")[, c("age", "sex_basis", "survival_probability")]
}

nzs_public_benefit_assumptions <- function() {
  # These are reviewed calibration values from the MSD/Work and Income sources
  # in the manifest. The source publications are not machine-readable enough to
  # support a reliable automatic extract, so each refresh requires review.
  data.frame(
    parameter = c("public_main_benefit_takeup", "public_jss_mix_share", "public_slp_mix_share", "public_eb_mix_share", "public_sps_mix_share", "public_jss_benefit_to_nzs", "public_slp_benefit_to_nzs", "public_eb_benefit_to_nzs", "public_sps_benefit_to_nzs", "public_as_delta_takeup", "public_as_amount_to_nzs", "public_da_delta_takeup", "public_da_amount_to_nzs", "public_tas_delta_takeup", "public_tas_amount_to_nzs"),
    value = c(0.15, 0.43588199879591, 0.48705599036725, 0.07304836443909, 0.00401364639775, 0.658232540896, 0.755688400760, 0.658232540896, 0.937161128875, 0.09, 0.15148132793, 0.02, 0.04464712823, 0.04, 0.17221035176),
    source = c("MSD OIA NZS transfer flow and 55-64 stock cross-check", rep("MSD OIA main-benefit-to-NZS transfers, 2024 and H1 2025", 4), rep("Work and Income gross weekly rates, 1 April 2026", 4), "Judgement retained pending better marginal AS take-up evidence", "MSD RRIP background paper average AS payment, 2025 / 2025 NZS SLA gross rate", "Judgement retained pending better marginal DA take-up evidence", "MSD RRIP background paper average DA payment, 2025 / 2025 NZS SLA gross rate", "Judgement retained pending better marginal TAS take-up evidence", "MSD RRIP background paper average TAS payment, 2025 / 2025 NZS SLA gross rate"),
    stringsAsFactors = FALSE
  )
}

nzs_prepare_public_inputs <- function(source_dir, output_dir) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required for a source refresh.", call. = FALSE)
  sources <- nzs_require_public_sources(source_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ltfm <- nzs_prepare_ltfm_inputs(sources[["ltfm"]])
  base_gross_aotwe_weekly <- nzs_read_base_gross_aotwe(sources[["qes"]])
  ird <- nzs_read_ird_income_distribution(sources[["ird_income"]])
  categories <- nzs_public_payment_categories()
  shares <- nzs_rake_payment_category_shares(nzs_household_proxy_shares(sources[["household"]]), categories)
  income_histogram <- nzs_prepare_income_histogram(ird, shares)
  income_histogram <- income_histogram[order(income_histogram$age_from, income_histogram$living_arrangement, income_histogram$income_lower), ]
  row.names(income_histogram) <- NULL
  outputs <- list(
    "ltfm-projections.csv" = ltfm$projections,
    "fiscal-baseline.csv" = ltfm$baseline,
    "population-by-age-sex.csv" = ltfm$population,
    "nzs-rate-paths-by-category.csv" = nzs_prepare_rate_paths_by_category(ltfm$projections, base_gross_aotwe_weekly),
    "income-histogram.csv" = income_histogram,
    "kiwisaver-earnings-archetypes.csv" = nzs_prepare_earnings_archetypes(sources[["hlfs"]]),
    "kiwisaver-retirement-income.csv" = nzs_prepare_retirement_income_profiles(ird, categories),
    "kiwisaver-survival-total.csv" = nzs_prepare_total_survival(sources[["life_tables"]], ltfm$population),
    "benefit-assumptions.csv" = nzs_public_benefit_assumptions()
  )
  for (name in names(outputs)) utils::write.csv(outputs[[name]], file.path(output_dir, name), row.names = FALSE)
  invisible(outputs)
}
