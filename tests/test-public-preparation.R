source("helper-model.R")
project_root <- nzs_source_model()
source(file.path(project_root, "scripts/extract-ltfm-path.R"))
source(file.path(project_root, "scripts/prepare_public_inputs.R"))

testthat::test_that("public-source preparation parses IRD bands and validates source folders", {
  testthat::expect_equal(nzs_parse_income_band("10,000 - 15,000"), c(lower = 10000, upper = 15000, midpoint = 12500))
  testthat::expect_equal(nzs_parse_income_band("Over $250,000"), c(lower = 250000, upper = Inf, midpoint = 250000))
  testthat::expect_error(nzs_require_public_sources(tempdir()), "Missing required public source files")
})

testthat::test_that("payment-category raking preserves age totals and matches the MSD mix", {
  proxy <- data.frame(
    age_band = rep(c("65-69", "70-74", "75+"), each = 3),
    payment_category = rep(c("couple", "single_sharing", "single_living_alone"), 3),
    population = c(55, 10, 35, 50, 15, 35, 40, 20, 40),
    population_total = rep(c(100, 100, 100), each = 3)
  )
  proxy$share <- proxy$population / proxy$population_total
  shares <- nzs_rake_payment_category_shares(proxy)
  testthat::expect_equal(
    stats::aggregate(payment_category_share ~ age_band, shares, sum)$payment_category_share,
    rep(1, 3), tolerance = 1e-10
  )
  totals <- stats::aggregate(payment_category_share * population_total ~ payment_category, shares, sum)
  target <- nzs_public_payment_categories()
  totals <- merge(totals, target[, c("payment_category", "msd_share")], by = "payment_category")
  testthat::expect_equal(totals[["payment_category_share * population_total"]] / sum(totals[["payment_category_share * population_total"]]), totals$msd_share, tolerance = 1e-10)
})

testthat::test_that("prepared public-source income histograms are valid distributions", {
  ird <- data.frame(
    age_band = rep(c("65-69", "70-74", "75+"), each = 2),
    taxable_midpoint = rep(c(10000, 50000), 3),
    count = rep(c(80, 20), 3)
  )
  shares <- expand.grid(age_band = c("65-69", "70-74", "75+"), payment_category = c("couple", "single_sharing", "single_living_alone"), stringsAsFactors = FALSE)
  categories <- nzs_public_payment_categories()
  shares <- merge(shares, categories[, c("payment_category", "living_arrangement", "gross_nzs_rate")], by = "payment_category")
  shares$payment_category_share <- 1 / 3
  histogram <- nzs_prepare_income_histogram(ird, shares)
  total <- stats::aggregate(weighted_n ~ age_band, histogram, sum)
  testthat::expect_equal(total$weighted_n, rep(1, 3), tolerance = 1e-10)
  testthat::expect_true(all(histogram$weighted_mean_income >= 0))
})

testthat::test_that("release boundary contains no active Python, TAWAlite, Stage 1, or Pages workflow", {
  root <- nzs_test_project_root()
  release_files <- c(
    list.files(file.path(root, "app"), recursive = TRUE, full.names = TRUE),
    list.files(file.path(root, "scripts"), recursive = TRUE, full.names = TRUE),
    list.files(file.path(root, "docs"), recursive = TRUE, full.names = TRUE)
  )
  release_files <- release_files[file.info(release_files)$isdir %in% FALSE]
  testthat::expect_false(any(grepl("\\.py$", release_files, ignore.case = TRUE)))
  testthat::expect_false(file.exists(file.path(root, ".github", "workflows", "deploy-pages.yml")))
  text <- paste(unlist(lapply(release_files[grepl("\\.(R|md|csv)$", release_files, ignore.case = TRUE)], readLines, warn = FALSE)), collapse = "\n")
  testthat::expect_false(grepl("TAWAlite|Stage 1|stage 1", text, ignore.case = TRUE))
})
