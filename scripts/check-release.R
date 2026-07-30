if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the repository root.", call. = FALSE)
}

source("tests/helper-model.R")
nzs_source_model()
source("tests/release-fixtures.R")

testthat::test_dir("tests", reporter = "summary", stop_on_failure = TRUE)

app_environment <- new.env(parent = globalenv())
original_directory <- getwd()
on.exit(setwd(original_directory), add = TRUE)
setwd("app")
source("app.R", local = app_environment)

if (!is.function(app_environment$server) || is.null(app_environment$ui)) {
  stop("App entry point did not create a Shiny UI and server.", call. = FALSE)
}
setwd(original_directory)

public_source_dir <- Sys.getenv("NZS_PUBLIC_SOURCE_DIR", unset = "")
if (nzchar(public_source_dir)) {
  source("scripts/prepare-app-data.R")
  refresh_dir <- tempfile("nzs-public-refresh-")
  prepare_app_data(public_source_dir, refresh_dir)
  shipped_files <- sort(list.files("app/data", pattern = "\\.csv$"))
  refreshed_files <- sort(list.files(refresh_dir, pattern = "\\.csv$"))
  if (!identical(shipped_files, refreshed_files)) {
    stop("Public-source refresh did not produce the shipped app-data file set.", call. = FALSE)
  }
  for (file in shipped_files) {
    shipped <- utils::read.csv(file.path("app/data", file), check.names = FALSE)
    refreshed <- utils::read.csv(file.path(refresh_dir, file), check.names = FALSE)
    if (!isTRUE(all.equal(shipped, refreshed, check.attributes = FALSE))) {
      stop("Public-source refresh does not reproduce app/data/", file, ".", call. = FALSE)
    }
  }
  cat("\nPublic-data refresh reconciles to shipped prepared data files.\n")
} else {
  cat("\nPublic-source refresh reconciliation skipped (set NZS_PUBLIC_SOURCE_DIR to run it).\n")
}

cat("\nApproved release fixtures:\n")
print(nzs_release_fixture_results(), row.names = FALSE)
cat("\nModel tests and app-load check passed.\n")
