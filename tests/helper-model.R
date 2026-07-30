nzs_test_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(path, "DESCRIPTION"))) {
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate project root from test working directory.")
    }
    path <- parent
  }
  path
}

nzs_source_model <- function() {
  project_root <- nzs_test_project_root()
  files <- c(
    "model_constants.R",
    "parameters.R",
    "data_paths.R",
    "nzs_rates.R",
    "fiscal_engine.R",
    "person_impacts.R",
    "cohort_transition.R",
    "export_results.R",
    "export_workbook.R"
  )
  for (file in files) {
    source(file.path(project_root, "app/R/model", file))
  }
  invisible(project_root)
}
