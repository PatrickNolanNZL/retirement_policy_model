nzs_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      return(NULL)
    }
    path <- parent
  }
}

nzs_app_data_file <- function(file_name) {
  project_root <- nzs_project_root()
  candidates <- c(
    file.path("data", file_name),
    file.path("app", "data", file_name),
    if (!is.null(project_root)) file.path(project_root, "app", "data", file_name)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("Could not find app data file: ", file_name, call. = FALSE)
  }
  existing[[1]]
}

nzs_load_ltfm_projections <- function(file_name = "ltfm-projections.csv") {
  utils::read.csv(nzs_app_data_file(file_name), check.names = FALSE)
}

nzs_economic_path_from_ltfm <- function(
    projections = nzs_load_ltfm_projections(),
    defaults = nzs_default_inputs()) {
  required <- c("year", "ltfm_cpi_growth_projection", "ltfm_wage_growth_projection")
  missing <- setdiff(required, names(projections))
  if (length(missing) > 0) {
    stop("Missing LTFM projection columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  period <- projections$year - min(projections$year)
  data.frame(
    age = defaults$start_age + period,
    year = projections$year,
    period = period,
    ltfm_cpi_growth_projection = projections$ltfm_cpi_growth_projection,
    ltfm_wage_growth_projection = projections$ltfm_wage_growth_projection
  )
}
