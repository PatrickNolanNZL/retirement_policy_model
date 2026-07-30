app_title <- "NZ retirement policy model"
app_dir <- "app"
site_dir <- "site"
favicon_name <- "hw-favicon.ico"
favicon_path <- file.path(app_dir, "www", favicon_name)

if (!file.exists(file.path(app_dir, "app.R"))) {
  stop("Run this script from the repository root.", call. = FALSE)
}

if (!file.exists(favicon_path)) {
  stop("Missing favicon asset: ", favicon_path, call. = FALSE)
}

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("Package 'shinylive' is required. Run renv::restore() before exporting.", call. = FALSE)
}

if (dir.exists(site_dir)) {
  unlink(site_dir, recursive = TRUE, force = TRUE)
}

shinylive::export(
  app_dir,
  site_dir,
  template_params = list(
    title = app_title,
    include_in_head = paste0(
      '<link rel="icon" type="image/x-icon" href="./',
      favicon_name,
      '" />',
      "\n",
      '<link rel="shortcut icon" type="image/x-icon" href="./',
      favicon_name,
      '" />'
    )
  )
)

file.copy(favicon_path, file.path(site_dir, favicon_name), overwrite = TRUE)

index_path <- file.path(site_dir, "index.html")
index_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")

if (!grepl(paste0("<title>", app_title, "</title>"), index_html, fixed = TRUE)) {
  index_html <- sub("<title>.*</title>", paste0("<title>", app_title, "</title>"), index_html)
  writeLines(index_html, index_path, useBytes = TRUE)
}

index_html <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
if (!grepl(paste0("<title>", app_title, "</title>"), index_html, fixed = TRUE)) {
  stop("Shinylive export did not write the expected page title.")
}
if (!file.exists(file.path(site_dir, favicon_name))) {
  stop("Shinylive export did not copy the favicon.")
}

message("Wrote disposable static build to ", normalizePath(site_dir, winslash = "/", mustWork = TRUE))
