# Rebuild prepared model data files from an explicit public-data bundle.
#
# Usage from the repository root:
#   source("scripts/prepare-app-data.R")
#   prepare_app_data("C:/path/to/public-data-bundle")

source("app/R/model/model_constants.R")
source("app/R/model/parameters.R")
source("app/R/model/data_paths.R")
source("app/R/model/nzs_rates.R")
source("scripts/extract-ltfm-path.R")
source("scripts/prepare_public_inputs.R")

prepare_app_data <- function(source_dir, output_dir = "app/data") {
  nzs_prepare_public_inputs(
    source_dir = normalizePath(source_dir, winslash = "/", mustWork = TRUE),
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  )
  message("Prepared public-source model data files in ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
}
