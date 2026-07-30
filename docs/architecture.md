# App architecture

This note summarises the current structure of the NZ retirement policy model Shiny app. It is intended as a lightweight orientation note for maintainers, not as the final technical documentation.

## Current Analytical Layers

The app currently exposes three analytical tabs:

1. **Fiscal impacts**: aggregate NZ Super fiscal cost paths, savings decomposition, NZS expense as a share of GDP, and headline NPV savings.
2. **Cohort impacts**: cohort transition impacts, eligibility-age deferral effects, and current-year income-test incidence by income group.
3. **Person impacts**: illustrative person-level KiwiSaver offset modelling for wage/salary earnings and retirement-income archetypes.

The app also includes:

- a shared left-hand reform-control panel;
- an About tab;
- a global Excel export of the current scenario;
- Shinylive export support for static hosting.

## Runtime Structure

The app entry point is `app/app.R`. It sources model and UI modules in this order:

| Area | Files | Purpose |
|---|---|---|
| Parameters and paths | `R/model/parameters.R`, `R/model/data_paths.R` | Defaults, control choices, app-data lookup, and data loading helpers. |
| NZ Super rates | `R/model/nzs_rates.R` | Published-rate-anchored NZS paths under the supported indexation regimes. |
| Fiscal engine | `R/model/fiscal_engine.R` | Eligibility-age, indexation, income-testing, benefit-offset, summary, and fiscal incidence calculations. |
| Person impacts | `R/model/person_impacts.R` | KiwiSaver accumulation, ESCT, government contributions, retirement drawdown, survival weighting, and required matched-rate calculations. |
| Cohort impacts | `R/model/cohort_transition.R` | Birth-cohort transition path, eligibility-age deferral, and cohort-level policy-impact calculations. |
| Export | `R/model/export_results.R`, `R/model/export_workbook.R` | Excel export assembly and workbook rendering. |
| UI | `R/ui/theme.R`, `R/ui/formatting.R`, `R/ui/plotting_*.R`, `R/ui/layout_*.R` | H|W styling, display formatting, charts, controls, panels, notes, and tab layout, separated by analytical tab. |

## Data Handling

Source data stays outside the model repository. The repository contains compact prepared data files under `app/data/`.

The main data-preparation entry point is:

```text
source("scripts/prepare-app-data.R")
prepare_app_data("C:/path/to/public-data-bundle")
```

See `app/data/README.md` for the current prepared-data inventory, source versions, and update guidance.

## Local Development And Export

Typical local workflow:

```text
renv::restore()
testthat::test_dir("tests")
shiny::runApp("app")
```

The static Shinylive build should be generated through:

```text
source("scripts/export-shinylive.R")
```

This wrapper preserves the app title, favicon, and other static-site metadata that are not reliably retained by a bare `shinylive::export()` call. It produces a disposable `site/` directory; the repository deliberately does not include hosting automation.

## Testing

The active test suite covers:

- NZS rate-path calculations;
- prepared-data contracts and public-data boundaries;
- fiscal engine logic and interactions;
- cohort-transition calculations;
- Person impacts calculations;
- formatting helpers;
- Excel export assembly and workbook writing.

Three named release fixtures provide a compact numerical control for an eligibility-age-only reform, a CPI-indexation-only reform, and a combined reform. A changed fixture value requires a deliberate re-baseline following an approved data or methodology change.

Run the model tests with:

```text
source("tests/helper-model.R")
testthat::test_dir("tests")
```

Run the non-mutating release check with:

```text
source("scripts/check-release.R")
```

It runs the suite, verifies the app entry point loads, and reports the approved release fixtures. Run `source("scripts/export-shinylive.R")` separately before release to complete the static-export smoke check.

## Release boundary

The release repository contains R source, prepared public-source data files, tests, and documentation. It excludes raw sources, restricted material, historical comparators, generated static output, and deployment automation. A separate public-data bundle can be supplied for a full data refresh using `scripts/prepare-app-data.R`.
