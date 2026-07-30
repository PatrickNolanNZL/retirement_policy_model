# NZ retirement policy model

An R/Shiny scenario model for exploring selected changes to New Zealand Superannuation (NZ Super). It was commissioned by Te Ara Ahunga Ora Retirement Commission and developed by Heuser | Whittington.

The model supports policy exploration and discussion. It is not an official government forecast, costing, or policy position.

## What is included

- Fiscal impacts: aggregate NZ Super costs, annual saving decomposition, and net-present-value summaries.
- Cohort impacts: reform transition effects, eligibility-age deferral, and illustrative income-test incidence.
- Person impacts: illustrative working-life earnings, KiwiSaver accumulation, retirement-income composition, and contribution-rate sensitivity.
- An Excel export for the selected scenario.

## Run locally

From the repository root, restore the pinned R environment and start the model:

```r
renv::restore()
shiny::runApp("app")
```

Run the automated release check before accepting a code or prepared-data change:

```r
source("scripts/check-release.R")
```

The check runs the test suite, confirms that the Shiny entry point loads, and reports the approved numerical release fixtures.

## Build a static site

The model can be exported as a static Shinylive site for a hosting route chosen by the Retirement Commission:

```r
source("scripts/export-shinylive.R")
httpuv::runStaticServer("site")
```

`site/` is a disposable build output and is deliberately excluded from version control. This repository does not prescribe a deployment platform or include hosting automation.

## Repository structure

| Location | Purpose |
| --- | --- |
| `app/` | Shiny entry point, model/UI code, prepared data files, and static assets. |
| `app/R/model/` | Testable fiscal, cohort, person-impact, rate-path, and export logic. |
| `app/R/ui/` | Layout, theme, plotting, formatting, and display helpers. |
| `app/data/` | Compact prepared data files used by the released model. |
| `scripts/` | R scripts for validation, public-source preparation, and Shinylive export. |
| `tests/` | Unit, contract, and approved release-fixture tests. |
| `docs/` | Architecture note and public-source manifest. |

## Data refresh boundary

Normal model use requires only the prepared data files shipped in `app/data/`. Raw source files are deliberately outside Git and are not a deployment prerequisite.

A maintainer can rebuild prepared data files from the separately supplied public-data bundle:

```r
source("scripts/prepare-app-data.R")
prepare_app_data("C:/path/to/public-data-bundle")
```

The required source files, publisher links, data versions, refresh guidance, redistribution notes, checksums, and downstream prepared data files are listed in [`docs/public-source-manifest.csv`](docs/public-source-manifest.csv). To confirm a refresh exactly reproduces the shipped inputs, set `NZS_PUBLIC_SOURCE_DIR` and run `scripts/check-release.R`.

Detailed methodology, maintenance instructions, and end-user guidance are supplied as separate handover documents.
