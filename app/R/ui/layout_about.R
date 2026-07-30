about_feature_card <- function(title, text) {
  shiny::tags$div(
    class = "hw-about-card",
    shiny::tags$div(class = "hw-about-card-title", title),
    shiny::tags$p(class = "hw-about-card-text", text)
  )
}

about_data_source_row <- function(source, used_for, update_note) {
  shiny::tags$tr(
    shiny::tags$td(source),
    shiny::tags$td(used_for),
    shiny::tags$td(update_note)
  )
}

about_assumption_row <- function(topic, headline, detail) {
  shiny::tags$tr(
    shiny::tags$td(topic),
    shiny::tags$td(headline),
    shiny::tags$td(detail)
  )
}

about_detail <- function(title, ...) {
  shiny::tags$details(
    class = "hw-about-detail",
    shiny::tags$summary(title),
    shiny::tags$div(class = "hw-about-detail-body", ...)
  )
}

about_view <- function() {
  source_link <- function(label, href) {
    shiny::tags$a(label, href = href, target = "_blank", rel = "noopener noreferrer")
  }

  shiny::tagList(
    hw_section(
      "ABOUT THIS MODEL",
      shiny::tags$div(
        class = "hw-about-hero",
        shiny::tags$p(
          class = "hw-about-copy",
          "The NZ retirement policy model is a scenario tool for exploring selected NZ Super reform settings and their fiscal, cohort, and illustrative person-level impacts."
        ),
        shiny::tags$p(
          class = "hw-about-copy",
          "The project was commissioned by Te Ara Ahunga Ora Retirement Commission and developed by Heuser | Whittington as part of the Commission's empirical modelling of retirement income policy changes."
        ),
        shiny::tags$p(
          class = "hw-about-status",
          "Note: the model is currently marked as a prototype. The model outputs support policy exploration and discussion. They should not be taken as an official government forecast, costing, or policy position."
        ),
        shiny::tags$p(
          class = "hw-about-copy hw-about-browse-note",
          "Browse the sections below for a quick guide to what the model shows, the core assumptions, and the source data behind it."
        )
      ),
      shiny::tags$div(
        class = "hw-about-detail-stack hw-about-detail-stack-contained",
      about_detail(
        "What each tab answers",
        shiny::tags$div(
          class = "hw-about-card-stack hw-about-card-row",
          about_feature_card(
            "Fiscal impacts",
            "Shows the aggregate fiscal cost path, net savings, and policy-channel decomposition for the selected reform."
          ),
          about_feature_card(
            "Cohort impacts",
            "Shows how selected reforms affect different birth cohorts and income groups."
          ),
          about_feature_card(
            "Person impacts",
            "Shows an illustrative person/archetype view, including KiwiSaver contribution settings that would offset modelled NZS reform losses."
          )
        )
      ),
      about_detail(
        "Key assumptions",
        shiny::tags$table(
          class = "hw-about-source-table hw-about-assumption-table",
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th("Topic"),
              shiny::tags$th("Headline setting"),
              shiny::tags$th("Detail")
            )
          ),
          shiny::tags$tbody(
            about_assumption_row(
              "Baseline fiscal costs of NZS",
              "Treasury 2025 Long-Term Fiscal Model projection",
              "Uses the 2025 LTFM workbook for the baseline fiscal costs of NZS, as well as long-term projection paths for GDP, CPI, wage-growth and population."
            ),
            about_assumption_row(
              "Macro assumptions",
              shiny::tagList(
                "Long-run CPI growth 2.0%;",
                shiny::tags$br(),
                "Long-run wage growth 2.9%"
              ),
              "Terminal projection rates in the 2025 LTFM."
            ),
            about_assumption_row(
              "Discounting",
              "4.3% nominal default",
              "Inherited from the LTFM long-run Government 10-year bond annual rate of return assumption."
            ),
            about_assumption_row(
              "NZ Super rates",
              "Published 2026 anchor; LTFM wage path",
              "The 2026 net-AOTWE benchmark is calculated from QES earnings, standard tax and ACC. It then grows with LTFM wage growth and the statutory CPI corridor determines the current-formula path."
            ),
            about_assumption_row(
              "Income testing",
              "Non-NZS taxable retirement income",
              "Income-test inputs for both the fiscal and person-level modelling are constructed from public IRD/MSD/Stats NZ sources."
            ),
            about_assumption_row(
              "Survival",
              "Stats NZ Total-population life tables",
              "Person and cohort present-value calculations use total-population survival probabilities."
            ),
            about_assumption_row(
              "KiwiSaver returns",
              "1.5% to 5.5% nominal net",
              "Fund returns based on Financial Markets Conduct Regulations 2014: Defensive 1.5%, Conservative 2.5%, Balanced 3.5%, Growth 4.5%, Aggressive 5.5%."
            ),
            about_assumption_row(
              "KiwiSaver contributions",
              "ESCT and government contribution rules included",
              "Employer contributions are net of ESCT. Government contributions follow the current capped matching rules, with the cap and income threshold held fixed nominally. The status quo comparison uses the enacted 3.5%-then-4.0% default contribution schedule."
            )
          )
        )
      ),
      about_detail(
        "Model scope and interpretation",
        shiny::tags$p(
          class = "hw-about-narrative",
          "The model is designed for scenario analysis. It compares selected NZ Super reform settings against a status quo path and shows the results through three lenses: aggregate fiscal impacts, cohort and income-group impacts, and illustrative person-level impacts."
        ),
        shiny::tags$p(
          class = "hw-about-narrative",
          "The results should be read as modelled implications of the selected assumptions, not as forecasts of future policy, behavioural responses, or individual outcomes. Each analytical tab includes modelling notes that explain the main calculation choices and caveats for that view."
        )
      ),
      about_detail(
        "Data sources and update path",
        shiny::tags$table(
          class = "hw-about-source-table",
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th("Source"),
              shiny::tags$th("Used for"),
              shiny::tags$th("Update pathway")
            )
          ),
          shiny::tags$tbody(
            about_data_source_row(
              source_link("Treasury Long-term Fiscal Model", "https://www.treasury.govt.nz/publications/ltfm/long-term-fiscal-model-he-tirohanga-mokopuna-2025"),
              "Fiscal baseline, GDP, CPI and wage-growth paths",
              "Periodic Treasury release rather than annual; review when a new LTFM is published, or when BEFU or HYEFU macro assumptions materially change."
            ),
            about_data_source_row(
              source_link("Stats NZ population projections", "https://www.stats.govt.nz/information-releases/national-population-projections-2024base2078/"),
              "Projected population by age and sex",
              "Periodic projection-base release; can be refreshed when Stats NZ publishes a new national population projection vintage."
            ),
            about_data_source_row(
              source_link("IRD taxable-income distributions", "https://www.ird.govt.nz/about-us/tax-statistics/revenue-refunds/income-distribution/tax-on-taxable-income-datasets"),
              "Non-NZS taxable-income distributions for income testing",
              "Annual administrative series; can be refreshed when IRD publishes the next taxable-income distribution workbook."
            ),
            about_data_source_row(
              source_link("MSD NZS and benefit data", "https://www.msd.govt.nz/about-msd-and-our-work/publications-resources/statistics/benefit/index.html"),
              "NZS payment mix and replacement-benefit offset assumptions",
              "Quarterly benefit fact sheets; can be refreshed from the latest March, June, September, or December quarter tables. OIA inputs will require a fresh information request to MSD."
            ),
            about_data_source_row(
              source_link("Stats NZ household projections", "https://www.stats.govt.nz/information-releases/family-and-household-projections-2018base-2043/"),
              "Age pattern of NZS payment-category mix",
              "Periodic projection release; can be refreshed when Stats NZ publishes a new family and household projection base."
            ),
            about_data_source_row(
              source_link("Stats NZ earnings", "https://explore.data.stats.govt.nz/"),
              "Working-life earnings archetypes",
              "Annual HLFS income series; can be refreshed when the next June-year earnings release is available in Aotearoa Data Explorer."
            ),
            about_data_source_row(
              source_link("Stats NZ life tables", "https://www.stats.govt.nz/information-releases/national-and-subnational-period-life-tables-2022-2024/"),
              "Survival-weighted cohort and person impacts",
              "Periodic three-year life-table release; can be refreshed when a newer complete period life-table workbook is published."
            )
          )
        )
      )
      ),
      shiny::tags$p(
        class = "hw-panel-note hw-about-closing-note",
        "Use the analytical tabs to explore scenarios, then use Export results to download the selected settings and chart-ready outputs for the current scenario. Full methodology and update instructions are documented separately in the technical documentation."
      ),
      class = "hw-about-hero-section",
      subtitle = "Purpose, status, assumptions, and sources"
    )
  )
}

