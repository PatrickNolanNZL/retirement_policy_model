cohort_impacts_view <- function() {
  shiny::tagList(
    shiny::tags$div(
      class = "hw-cohort-transition-grid",
      hw_section(
        "02 / TRANSITION IMPACT BY COHORT",
        shiny::plotOutput("cohort_transition_plot", height = "380px"),
        shiny::tags$div(
          class = "hw-note-with-info",
          shiny::tags$p(
            class = "hw-panel-note",
            "Each bar shows the share of future NZ Super a birth cohort loses under the selected reform - before income testing is applied. Income testing incidence is shown separately below."
          ),
          hw_info_popover(
            "Calculated as the present value of future NZS payments under the reform (excluding income testing), divided by the present value of future NZS payments under the status quo. Values are survival-weighted and expressed in real 2026 terms."
          )
        ),
        class = "hw-chart-section",
        subtitle = "Share of NZ Super lost before income testing is applied"
      )
    ),
    shiny::tags$div(
      class = "hw-two-chart-grid",
      hw_section(
        "03 / ELIGIBILITY DELAY BY COHORT",
        shiny::plotOutput("cohort_deferral_plot", height = "320px"),
        shiny::tags$p(
          class = "hw-panel-note",
          "Shows how many years of NZ Super access are delayed for birth cohorts around the selected eligibility-age reform."
        ),
        class = "hw-chart-section",
        subtitle = "Years of NZ Super access deferred by the eligibility-age reform"
      ),
      hw_section(
        "04 / INCOME-TEST INCIDENCE BY INCOME GROUP",
        shiny::tags$div(
          class = "hw-inline-select",
          shiny::selectInput(
            "cohort_income_test_metric",
            "Measure",
            choices = c(
              "People affected" = "people",
              "Share of 65+ population" = "share",
              "% loss of status quo payment" = "percent_loss"
            ),
            selected = "percent_loss"
          )
        ),
        shiny::plotOutput("cohort_income_test_incidence_plot", height = "320px"),
        shiny::tags$div(
          class = "hw-note-with-info",
          shiny::tags$p(
            class = "hw-panel-note",
            "Illustrates how the income test would apply across income groups for current retirees within the selected age window."
          ),
          hw_info_popover(
            shiny::tagList(
              shiny::tags$p(
                "Based on non-NZS taxable income for people aged 65+ constructed from IRD, MSD, and Stats NZ data. See model documentation for more details."
              ),
              shiny::tags$ul(
                shiny::tags$li("Applies the selected income-test threshold, abatement rate, and age window to current retirees."),
                shiny::tags$li("Does not apply policy start-year or grandparenting timing."),
                shiny::tags$li("Uses weighted mean income within each displayed income band.")
              )
            )
          )
        ),
        class = "hw-chart-section",
        subtitle = "Current-year illustration for people aged 65+"
      )
    ),
    hw_section(
      "05 / MODELLING NOTES",
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$strong("Purpose. "),
          "This tab is designed to show who is affected by the selected reform, rather than to reconcile aggregate fiscal savings. Fiscal totals are shown in the Fiscal impacts tab."
        ),
        shiny::tags$li(
          shiny::tags$strong("Cohort perspective. "),
          "Birth cohorts are read from the current model year perspective, so each bar represents people born in that year facing the future policy settings selected in the controls."
        ),
        shiny::tags$li(
          shiny::tags$strong("Income testing. "),
          "The lifetime transition chart excludes income testing by design, because income-test effects depend on retirement-income assumptions and are illustrated separately by income group."
        ),
        shiny::tags$li(
          shiny::tags$strong("Incidence snapshot. "),
          "The income-test incidence chart applies the selected test to current retirees in the selected age window; it is not a projected fiscal incidence path."
        )
      ),
      class = "hw-note-section"
    )
  )
}

