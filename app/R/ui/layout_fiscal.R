fiscal_impacts_view <- function() {
  shiny::tagList(
    shiny::tags$div(
      class = "hw-fiscal-top-grid",
      hw_section(
        "02 / FISCAL COST PATH",
        shiny::tags$div(
          class = "hw-inline-radio",
          shiny::radioButtons(
            "fiscal_cost_path_units",
            "Display",
            choices = c("Nominal $bn" = "nominal", "% of GDP" = "gdp"),
            selected = "nominal",
            inline = TRUE
          )
        ),
        shiny::plotOutput("fiscal_cost_plot", height = "320px"),
        class = "hw-chart-section",
        subtitle = "Fiscal costs of NZ Super under the status quo and the reform"
      ),
      hw_section(
        "03 / HEADLINE SAVINGS",
        shiny::tags$div(
          class = "hw-headline-metrics",
          shiny::tags$div(
            class = "hw-kpi-card",
            shiny::tags$div(class = "hw-kpi-label", "Net fiscal saving NPV"),
            shiny::tags$div(class = "hw-kpi-value", shiny::textOutput("fiscal_npv_saving", inline = TRUE)),
            shiny::tags$div(class = "hw-kpi-sub", "discounted over the full projection period")
          ),
          shiny::tags$div(
            class = "hw-kpi-card",
            shiny::tags$div(class = "hw-kpi-label", "Average annual saving"),
            shiny::tags$div(class = "hw-kpi-value", shiny::textOutput("fiscal_average_saving", inline = TRUE)),
            shiny::tags$div(class = "hw-kpi-sub", "over the full projection period")
          )
        ),
        shiny::tags$p(
          class = "hw-panel-note",
          "NPVs are discounted to FY2026 using the selected nominal discount rate."
        ),
        class = "hw-summary-section hw-headline-section"
      )
    ),
    shiny::tags$div(
      class = "hw-fiscal-top-grid",
      hw_section(
        "04 / ANNUAL SAVINGS DECOMPOSITION",
        shiny::plotOutput("fiscal_decomposition_plot", height = "300px"),
        shiny::tags$div(
          class = "hw-note-with-info",
          shiny::tags$p(
            class = "hw-panel-note",
            "Benefit takeup is shown as a negative offset to eligibility-age savings."
          ),
          hw_info_popover(
            "Savings are decomposed using the model ordering: eligibility age first, then indexation, then income testing. Component values should be read as an attribution of the combined package, not as standalone policy estimates."
          )
        ),
        class = "hw-chart-section",
        subtitle = "Annual savings attributed by policy channel"
      ),
      hw_section(
        "05 / NPV SAVING DECOMPOSITION",
        shiny::tableOutput("fiscal_headline_decomposition_table"),
        shiny::tags$p(
          class = "hw-panel-note",
          "NPVs are discounted to FY2026 using the selected nominal discount rate."
        ),
        class = "hw-summary-section"
      )
    ),
    hw_section(
      "06 / MODELLING NOTES",
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$strong("Purpose. "),
          "This tab shows the aggregate fiscal impacts of the selected reform package, including the status quo path, the reform path, annual savings and net present value (NPV) savings."
        ),
        shiny::tags$li(
          shiny::tags$strong("Policy ordering. "),
          "Eligibility-age changes are applied first, then indexation changes, then income testing. This avoids counting people excluded by an eligibility-age change as also receiving NZ Super subject to later reforms."
        ),
        shiny::tags$li(
          shiny::tags$strong("Decomposition. "),
          "Component savings are an attribution convention, not independent policy estimates. In particular, indexation changes alter the pre-test payment path, while income testing then applies to the remaining entitlement."
        ),
        shiny::tags$li(
          shiny::tags$strong("Income testing. "),
          "Fiscal income-test savings are based on non-NZS taxable income for people aged 65+ constructed from IRD, MSD, and Stats NZ data. Abatement is capped at the NZ Super payment for the relevant living arrangement."
        ),
        shiny::tags$li(
          shiny::tags$strong("Benefit takeup. "),
          "Replacement-benefit takeup is shown as an offset to eligibility-age savings only."
        )
      ),
      class = "hw-note-section"
    ),
    # Retained for post-demo iteration.
    # hw_section(
    #   "06 / ACTIVE ASSUMPTIONS",
    #   shiny::tableOutput("fiscal_assumptions"),
    #   class = "hw-summary-section"
    # )
  )
}

