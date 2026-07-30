person_impacts_view <- function() {
  shiny::tagList(
    shiny::tags$div(
      class = "hw-two-chart-grid",
      hw_section(
        "03 / WORKING-LIFE EARNINGS AND CONTRIBUTIONS",
        shiny::plotOutput("person_working_life_plot", height = "320px"),
        shiny::tags$div(
          class = "hw-note-with-info",
          shiny::tags$p(
            class = "hw-panel-note",
            "Active modelling starts at the selected current age. The lighter earlier-age line is shown as context for the earnings profile."
          ),
          hw_info_popover(
            shiny::tagList(
              shiny::tags$p(
                "Working-life earnings are based on Stats NZ HLFS wage/salary earnings by age. The cross-sectional age profile is used as a stylised lifetime profile."
              ),
              shiny::tags$ul(
                shiny::tags$li("The median profile follows the Stats NZ age profile."),
                shiny::tags$li("Low and high profiles use 2/3 and 3/2 of median earnings, matching OECD-style low/high pay thresholds."),
                shiny::tags$li("Employee contributions are shown within gross earnings; employer contributions are shown net of ESCT above gross earnings.")
              )
            )
          )
        ),
        class = "hw-chart-section",
        subtitle = "Real wage/salary profile and KiwiSaver contribution flows"
      ),
      hw_section(
        "04 / KIWISAVER BALANCE PATH",
        shiny::plotOutput("person_balance_plot", height = "320px"),
        shiny::tags$div(
          class = "hw-note-with-info",
          shiny::tags$p(
            class = "hw-panel-note",
            "Projection starts from the selected current age and current KiwiSaver balance; no earlier balance path is inferred."
          ),
          hw_info_popover(
            "The comparison line is the age-65 balance that would offset the expected NZS reform loss, using survival probabilities and the selected post-65 nominal return."
          )
        ),
        class = "hw-chart-section",
        subtitle = "Projected balance to age 65 compared with the balance that would offset NZS reform"
      )
    ),
    shiny::tags$div(
      class = "hw-fiscal-top-grid",
      hw_section(
        "05 / RETIREMENT INCOME COMPOSITION",
        shiny::plotOutput("person_retirement_income_plot", height = "320px"),
        shiny::tags$div(
          class = "hw-note-with-info",
          shiny::tags$p(
            class = "hw-panel-note",
            "The stacked areas show income under the reform. The status quo line uses the same retirement income, KiwiSaver starting balance and fund return assumptions, but applies the enacted default KiwiSaver schedule: 3.5% employee and employer contributions in 2026-27, rising to 4.0% from 2028. It is a statutory baseline comparison, rather than an NZS-only counterfactual to the selected saving path."
          ),
          hw_info_popover(
            shiny::tagList(
              shiny::tags$p(
                "This chart is an illustrative retirement-income composition view, not a full retirement-adequacy benchmark."
              ),
              shiny::tags$ul(
                shiny::tags$li("KiwiSaver drawdown is a constant real annual amount supported by the projected age-65 balance."),
                shiny::tags$li("KiwiSaver drawdown is not included in the income-test base."),
                shiny::tags$li("The chart is clipped at age 90 for readability; calculations use the full survival table.")
              )
            )
          )
        ),
        class = "hw-chart-section",
        subtitle = "Illustrative annual retirement income under the selected saving path"
      ),
      hw_section(
        "06 / REPLACEMENT RATE",
        shiny::tableOutput("person_replacement_table"),
        shiny::tags$p(
          class = "hw-panel-note",
          "Average annual real income at ages 65-69 divided by real wage/salary earnings at age 64. This is an illustrative replacement-rate measure, not a full adequacy benchmark."
        ),
        class = "hw-summary-section",
        subtitle = "Retirement income as a share of age-64 earnings"
      )
    ),
    shiny::tags$div(
      class = "hw-offset-sensitivity-grid",
      hw_section(
        "07 / NZS POLICY REFORM OFFSET",
        shiny::tags$div(
          class = "hw-person-kpi-grid hw-person-kpi-grid-compact",
          shiny::tags$div(
            class = "hw-kpi-card",
            shiny::tags$div(class = "hw-kpi-label", "KiwiSaver balance at 65"),
            shiny::tags$div(class = "hw-kpi-value", shiny::textOutput("person_projected_balance", inline = TRUE)),
            shiny::tags$div(class = "hw-kpi-sub", "under selected saving settings, real 2026 dollars")
          ),
          shiny::tags$div(
            class = "hw-kpi-card",
            shiny::tags$div(class = "hw-kpi-label", "Age-65 balance to offset NZS reform"),
            shiny::tags$div(class = "hw-kpi-value", shiny::textOutput("person_target_balance", inline = TRUE)),
            shiny::tags$div(class = "hw-kpi-sub", "real 2026 dollars")
          ),
          shiny::tags$div(
            class = "hw-kpi-card",
            shiny::tags$div(class = "hw-kpi-label", "KiwiSaver surplus / shortfall"),
            shiny::tags$div(class = "hw-kpi-value hw-kpi-value-accent", shiny::textOutput("person_shortfall", inline = TRUE)),
            shiny::tags$div(class = "hw-kpi-sub", "to cover NZS reform losses, real 2026 dollars")
          ),
          shiny::tags$div(
            class = "hw-kpi-card",
            shiny::tags$div(class = "hw-kpi-label", "Matched KiwiSaver contribution rate that would offset NZS reform"),
            shiny::tags$div(class = "hw-kpi-value", shiny::textOutput("person_required_matched_rate", inline = TRUE)),
            shiny::tags$div(class = "hw-kpi-sub", "same employee and employer contribution rate")
          )
        ),
        shiny::tags$p(
          class = "hw-panel-note",
          "The offset target is based on NZS reform losses only. Including a current KiwiSaver balance reduces the required contribution rate but does not change the NZS loss target."
        ),
        class = "hw-summary-section",
        subtitle = "How much KiwiSaver would offset the modelled NZS policy loss"
      ),
      hw_section(
        "08 / FUND SENSITIVITY",
        shiny::tableOutput("person_sensitivity_table"),
        shiny::tags$p(
          class = "hw-panel-note",
          "Uses the selected person and policy settings. Higher returns generally lower the required matched rate, but ESCT and government-contribution caps mean the relationship is not perfectly proportional."
        ),
        class = "hw-summary-section",
        subtitle = "Matched employer-employee KiwiSaver contribution rates that would offset NZS reform"
      )
    ),
    hw_section(
      "09 / MODELLING NOTES",
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$strong("Purpose. "),
          "This tab shows an illustrative person/archetype, not a population distribution or full retirement-adequacy calculator."
        ),
        shiny::tags$li(
          shiny::tags$strong("Dollar basis. "),
          "Dollar values are real 2026 dollars unless labelled otherwise; fund and post-65 returns are nominal net returns."
        ),
        shiny::tags$li(
          shiny::tags$strong("Earnings and retirement income. "),
          "Working-life earnings use Stats NZ wage/salary archetypes; retirement income uses IRD/MSD taxable non-NZS income archetypes."
        ),
        shiny::tags$li(
          shiny::tags$strong("KiwiSaver contributions. "),
          "Employee and employer rates apply to gross wage/salary earnings; employer contributions are reduced for ESCT; government contributions are included under the current capped contribution rules, with the cap and income threshold held fixed nominally."
        ),
        shiny::tags$li(
          shiny::tags$strong("NZS reform offset. "),
          "The target balance reflects expected NZS policy losses using Stats NZ life-table survival probabilities and the selected post-65 return. It is not a full retirement savings target."
        ),
        shiny::tags$li(
          shiny::tags$strong("Income testing. "),
          "KiwiSaver drawdown is not included in the income-test base."
        )
      ),
      class = "hw-note-section"
    )
  )
}

