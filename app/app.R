library(shiny)
library(bslib)
library(scales)
library(zip)

source("R/model/model_constants.R")
source("R/model/parameters.R")
source("R/model/data_paths.R")
source("R/model/nzs_rates.R")
source("R/model/fiscal_engine.R")
source("R/model/person_impacts.R")
source("R/model/cohort_transition.R")
source("R/model/export_results.R")
source("R/model/export_workbook.R")
source("R/ui/theme.R")
source("R/ui/formatting.R")
source("R/ui/plotting.R")
source("R/ui/plotting_fiscal.R")
source("R/ui/plotting_cohort.R")
source("R/ui/plotting_person.R")
source("R/ui/layout.R")
source("R/ui/layout_about.R")
source("R/ui/layout_person.R")
source("R/ui/layout_cohort.R")
source("R/ui/layout_fiscal.R")

fiscal_inputs <- nzs_load_fiscal_inputs()
person_inputs <- nzs_load_person_impacts_inputs()

ui <- build_app_ui()

server <- function(input, output, session) {
  income_test_window <- shiny::debounce(
    reactive(input$income_test_window),
    millis = 500
  )
  person_current_age <- shiny::debounce(
    reactive(input$person_current_age),
    millis = 500
  )

  fiscal_policy <- reactive({
    income_test_ages <- income_test_window()
    modifyList(
      nzs_default_fiscal_policy(),
      list(
        indexation_regime = input$indexation_regime,
        indexation_start_year = input$indexation_start_year,
        indexation_grandparenting = input$indexation_grandparenting,
        eligibility_age_active = input$eligibility_age_active,
        eligibility_age_start_year = input$eligibility_age_start_year,
        eligibility_age_new_age = input$eligibility_age,
        eligibility_age_phase_in_years = as.numeric(input$eligibility_age_phase_in_years),
        eligibility_age_phase_in_months_per_year = nzs_phase_in_months_per_year(
          input$eligibility_age,
          as.numeric(input$eligibility_age_phase_in_years)
        ),
        eligibility_age_grandparenting = input$eligibility_age_grandparenting,
        income_test_active = input$income_test_active,
        income_test_start_year = input$income_test_start_year,
        income_test_from_age = income_test_ages[1],
        income_test_to_age = income_test_ages[2],
        income_test_threshold = parse_dollar_input(input$income_threshold),
        income_test_abatement_rate = parse_percent_point_input(input$abatement_rate),
        income_test_grandparenting = input$income_test_grandparenting,
        discount_rate = parse_percent_point_input(input$discount_rate),
        npv_base_year = nzs_model_base_year()
      )
    )
  })

  fiscal_path <- reactive({
    nzs_project_fiscal_impacts(fiscal_policy(), fiscal_inputs)
  })

  fiscal_summary <- reactive({
    nzs_summarise_fiscal_impacts(
      fiscal_path(),
      discount_rate = fiscal_policy()$discount_rate,
      base_year = fiscal_policy()$npv_base_year
    )
  })

  person_impacts <- reactive({
    nzs_project_person_impacts(
      fiscal_policy = fiscal_policy(),
      current_age = person_current_age(),
      earnings_archetype = input$person_earnings_archetype,
      retirement_income_archetype = input$person_retirement_income,
      living_arrangement = input$person_living_arrangement,
      employee_rate = parse_percent_point_input(input$person_employee_rate),
      employer_rate = parse_percent_point_input(input$person_employer_rate),
      accumulation_return = nzs_kiwisaver_fund_returns()[[input$person_fund_return]],
      drawdown_return = parse_percent_point_input(input$person_drawdown_return),
      current_balance = parse_dollar_input(input$person_current_balance),
      inputs = person_inputs
    )
  })

  person_fund_sensitivity <- reactive({
    nzs_person_impacts_fund_sensitivity(
      fiscal_policy = fiscal_policy(),
      current_age = person_current_age(),
      earnings_archetype = input$person_earnings_archetype,
      retirement_income_archetype = input$person_retirement_income,
      living_arrangement = input$person_living_arrangement,
      drawdown_return = parse_percent_point_input(input$person_drawdown_return),
      current_balance = parse_dollar_input(input$person_current_balance),
      inputs = person_inputs
    )
  })

  cohort_transition <- reactive({
    nzs_project_cohort_transition(
      fiscal_policy = fiscal_policy(),
      inputs = person_inputs,
      nominal_discount_rate = fiscal_policy()$discount_rate
    )
  })

  cohort_deferral <- reactive({
    nzs_project_cohort_deferral(
      fiscal_policy = fiscal_policy(),
      inputs = fiscal_inputs
    )
  })

  cohort_income_test_incidence <- reactive({
    nzs_current_income_test_incidence(
      policy = fiscal_policy(),
      inputs = fiscal_inputs
    )
  })

  output$person_balance_plot <- renderPlot({
    result <- person_impacts()
    plot_person_balance_path(result$balance_profile, result$target_balance_real)
  })

  output$person_working_life_plot <- renderPlot({
    result <- person_impacts()
    plot_person_working_life(result$working_life)
  })

  output$person_retirement_income_plot <- renderPlot({
    result <- person_impacts()
    plot_person_retirement_income(result$retirement_income)
  })

  output$person_replacement_table <- renderTable({
    out <- person_impacts()$replacement_rates
    data.frame(
      Metric = out$metric,
      `Replacement rate` = format_percent(out$replacement_rate),
      `Age-64 earnings` = format_dollar(out$age64_earnings_real),
      check.names = FALSE
    )
  })

  output$person_target_balance <- renderText({
    format_dollar(person_impacts()$target_balance_real)
  })

  output$person_projected_balance <- renderText({
    format_dollar(person_impacts()$projected_balance_real)
  })

  output$person_shortfall <- renderText({
    shortfall <- person_impacts()$shortfall_real
    if (abs(shortfall) < 1) {
      "On target"
    } else if (shortfall > 0) {
      paste(format_dollar(shortfall), "shortfall")
    } else {
      paste(format_dollar(abs(shortfall)), "surplus")
    }
  })

  output$person_required_matched_rate <- renderText({
    rate <- person_impacts()$required_matched_rate
    if (!is.finite(rate)) {
      "Not reached"
    } else {
      format_percent(rate)
    }
  })

  output$person_sensitivity_table <- renderTable({
    out <- person_fund_sensitivity()
    data.frame(
      "Fund type" = out$fund_type,
      "Return (nominal)" = format_percent(out$accumulation_return),
      "Required matched rate" = ifelse(is.finite(out$required_matched_rate), format_percent(out$required_matched_rate), "Not reached"),
      check.names = FALSE
    )
  }, striped = TRUE, align = "lrr")

  output$person_archetype_snapshot <- renderUI({
    snapshot <- nzs_person_archetype_snapshot(
      inputs = person_inputs,
      current_age = person_current_age(),
      earnings_archetype = input$person_earnings_archetype,
      retirement_income_archetype = input$person_retirement_income
    )

    shiny::tags$div(
      class = "hw-archetype-snapshot",
      shiny::tags$div(class = "hw-archetype-snapshot-title", "Selected archetype"),
      shiny::tags$div(
        class = "hw-archetype-snapshot-lines",
        shiny::tags$div(
          class = "hw-archetype-snapshot-line",
          shiny::tags$span(class = "hw-archetype-snapshot-value", format_dollar_k(snapshot$current_age_earnings_real)),
          shiny::tags$span(" earnings at age ", snapshot$current_age)
        ),
        shiny::tags$div(
          class = "hw-archetype-snapshot-line",
          shiny::tags$span(class = "hw-archetype-snapshot-value", format_dollar_k(snapshot$age65_retirement_income_real)),
          shiny::tags$span(" other taxable income at age 65")
        )
      ),
      shiny::tags$div(
        class = "hw-archetype-snapshot-note",
        "Real 2026 dollars. Retirement income excludes NZS and KiwiSaver drawdown."
      )
    )
  })

  output$person_archetype_guide <- renderUI({
    guide <- nzs_person_archetype_guide_data(person_inputs)
    guide_rows <- lapply(seq_len(nrow(guide)), function(i) {
      shiny::tags$tr(
        shiny::tags$td(guide$archetype[[i]]),
        shiny::tags$td(shiny::HTML(paste0(
          "Age ", guide$peak_age[[i]], " peak ~", format_dollar_k(guide$peak_earnings_real[[i]]),
          "<br>Age 64 ~", format_dollar_k(guide$age64_earnings_real[[i]])
        ))),
        shiny::tags$td(shiny::HTML(paste0(
          "Avg 65-69 ~", format_dollar_k(guide$average_retirement_income_65_69_real[[i]]),
          "<br>Age 75+ ~", format_dollar_k(guide$age75_plus_retirement_income_real[[i]])
        )))
      )
    })

    shiny::tags$div(
      class = "hw-archetype-guide-table-wrap",
      shiny::tags$table(
        class = "hw-archetype-guide-table",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Archetype"),
            shiny::tags$th("Working-life earnings"),
            shiny::tags$th("Retirement income")
          )
        ),
        shiny::tags$tbody(guide_rows)
      )
    )
  })

  output$export_results <- downloadHandler(
    filename = function() {
      paste0(
        "nz-retirement-policy-model-results-v",
        nzs_model_version(),
        "-",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      )
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      person_settings <- list(
        `Current age in 2026` = person_current_age(),
        `Working-life earnings` = input$person_earnings_archetype,
        `Retirement income` = input$person_retirement_income,
        `Living arrangement` = input$person_living_arrangement,
        `Current KiwiSaver balance` = parse_dollar_input(input$person_current_balance),
        `Fund return` = input$person_fund_return,
        `Fund return (nominal)` = nzs_kiwisaver_fund_returns()[[input$person_fund_return]],
        `Post-65 return (nominal)` = parse_percent_point_input(input$person_drawdown_return),
        `Employee contribution rate` = parse_percent_point_input(input$person_employee_rate),
        `Employer contribution rate` = parse_percent_point_input(input$person_employer_rate)
      )
      export_results <- nzs_build_export_results(
        policy = fiscal_policy(),
        fiscal_path = fiscal_path(),
        fiscal_summary = fiscal_summary(),
        cohort_transition = cohort_transition(),
        cohort_deferral = cohort_deferral(),
        cohort_income_test_incidence = cohort_income_test_incidence(),
        person_impacts = person_impacts(),
        person_fund_sensitivity = person_fund_sensitivity(),
        person_settings = person_settings
      )
      nzs_write_export_workbook(export_results, file)
    }
  )

  output$cohort_transition_plot <- renderPlot({
    plot_cohort_transition_staircase(cohort_transition())
  })

  output$cohort_deferral_plot <- renderPlot({
    plot_cohort_deferral_by_age(cohort_deferral())
  })

  output$cohort_income_test_incidence_plot <- renderPlot({
    plot_income_test_incidence(
      cohort_income_test_incidence(),
      metric = input$cohort_income_test_metric
    )
  })

  output$fiscal_cost_plot <- renderPlot({
    plot_fiscal_cost_path(fiscal_path(), units = input$fiscal_cost_path_units)
  })

  output$fiscal_decomposition_plot <- renderPlot({
    plot_fiscal_saving_decomposition(fiscal_path(), units = input$fiscal_cost_path_units)
  })

  output$fiscal_headline_decomposition_table <- renderTable({
    out <- fiscal_summary()
    data.frame(
      `Policy component` = c("Eligibility age", "Income test", "Indexation"),
      `NPV saving` = format_billion(c(
        out$value[out$metric == "EA net saving NPV"],
        out$value[out$metric == "Income-test saving NPV"],
        out$value[out$metric == "Indexation saving NPV"]
      )),
      check.names = FALSE
    )
  }, striped = TRUE, align = "lr")

  output$fiscal_npv_saving <- renderText({
    format_billion(fiscal_summary()$value[fiscal_summary()$metric == "Net fiscal saving NPV"])
  })

  output$fiscal_average_saving <- renderText({
    format_billion(mean(fiscal_path()$net_saving_billion, na.rm = TRUE))
  })

  output$fiscal_assumptions <- renderTable({
    p <- fiscal_policy()
    data.frame(
      Assumption = c(
        "Eligibility age",
        "Income test",
        "Indexation",
        "Discount rate",
        "Income basis",
        "Benefit offsets"
      ),
      Setting = c(
        if (isTRUE(p$eligibility_age_active)) {
          paste0(
            "Age ",
            p$eligibility_age_new_age,
            " from ",
            p$eligibility_age_start_year,
            "; phase-in ",
            if (p$eligibility_age_phase_in_years <= 0) {
              "immediate"
            } else {
              paste0("over ", p$eligibility_age_phase_in_years, " years")
            }
          )
        } else {
          "Inactive"
        },
        if (isTRUE(p$income_test_active)) {
          paste0(
            "Ages ",
            p$income_test_from_age,
            "-",
            p$income_test_to_age - 1,
            "; ",
            format_dollar(p$income_test_threshold),
            " threshold; ",
            "CPI-indexed",
            "; ",
            format_percent(p$income_test_abatement_rate),
            " rate"
          )
        } else {
          "Inactive"
        },
        paste0(nzs_policy_regime_label(p$indexation_regime), " from ", p$indexation_start_year),
        paste0(format_percent(p$discount_rate), " nominal"),
        "IRD/MSD/Stats NZ taxable-income histogram; wage-uprated income; CPI-indexed threshold",
        "Public-source MSD benefit-offset calibration"
      ),
      check.names = FALSE
    )
  }, striped = TRUE)
}

shinyApp(ui, server)
