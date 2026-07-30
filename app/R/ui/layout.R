hw_info_popover <- function(info_text, label = "More info") {
  shiny::tags$div(
    class = "hw-info-popover",
    tabindex = "0",
    shiny::tags$span(
      class = "hw-info-trigger",
      shiny::tags$span(class = "hw-info-trigger-label", label),
      shiny::tags$span(class = "hw-info-trigger-icon", "i")
    ),
    shiny::tags$div(class = "hw-info-popover-body", info_text)
  )
}

hw_section <- function(title, ..., class = NULL, subtitle = NULL, info_text = NULL) {
  section_title <- title
  title_match <- regexec("^([0-9]+[A-Za-z]?)\\s*/\\s*(.+)$", title)
  title_parts <- regmatches(title, title_match)[[1]]

  if (length(title_parts) == 3) {
    section_title <- shiny::tagList(
      shiny::tags$span(class = "hw-section-number", title_parts[2]),
      shiny::tags$span(class = "hw-section-divider", " / "),
      shiny::tags$span(title_parts[3])
    )
  }

  shiny::tags$section(
    class = paste(c(
      "hw-section",
      if (!is.null(info_text)) "hw-section-has-info",
      class
    ), collapse = " "),
    shiny::tags$div(
      class = "hw-section-title-row",
      shiny::tags$div(class = "hw-section-title", section_title)
    ),
    if (!is.null(subtitle) || !is.null(info_text)) {
      shiny::tags$div(
        class = "hw-section-subtitle-row",
        if (!is.null(subtitle)) {
          shiny::tags$p(class = "hw-section-subtitle", subtitle)
        }
      )
    },
    ...,
    if (!is.null(info_text)) {
      hw_info_popover(info_text)
    }
  )
}

hw_control_group <- function(title, ..., class = NULL) {
  shiny::tags$div(
    class = paste(c("hw-control-group", class), collapse = " "),
    shiny::tags$div(class = "hw-control-group-title", title),
    ...
  )
}

hw_mini_info_popover <- function(info_text) {
  shiny::tags$span(
    class = "hw-mini-info-popover",
    tabindex = "0",
    shiny::tags$span(class = "hw-mini-info-trigger", "i"),
    shiny::tags$span(class = "hw-info-popover-body", info_text)
  )
}

hw_label_with_info <- function(label, info_text) {
  shiny::tagList(
    shiny::tags$span(label),
    hw_mini_info_popover(info_text)
  )
}

person_archetype_guide <- function() {
  shiny::tags$details(
    class = "hw-archetype-guide",
    shiny::tags$summary("Guide to earnings profiles"),
    shiny::uiOutput("person_archetype_snapshot"),
    shiny::uiOutput("person_archetype_guide")
  )
}

person_impacts_controls <- function() {
  fund_returns <- nzs_kiwisaver_fund_returns()
  fund_return_choices <- stats::setNames(
    names(fund_returns),
    paste0(names(fund_returns), " (", scales::percent(fund_returns, accuracy = 0.1), ")")
  )

  hw_section(
    "02 / PERSON AND SAVING ASSUMPTIONS",
    shiny::tags$div(
      class = "hw-control-group hw-control-group-untitled",
      shiny::sliderInput("person_current_age", "Current age in 2026", min = 20, max = 64, value = 35, step = 1),
      shiny::tags$div(
        class = "hw-field-grid",
        shiny::selectInput(
          "person_earnings_archetype",
          hw_label_with_info(
            "Working-life earnings",
            shiny::tagList(
              shiny::tags$p("Choose the wage/salary path used for KiwiSaver contributions before age 65."),
              shiny::tags$ul(
                shiny::tags$li("Median follows Stats NZ HLFS wage/salary earnings by age."),
                shiny::tags$li("Low and high are stylised lower/higher versions of the median profile."),
                shiny::tags$li("For the median profile, earnings peak at about $85k and are about $74k at age 64.")
              )
            )
          ),
          choices = c("Low" = "low", "Median" = "median", "High" = "high"),
          selected = "median"
        ),
        shiny::selectInput(
          "person_retirement_income",
          hw_label_with_info(
            "Retirement income",
            shiny::tagList(
              shiny::tags$p("Choose the person's other taxable income in retirement, before NZ Super and before KiwiSaver drawdown."),
              shiny::tags$ul(
                shiny::tags$li("Low means little or no taxable income beyond NZ Super."),
                shiny::tags$li("Medium is a typical middle case, around $14k a year across ages 65-69."),
                shiny::tags$li("High represents a person with materially higher taxable income in retirement.")
              )
            )
          ),
          choices = c("Low" = "low", "Medium" = "medium", "High" = "high"),
          selected = "medium"
        )
      ),
      person_archetype_guide(),
      shiny::tags$div(
        class = "hw-field-grid",
        shiny::selectInput("person_living_arrangement", "Living arrangement", choices = names(nzs_living_arrangements())),
        shiny::numericInput("person_current_balance", "Current KiwiSaver balance", value = 0, min = 0, step = 1000)
      ),
      shiny::tags$div(
        class = "hw-field-grid",
        shiny::selectInput("person_fund_return", "Fund return (nominal)", choices = fund_return_choices, selected = "Balanced"),
        shiny::numericInput("person_drawdown_return", "Post-65 return (%, nominal)", value = 2.5, min = 0, step = 0.5)
      ),
      shiny::tags$div(
        class = "hw-field-grid",
        shiny::numericInput("person_employee_rate", "Employee contribution (%)", value = 4, min = 0, step = 0.5),
        shiny::numericInput("person_employer_rate", "Employer contribution (%)", value = 4, min = 0, step = 0.5)
      )
    )
  )
}

reform_controls <- function() {
  hw_section(
    "01 / NZ SUPERANNUATION REFORM PARAMETERS",
    shiny::tags$div(
      class = "hw-reform-grid",
      hw_control_group(
        "Eligibility age",
        shiny::checkboxInput("eligibility_age_active", "Include eligibility-age change", value = TRUE),
        shiny::numericInput("eligibility_age", "New eligibility age", value = 67, min = 65, max = 75, step = 1),
        shiny::numericInput("eligibility_age_start_year", "Start year", value = 2027, min = 2026, max = 2065, step = 1),
        shiny::selectInput("eligibility_age_phase_in_years", "Phase-in period", choices = nzs_phase_in_periods(), selected = 2),
        shiny::checkboxInput("eligibility_age_grandparenting", "Grandparent existing recipients", value = FALSE)
      ),
      hw_control_group(
        "Indexation regime",
        shiny::selectInput("indexation_regime", "Regime", choices = nzs_policy_regimes(), selected = "A"),
        shiny::numericInput("indexation_start_year", "Start year", value = 2027, min = 2026, max = 2065, step = 1),
        shiny::checkboxInput("indexation_grandparenting", "Grandparent existing recipients", value = FALSE)
      )
    ),
    hw_control_group(
      "Income testing",
      shiny::checkboxInput("income_test_active", "Include income test", value = TRUE),
      shiny::sliderInput("income_test_window", "Income test ages", min = 65, max = 90, value = c(65, 70), step = 1),
      shiny::tags$div(
        class = "hw-field-grid",
        shiny::numericInput("income_threshold", "Income threshold", value = 10000, min = 0, step = 1000),
        shiny::numericInput("abatement_rate", "Abatement rate (%)", value = 25, min = 0, max = 100, step = 5)
      ),
      shiny::tags$div(
        class = "hw-field-grid",
        shiny::numericInput("income_test_start_year", "Start year", value = 2027, min = 2026, max = 2065, step = 1),
        shiny::tags$div(
          class = "hw-checkbox-align-input",
          shiny::checkboxInput("income_test_grandparenting", "Grandparent existing recipients", value = FALSE)
        )
      ),
      class = "hw-income-test-group"
    ),
    hw_control_group(
      "Other settings",
      shiny::numericInput("discount_rate", "Discount rate (%, nominal)", value = 4.3, min = 0, step = 0.1),
      class = "hw-compact-setting-group"
    )
  )
}

tab_label <- function(label, tooltip) {
  shiny::tags$span(title = tooltip, label)
}

main_tabs <- function() {
  shiny::tags$div(
    class = "hw-model-tabs",
    shiny::downloadButton("export_results", "Export results", class = "hw-export-results-button"),
    shiny::tabsetPanel(
      id = "model_view",
      type = "tabs",
      shiny::tabPanel(
        title = tab_label("About", "Summarises the model purpose, scope, and prototype status."),
        value = "about",
        about_view()
      ),
      shiny::tabPanel(
        title = tab_label("Fiscal impacts", "Shows the fiscal costs and policy-channel decomposition of the selected reform."),
        value = "fiscal_impacts",
        fiscal_impacts_view()
      ),
      shiny::tabPanel(
        title = tab_label("Cohort impacts", "Shows how selected reforms affect different birth cohorts and income groups."),
        value = "cohort_impacts",
        cohort_impacts_view()
      ),
      shiny::tabPanel(
        title = tab_label("Person impacts", "Shows how selected reforms affect an illustrative person, including KiwiSaver contribution settings."),
        value = "person_impacts",
        person_impacts_view()
      )
    )
  )
}

build_app_ui <- function() {
  bslib::page_fillable(
    title = "NZ retirement policy model",
    theme = hw_theme(),
    shiny::tags$head(
      shiny::tags$title("NZ retirement policy model"),
      shiny::tags$link(rel = "icon", type = "image/x-icon", href = "hw-favicon.ico"),
      shiny::tags$link(rel = "stylesheet", href = "styles.css?v=20260727a"),
      shiny::tags$script("document.title = 'NZ retirement policy model';"),
      shiny::tags$script(shiny::HTML(
        "
        document.addEventListener('click', async function(event) {
          const link = event.target.closest && event.target.closest('#export_results');
          if (!link || link.classList.contains('disabled')) return;
          const href = link.getAttribute('href');
          if (!href) return;

          event.preventDefault();
          event.stopPropagation();

          const originalText = link.textContent;
          link.classList.add('disabled');
          link.textContent = 'Exporting...';

          try {
            const response = await fetch(new URL(href, window.location.href));
            if (!response.ok) {
              throw new Error('Export failed with status ' + response.status);
            }
            const blob = await response.blob();
            const today = new Date().toISOString().slice(0, 10).replaceAll('-', '');
            const downloadLink = document.createElement('a');
            downloadLink.href = URL.createObjectURL(blob);
            downloadLink.download = 'nz-retirement-policy-model-results-' + today + '.xlsx';
            document.body.appendChild(downloadLink);
            downloadLink.click();
            downloadLink.remove();
            setTimeout(function() { URL.revokeObjectURL(downloadLink.href); }, 1000);
          } catch (error) {
            console.error(error);
            window.alert('The Excel export could not be created. Please try again.');
          } finally {
            link.textContent = originalText;
            link.classList.remove('disabled');
          }
        }, true);
        "
      ))
    ),
    shiny::tags$div(
      class = "hw-page",
      shiny::tags$header(
        class = "hw-header",
        shiny::tags$div(
          class = "hw-title-lockup",
          shiny::tags$img(
            class = "hw-title-logo",
            src = "hw-logo-mark.svg",
            alt = "Heuser Whittington"
          ),
          shiny::tags$div(
            class = "hw-title-text",
            shiny::tags$h1("NZ retirement policy model"),
            shiny::tags$div(
              class = "hw-subtitle",
              "HEUSER | WHITTINGTON"
            )
          )
        ),
        shiny::tags$div(
          class = "hw-proto-ribbon",
          "SCENARIO MODEL PROTOTYPE"
        )
      ),
      shiny::tags$div(
        class = "hw-dashboard",
        shiny::tags$aside(
          class = "hw-control-panel",
          reform_controls(),
          shiny::conditionalPanel(
            condition = "input.model_view == 'person_impacts'",
            person_impacts_controls()
          )
        ),
        shiny::tags$main(
          class = "hw-main-panel",
          main_tabs()
        )
      )
    )
  )
}
