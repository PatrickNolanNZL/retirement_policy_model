nzs_export_table_name <- function(sheet_name, section_name) {
  raw <- paste(sheet_name, section_name, sep = "_")
  clean <- gsub("[^A-Za-z0-9_]", "_", raw)
  clean <- gsub("_+", "_", clean)
  clean <- gsub("^_|_$", "", clean)
  paste0("tbl_", tolower(substr(clean, 1, 240)))
}

nzs_export_style_palette <- function() {
  list(
    primary_navy = "152F43",
    body_navy = "163E5F",
    slate_label = "6F899A",
    accent_orange = "F26B44",
    canvas_white = "FFFFFF"
  )
}

nzs_export_range_dims <- function(rows, cols) {
  paste0(
    openxlsx2::int2col(min(cols)), min(rows),
    ":",
    openxlsx2::int2col(max(cols)), max(rows)
  )
}

nzs_export_style_range <- function(
    wb,
    sheet,
    rows,
    cols,
    font = "Segoe UI",
    size = 10,
    color = nzs_export_style_palette()$body_navy,
    bold = FALSE,
    fill = NULL,
    border = c("none", "section", "header")) {
  if (length(rows) == 0 || length(cols) == 0) {
    return(wb)
  }

  palette <- nzs_export_style_palette()
  border <- match.arg(border)
  dims <- nzs_export_range_dims(rows, cols)

  wb <- openxlsx2::wb_add_font(
    wb,
    sheet = sheet,
    dims = dims,
    name = font,
    color = openxlsx2::wb_color(hex = color),
    size = size,
    bold = if (bold) TRUE else "",
    update = TRUE
  )

  if (border == "section") {
    wb <- openxlsx2::wb_add_border(
      wb,
      sheet = sheet,
      dims = dims,
      bottom_color = openxlsx2::wb_color(hex = palette$primary_navy),
      bottom_border = "medium",
      left_border = "none",
      right_border = "none",
      update = TRUE
    )
  } else if (border == "header") {
    wb <- openxlsx2::wb_add_border(
      wb,
      sheet = sheet,
      dims = dims,
      top_color = openxlsx2::wb_color(hex = palette$primary_navy),
      bottom_color = openxlsx2::wb_color(hex = palette$primary_navy),
      top_border = "thin",
      bottom_border = "medium",
      left_border = "none",
      right_border = "none",
      update = TRUE
    )
  }

  wb
}

nzs_export_format_data_columns <- function(wb, sheet, data, start_row, start_col) {
  if (nrow(data) == 0 || ncol(data) == 0) {
    return(wb)
  }

  data_rows <- (start_row + 1):(start_row + nrow(data))
  for (i in seq_along(data)) {
    col <- start_col + i - 1
    fmt <- nzs_export_column_numfmt(names(data)[[i]])

    if (!is.null(fmt)) {
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        sheet = sheet,
        dims = nzs_export_range_dims(data_rows, col),
        numfmt = fmt
      )
    }
  }

  wb
}

nzs_export_column_numfmt <- function(header) {
  header <- tolower(header)

  calendar_year_columns <- c("year", "birth_year", "year_at_65")
  integer_age_columns <- c(
    "age",
    "current_age",
    "current age",
    "age at export",
    "age at year-end",
    "income test age from",
    "income test age to"
  )

  if (header %in% calendar_year_columns) {
    return('0;(0);"-"')
  }
  if (header %in% integer_age_columns) {
    return('0;(0);"-"')
  }
  if (header == "years_deferred") {
    return('0.0;(0.0);"-"')
  }
  if (grepl("%|rate", header)) {
    return('0.0%;(0.0%);"-"')
  }
  if (grepl("\\$bn", header)) {
    return('"$"#,##0.0"b";("$"#,##0.0"b");"-"')
  }
  if (grepl("\\$", header)) {
    return('"$"#,##0;("$"#,##0);"-"')
  }
  if (grepl("people|population|count|births|deferred_person_years", header)) {
    return('#,##0;(#,##0);"-"')
  }
  if (grepl("value|saving|loss|balance|income|contribution|nzs", header)) {
    return('#,##0.0;(#,##0.0);"-"')
  }

  NULL
}

nzs_write_export_section <- function(wb, sheet, title, data, start_row) {
  palette <- nzs_export_style_palette()
  start_col <- 2
  end_col <- start_col + ncol(data) - 1
  header_row <- start_row + 1

  wb$add_data(sheet, title, start_row = start_row, start_col = 2, col_names = FALSE)
  wb$add_data_table(
    sheet,
    data,
    start_row = header_row,
    start_col = start_col,
    table_name = nzs_export_table_name(sheet, title),
    table_style = "TableStyleLight1",
    with_filter = TRUE,
    banded_rows = FALSE
  )

  wb <- nzs_export_style_range(
    wb,
    sheet,
    rows = start_row,
    cols = start_col:end_col,
    size = 11,
    color = palette$slate_label,
    bold = TRUE,
    border = "section"
  )
  wb <- nzs_export_style_range(
    wb,
    sheet,
    rows = header_row,
    cols = start_col:end_col,
    size = 9,
    color = palette$primary_navy,
    bold = TRUE,
    border = "header"
  )
  wb <- nzs_export_style_range(
    wb,
    sheet,
    rows = (header_row + 1):(header_row + nrow(data)),
    cols = start_col:end_col,
    color = palette$body_navy
  )
  wb <- nzs_export_format_data_columns(wb, sheet, data, header_row, start_col)

  if (identical(sheet, "Readme")) {
    wb <- openxlsx2::wb_add_cell_style(
      wb,
      sheet = sheet,
      dims = nzs_export_range_dims((header_row + 1):(header_row + nrow(data)), start_col:end_col),
      wrap_text = TRUE,
      vertical = "top"
    )
  }

  list(wb = wb, next_row = start_row + nrow(data) + 5)
}

nzs_write_export_sheet <- function(wb, sheet, title, subtitle = NULL, sections = list(), start_row = 5) {
  palette <- nzs_export_style_palette()
  wb$add_worksheet(sheet)
  wb$set_grid_lines(sheet, show = FALSE)
  wb$set_sheetview(sheet, zoom_scale = 90)
  wb$set_col_widths(sheet, cols = 1, widths = 4.7)
  wb$set_col_widths(sheet, cols = 2:20, widths = 18)
  if (identical(sheet, "Cover")) {
    wb$set_col_widths(sheet, cols = 3, widths = 34)
  }
  if (identical(sheet, "Readme")) {
    wb$set_col_widths(sheet, cols = 2, widths = 96)
  }
  wb$add_data(sheet, title, start_row = 2, start_col = 2, col_names = FALSE)
  wb <- nzs_export_style_range(
    wb,
    sheet,
    rows = 2,
    cols = 2:8,
    font = "Georgia",
    size = 22,
    color = palette$primary_navy
  )
  if (!is.null(subtitle)) {
    wb$add_data(sheet, subtitle, start_row = 3, start_col = 2, col_names = FALSE)
    wb <- nzs_export_style_range(
      wb,
      sheet,
      rows = 3,
      cols = 2:8,
      size = 9,
      color = palette$slate_label,
      bold = TRUE
    )
  }

  row <- start_row
  for (section in sections) {
    section_result <- nzs_write_export_section(wb, sheet, section$title, section$data, row)
    wb <- section_result$wb
    row <- section_result$next_row
  }
  wb
}

nzs_write_export_workbook <- function(export_results, file) {
  palette <- nzs_export_style_palette()
  wb <- openxlsx2::wb_workbook()
  wb$add_creators("Heuser | Whittington")
  wb <- openxlsx2::wb_set_base_font(
    wb,
    font_name = "Segoe UI",
    font_size = 10,
    font_color = openxlsx2::wb_color(hex = palette$body_navy)
  )

  wb <- nzs_write_export_sheet(
    wb,
    "Cover",
    "NZ retirement policy model",
    "SCENARIO MODEL PROTOTYPE",
    list(list(title = "01 / EXPORT METADATA", data = export_results$metadata)),
    start_row = 9
  )
  wb$add_data("Cover", "HEUSER | WHITTINGTON", start_row = 5, start_col = 2, col_names = FALSE)
  wb$add_data("Cover", "www.heuserwhittington.com", start_row = 6, start_col = 2, col_names = FALSE)
  wb <- nzs_export_style_range(
    wb,
    "Cover",
    rows = 5,
    cols = 2:8,
    size = 11,
    color = palette$slate_label,
    bold = TRUE
  )
  wb <- nzs_export_style_range(
    wb,
    "Cover",
    rows = 6,
    cols = 2:8,
    size = 10,
    color = palette$body_navy
  )
  wb <- openxlsx2::wb_add_hyperlink(
    wb,
    sheet = "Cover",
    dims = "B6",
    target = "https://www.heuserwhittington.com"
  )
  wb <- nzs_write_export_sheet(
    wb,
    "Readme",
    "Readme",
    "Export notes and caveats",
    list(list(title = "01 / NOTES", data = export_results$readme))
  )
  wb <- nzs_write_export_sheet(
    wb,
    "Settings",
    "Settings",
    "Selected app settings at export time",
    list(list(title = "01 / SELECTED SETTINGS", data = export_results$settings))
  )
  wb <- nzs_write_export_sheet(
    wb,
    "Fiscal impacts",
    "Fiscal impacts",
    "Aggregate fiscal outputs for the selected reform",
    list(
      list(title = "01 / HEADLINE SUMMARY", data = export_results$fiscal$headline_summary),
      list(title = "02 / ANNUAL FISCAL PATH", data = export_results$fiscal$annual_fiscal_path),
      list(title = "03 / ANNUAL SAVING DECOMPOSITION", data = export_results$fiscal$annual_saving_decomposition)
    )
  )
  wb <- nzs_write_export_sheet(
    wb,
    "Cohort impacts",
    "Cohort impacts",
    "Cohort and income-group outputs for the selected reform",
    list(
      list(title = "01 / TRANSITION IMPACT BY COHORT", data = export_results$cohort$transition_impact_by_cohort),
      list(title = "02 / EA DEFERRAL BY COHORT", data = export_results$cohort$ea_deferral_by_cohort),
      list(title = "03 / INCOME-TEST INCIDENCE", data = export_results$cohort$income_test_incidence)
    )
  )
  wb <- nzs_write_export_sheet(
    wb,
    "Person impacts",
    "Person impacts",
    "Illustrative person outputs for the selected reform",
    list(
      list(title = "01 / OFFSET SUMMARY", data = export_results$person$offset_summary),
      list(title = "02 / WORKING-LIFE EARNINGS AND CONTRIBUTIONS", data = export_results$person$working_life_earnings_and_contributions),
      list(title = "03 / KIWISAVER BALANCE PATH", data = export_results$person$kiwisaver_balance_path),
      list(title = "04 / RETIREMENT INCOME COMPOSITION", data = export_results$person$retirement_income_composition),
      list(title = "05 / REPLACEMENT RATE", data = export_results$person$replacement_rate),
      list(title = "06 / FUND SENSITIVITY", data = export_results$person$fund_sensitivity)
    )
  )

  old_options <- options(
    openxlsx2.no_utils_zip = TRUE,
    openxlsx2.no_bsdtar = TRUE
  )
  on.exit(options(old_options), add = TRUE)
  openxlsx2::wb_save(wb, file = file, overwrite = TRUE)
  invisible(file)
}
