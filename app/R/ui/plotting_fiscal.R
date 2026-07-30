plot_fiscal_cost_path <- function(fiscal_path, units = "nominal") {
  plot_data <- fiscal_path
  if (identical(units, "gdp")) {
    status_quo <- plot_data$status_quo_percent_gdp
    package <- plot_data$package_percent_gdp
    y_label <- "NZS expense (% of GDP)"
    y_axis_labels <- function(x) scales::percent(x, accuracy = 0.1)
    y_min <- 0.02
  } else {
    status_quo <- plot_data$status_quo_cost_billion
    package <- plot_data$package_cost_billion
    y_label <- "NZS expense ($b nominal)"
    y_axis_labels <- function(x) paste0("$", scales::comma(x, accuracy = 1), "b")
    y_min <- 0
  }
  y_max <- max(status_quo, package) * 1.08
  x_min <- 2025
  x_max <- max(plot_data$year)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(
    bg = "#FFFFFF",
    fg = "#163E5F",
    col.axis = "#163E5F",
    col.lab = "#163E5F",
    family = "sans",
    mar = c(5.1, 8.2, 1.1, 2.4),
    mgp = c(5.0, 0.9, 0),
    las = 1
  )

  plot(
    plot_data$year,
    status_quo,
    type = "n",
    ylim = c(y_min, y_max),
    xlim = c(x_min, x_max),
    xlab = "",
    ylab = y_label,
    bty = "l",
    axes = FALSE
  )
  x_ticks <- seq(x_min, x_max, by = 5)
  axis(1, at = x_ticks, labels = x_ticks, col = "#152F43", col.axis = "#163E5F")
  title(xlab = "Fiscal year", line = 3.1, col.lab = "#163E5F")
  if (identical(units, "nominal")) {
    y_ticks <- seq(0, ceiling(y_max / 20) * 20, by = 20)
  } else {
    y_ticks <- axTicks(2)
  }
  axis(
    2,
    at = y_ticks,
    labels = y_axis_labels(y_ticks),
    col = "#152F43",
    col.axis = "#163E5F"
  )
  abline(h = y_ticks, col = "#D9D9D9", lty = 1)
  box(bty = "l", col = "#152F43")
  lines(plot_data$year, status_quo, col = "#163E5F", lwd = 2)
  lines(plot_data$year, package, col = "#F26B44", lwd = 2)

  label_years <- c(2030, 2040, 2050, 2060, 2065)
  label_rows <- plot_data$year %in% label_years
  label_values <- function(x) {
    if (identical(units, "gdp")) {
      scales::percent(x, accuracy = 0.1)
    } else {
      paste0("$", scales::comma(x, accuracy = 1), "b")
    }
  }
  par(xpd = NA)
  text(
    plot_data$year[label_rows],
    status_quo[label_rows],
    labels = label_values(status_quo[label_rows]),
    pos = 3,
    offset = 0.35,
    cex = 0.78,
    col = "#163E5F"
  )
  text(
    plot_data$year[label_rows],
    package[label_rows],
    labels = label_values(package[label_rows]),
    pos = 1,
    offset = 0.35,
    cex = 0.78,
    col = "#A94B32"
  )
  par(xpd = FALSE)
  legend(
    "topleft",
    legend = c("Status quo", "Reform"),
    col = c("#163E5F", "#F26B44"),
    lwd = 2,
    bty = "n",
    text.col = "#163E5F"
  )
}

plot_fiscal_saving_decomposition <- function(fiscal_path, units = "nominal") {
  plot_data <- fiscal_path
  years <- plot_data$year
  positive <- data.frame(
    `Eligibility age` = plot_data$ea_gross_saving_billion,
    `Income test` = plot_data$income_test_saving_billion,
    `Indexation` = plot_data$indexation_saving_billion,
    check.names = FALSE
  )
  offset <- -plot_data$ea_offset_billion
  if (identical(units, "gdp")) {
    positive <- positive / plot_data$nominal_gdp_billion
    offset <- offset / plot_data$nominal_gdp_billion
    y_label <- "Annual saving (% of GDP)"
    y_axis_labels <- function(x) scales::percent(x, accuracy = 0.1)
  } else {
    y_label <- "Annual saving ($b nominal)"
    y_axis_labels <- function(x) paste0("$", scales::comma(x, accuracy = 1), "b")
  }
  positive_total <- rowSums(positive, na.rm = TRUE)
  y_min <- min(offset, 0, na.rm = TRUE) * 1.25
  y_max <- max(positive_total, 0, na.rm = TRUE) * 1.12
  if (identical(units, "gdp")) {
    y_max <- 0.04
  }
  if (y_max <= 0) {
    y_max <- 1
  }
  x_min <- 2025
  x_max <- max(years)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(
    bg = "#FFFFFF",
    fg = "#163E5F",
    col.axis = "#163E5F",
    col.lab = "#163E5F",
    family = "sans",
    mar = c(5.1, 8.2, 1.1, 1.1),
    mgp = c(5.0, 0.9, 0),
    las = 1
  )

  plot(
    years,
    positive_total,
    type = "n",
    ylim = c(y_min, y_max),
    xlim = c(x_min, x_max),
    xlab = "",
    ylab = y_label,
    bty = "l",
    axes = FALSE
  )
  x_ticks <- seq(x_min, x_max, by = 5)
  axis(1, at = x_ticks, labels = x_ticks, col = "#152F43", col.axis = "#163E5F")
  title(xlab = "Fiscal year", line = 3.1, col.lab = "#163E5F")
  y_ticks <- axTicks(2)
  axis(
    2,
    at = y_ticks,
    labels = y_axis_labels(y_ticks),
    col = "#152F43",
    col.axis = "#163E5F"
  )
  grid(nx = NA, ny = NULL, col = "#D9D9D9", lty = 1)
  abline(h = 0, col = "#152F43", lwd = 1.1)
  box(bty = "l", col = "#152F43")

  colours <- c("#2F6F73", "#F26B44", "#8C4A7C")
  lower <- rep(0, length(years))
  for (i in seq_along(positive)) {
    upper <- lower + positive[[i]]
    polygon(
      c(years, rev(years)),
      c(lower, rev(upper)),
      col = grDevices::adjustcolor(colours[[i]], alpha.f = 0.78),
      border = NA
    )
    lower <- upper
  }
  polygon(
    c(years, rev(years)),
    c(rep(0, length(years)), rev(offset)),
    col = grDevices::adjustcolor("#6F899A", alpha.f = 0.72),
    border = NA
  )
  lines(years, rowSums(positive, na.rm = TRUE) + offset, col = "#163E5F", lwd = 2)

  hw_mixed_legend(
    "topleft",
    labels = c(names(positive), "Benefit takeup", "Net saving"),
    key_type = c(rep("fill", length(positive) + 1), "line"),
    fill = c(grDevices::adjustcolor(colours, alpha.f = 0.78), grDevices::adjustcolor("#6F899A", alpha.f = 0.72), NA),
    col = c(rep(NA, 4), "#163E5F"),
    lwd = c(rep(NA, 4), 2),
    cex = 0.88
  )
}

