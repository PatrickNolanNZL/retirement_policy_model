plot_cohort_transition_staircase <- function(cohort_transition) {
  plot_data <- cohort_transition[order(cohort_transition$birth_year), ]
  x_min <- 1955
  x_max <- 1990
  plot_data <- plot_data[plot_data$birth_year >= x_min & plot_data$birth_year <= x_max, ]
  if (nrow(plot_data) == 0) {
    plot.new()
    text(0.5, 0.5, "No cohorts in selected display range", col = "#163E5F")
    return(invisible(NULL))
  }
  annotations <- attr(cohort_transition, "annotations", exact = TRUE)
  if (is.null(annotations)) {
    annotations <- list()
  }
  y_min <- min(plot_data$pct_loss, 0, na.rm = TRUE)
  if (!is.finite(y_min) || y_min >= 0) {
    y_min <- 0
  }
  y_max <- 0.40

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(
    bg = "#FFFFFF",
    fg = "#163E5F",
    col.axis = "#163E5F",
    col.lab = "#163E5F",
    family = "sans",
    mar = c(5.1, 7.4, 1.1, 1.1),
    mgp = c(4.4, 0.9, 0),
    las = 1
  )

  plot(
    plot_data$birth_year,
    plot_data$pct_loss,
    type = "n",
    xlim = c(x_min, x_max),
    ylim = c(y_min, y_max),
    xlab = "",
    ylab = "Lifetime loss under the reform",
    bty = "l",
    axes = FALSE
  )
  phase_min <- annotations$phase_in_birth_year_min
  phase_max <- annotations$phase_in_birth_year_max
  phase_visible_min <- max(phase_min, x_min)
  phase_visible_max <- min(phase_max, x_max)
  if (length(phase_min) == 1 && length(phase_max) == 1 &&
      is.finite(phase_min) && is.finite(phase_max) &&
      phase_visible_min <= phase_visible_max) {
    rect(
      phase_visible_min - 0.5,
      par("usr")[[3]],
      phase_visible_max + 0.5,
      par("usr")[[4]],
      col = grDevices::adjustcolor("#E3A878", alpha.f = 0.18),
      border = NA
    )
  }
  x_ticks <- seq(x_min, x_max, by = 5)
  axis(1, at = x_ticks, labels = x_ticks, col = "#152F43", col.axis = "#163E5F")
  title(xlab = "Cohort birth year", line = 3.1, col.lab = "#163E5F")
  y_ticks <- axTicks(2)
  axis(
    2,
    at = y_ticks,
    labels = scales::percent(y_ticks, accuracy = 1),
    col = "#152F43",
    col.axis = "#163E5F"
  )
  grid(nx = NA, ny = NULL, col = "#D9D9D9", lty = 1)
  abline(h = 0, col = "#152F43", lwd = 1)
  box(bty = "l", col = "#152F43")

  if (length(phase_min) == 1 && length(phase_max) == 1 &&
      is.finite(phase_min) && is.finite(phase_max) &&
      phase_visible_min <= phase_visible_max) {
    text(
      mean(c(phase_visible_min, phase_visible_max)),
      par("usr")[[4]] - diff(par("usr")[3:4]) * 0.045,
      labels = "EA phase-in",
      col = "#A94B32",
      cex = 0.78,
      font = 2
    )
  }

  draw_transition_marker <- function(x, label, col, y_offset = 0.08) {
    bar_width <- 0.82
    x_line <- x - bar_width / 2
    if (length(x_line) != 1 || !is.finite(x_line) || x_line < x_min || x_line > x_max) {
      return(invisible(NULL))
    }
    abline(v = x_line, col = col, lty = 2, lwd = 1.4)
    text(
      x_line + 0.18,
      par("usr")[[4]] - diff(par("usr")[3:4]) * y_offset,
      labels = label,
      adj = c(0, 1),
      col = col,
      cex = 0.78,
      font = 2
    )
  }

  bar_width <- 0.82
  for (i in seq_len(nrow(plot_data))) {
    rect(
      plot_data$birth_year[[i]] - bar_width / 2,
      min(0, plot_data$pct_loss[[i]]),
      plot_data$birth_year[[i]] + bar_width / 2,
      max(0, plot_data$pct_loss[[i]]),
      col = grDevices::adjustcolor("#CF6A37", alpha.f = 0.78),
      border = "#B85D31"
    )
  }

  eligibility_age_marker <- annotations$eligibility_age_marker
  draw_transition_marker(
    eligibility_age_marker,
    "First cohort affected\nby EA reform",
    "#6F899A",
    y_offset = 0.09
  )
}

plot_cohort_deferral_by_age <- function(cohort_deferral) {
  plot_data <- cohort_deferral[order(cohort_deferral$birth_year), ]
  y_max <- 5

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(
    bg = "#FFFFFF",
    fg = "#163E5F",
    col.axis = "#163E5F",
    col.lab = "#163E5F",
    family = "sans",
    mar = c(5.1, 7.4, 1.1, 1.1),
    mgp = c(4.4, 0.9, 0),
    las = 1
  )

  plot(
    plot_data$birth_year,
    plot_data$years_deferred,
    type = "n",
    xlim = range(plot_data$birth_year) + c(-0.5, 0.5),
    ylim = c(0, y_max),
    xlab = "",
    ylab = "Years deferred",
    bty = "l",
    axes = FALSE
  )
  axis(
    1,
    at = plot_data$birth_year,
    labels = plot_data$birth_year,
    col = "#152F43",
    col.axis = "#163E5F"
  )
  title(xlab = "Cohort birth year", line = 3.1, col.lab = "#163E5F")
  y_ticks <- pretty(c(0, y_max), n = 4)
  axis(
    2,
    at = y_ticks,
    labels = scales::comma(y_ticks, accuracy = 0.1),
    col = "#152F43",
    col.axis = "#163E5F"
  )
  grid(nx = NA, ny = NULL, col = "#D9D9D9", lty = 1)
  box(bty = "l", col = "#152F43")

  bar_width <- 0.72
  for (i in seq_len(nrow(plot_data))) {
    fill <- if (plot_data$years_deferred[[i]] > 0) {
      grDevices::adjustcolor("#163E5F", alpha.f = 0.92)
    } else {
      "#E7E1D8"
    }
    border <- if (plot_data$years_deferred[[i]] > 0) "#163E5F" else "#D5CDC2"
    rect(
      plot_data$birth_year[[i]] - bar_width / 2,
      0,
      plot_data$birth_year[[i]] + bar_width / 2,
      plot_data$years_deferred[[i]],
      col = fill,
      border = border
    )
    if (plot_data$years_deferred[[i]] > 0.01) {
      label <- if (abs(plot_data$years_deferred[[i]] - round(plot_data$years_deferred[[i]])) < 0.01) {
        scales::comma(round(plot_data$years_deferred[[i]]), accuracy = 1)
      } else {
        scales::comma(plot_data$years_deferred[[i]], accuracy = 0.1)
      }
      text(
        plot_data$birth_year[[i]],
        max(plot_data$years_deferred[[i]] - y_max * 0.06, plot_data$years_deferred[[i]] * 0.5),
        labels = label,
        adj = c(0.5, 0.5),
        cex = 0.78,
        col = "#FFFFFF",
        font = 2
      )
    }
  }
}

plot_income_test_incidence <- function(incidence, metric = "people") {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(
    bg = "#FFFFFF",
    fg = "#163E5F",
    col.axis = "#163E5F",
    col.lab = "#163E5F",
    family = "sans",
    mar = c(4.6, 10.5, 1.1, 1.1),
    mgp = c(3.2, 0.8, 0),
    las = 1
  )

  empty_income_test_panel <- function(message) {
    plot.new()
    center_x <- graphics::grconvertX(0.5, from = "ndc", to = "user")
    center_y <- graphics::grconvertY(0.5, from = "ndc", to = "user")
    box_width <- diff(graphics::grconvertX(c(0.27, 0.73), from = "ndc", to = "user"))
    box_height <- diff(graphics::grconvertY(c(0.43, 0.57), from = "ndc", to = "user"))
    old_xpd <- par("xpd")
    on.exit(par(xpd = old_xpd), add = TRUE)
    par(xpd = NA)
    rect(
      center_x - box_width / 2,
      center_y - box_height / 2,
      center_x + box_width / 2,
      center_y + box_height / 2,
      col = "#FDF7F2",
      border = "#E5CDBB",
      lwd = 1
    )
    text(center_x, center_y, message, col = "#163E5F", cex = 1.02, font = 2)
  }

  if (is.null(incidence) || nrow(incidence) == 0) {
    empty_income_test_panel("Income testing is not active")
    return(invisible(NULL))
  }

  plot_data <- incidence[incidence$estimated_people > 0, ]
  if (nrow(plot_data) == 0) {
    empty_income_test_panel("No population in selected income-test age window")
    return(invisible(NULL))
  }

  income_label <- function(lower, upper) {
    compact_dollars <- function(value) {
      if (value == 0) {
        "$0"
      } else {
        paste0("$", scales::comma(value / 1000, accuracy = 1), "k")
      }
    }
    if (!is.finite(upper)) {
      paste0(compact_dollars(lower), "+")
    } else {
      paste0(compact_dollars(lower), "-", gsub("^\\$", "", compact_dollars(upper)))
    }
  }
  plot_data$income_band <- mapply(income_label, plot_data$income_lower, plot_data$income_upper)

  if (identical(metric, "share")) {
    values <- plot_data$affected_population_share
    x_label <- "Share of selected 65+ population affected"
    axis_labels <- function(x) scales::percent(x, accuracy = 0.1)
    bar_labels <- axis_labels
  } else if (identical(metric, "percent_loss")) {
    values <- plot_data$average_percent_loss_affected
    x_label <- "Average NZS loss relative to status quo"
    axis_labels <- function(x) scales::percent(x, accuracy = 1)
    bar_labels <- axis_labels
  } else {
    values <- plot_data$estimated_people_affected
    x_label <- "Estimated people affected"
    axis_labels <- function(x) scales::comma(x, accuracy = 1)
    bar_labels <- function(x) scales::comma(x, accuracy = 1)
  }

  x_max <- max(values, na.rm = TRUE)
  if (!is.finite(x_max) || x_max <= 0) {
    x_max <- 1
  }
  xlim <- c(0, x_max * 1.16)
  y_pos <- seq_len(nrow(plot_data))
  bar_height <- 0.68

  plot(
    values,
    y_pos,
    type = "n",
    xlim = xlim,
    ylim = c(0.4, nrow(plot_data) + 0.6),
    xlab = x_label,
    ylab = "",
    axes = FALSE,
    bty = "l"
  )
  title(ylab = "Non-NZS taxable income band", line = 7.0, col.lab = "#163E5F")
  grid(nx = NULL, ny = NA, col = "#D9D9D9", lty = 1)
  axis(
    1,
    at = pretty(c(0, x_max), n = 4),
    labels = axis_labels(pretty(c(0, x_max), n = 4)),
    col = "#152F43",
    col.axis = "#163E5F"
  )
  axis(
    2,
    at = y_pos,
    labels = plot_data$income_band,
    tick = FALSE,
    col.axis = "#163E5F",
    cex.axis = 0.82
  )
  box(bty = "l", col = "#152F43")

  for (i in seq_along(values)) {
    fill <- if (values[[i]] > 0) "#CF6A37" else "#E7E1D8"
    border <- if (values[[i]] > 0) "#B85D31" else "#D5CDC2"
    rect(
      0,
      y_pos[[i]] - bar_height / 2,
      values[[i]],
      y_pos[[i]] + bar_height / 2,
      col = grDevices::adjustcolor(fill, alpha.f = 0.9),
      border = border
    )
    text(
      values[[i]] + xlim[[2]] * 0.015,
      y_pos[[i]],
      labels = bar_labels(values[[i]]),
      adj = c(0, 0.5),
      cex = 0.78,
      col = "#163E5F"
    )
  }
}

