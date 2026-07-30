plot_person_working_life <- function(profile) {
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

  y_max <- 150000
  plot(
    profile$age,
    profile$real_annual_earnings,
    type = "n",
    ylim = c(0, y_max),
    xlab = "",
    ylab = "Annual amount ($ real 2026)",
    bty = "l",
    axes = FALSE
  )
  x_ticks <- c(seq(20, 60, by = 5), 65)
  axis(1, at = x_ticks, labels = x_ticks, col = "#152F43", col.axis = "#163E5F")
  title(xlab = "Age", line = 3.1, col.lab = "#163E5F")
  y_ticks <- axTicks(2)
  axis(2, at = y_ticks, labels = scales::dollar(y_ticks, accuracy = 1), col = "#152F43", col.axis = "#163E5F")
  grid(nx = NA, ny = NULL, col = "#D9D9D9", lty = 1)
  box(bty = "l", col = "#152F43")

  context <- profile[!profile$active, ]
  active <- profile[profile$active, ]

  if (nrow(active) > 0) {
    employee_bottom <- pmax(active$real_annual_earnings - active$real_employee_contribution, 0)
    polygon(
      c(active$age, rev(active$age)),
      c(employee_bottom, rev(active$real_annual_earnings)),
      col = grDevices::adjustcolor("#F26B44", alpha.f = 0.50),
      border = NA
    )
    polygon(
      c(active$age, rev(active$age)),
      c(active$real_annual_earnings, rev(active$real_annual_earnings + active$real_employer_net_contribution)),
      col = grDevices::adjustcolor("#2F6F73", alpha.f = 0.58),
      border = NA
    )
  }

  current_age <- min(active$age, na.rm = TRUE)
  if (is.finite(current_age)) {
    abline(v = current_age, col = "#6F899A", lty = 2, lwd = 1.4)
  }

  lines(profile$age, profile$real_annual_earnings, col = "#CDD4D9", lwd = 1.4)
  if (nrow(active) > 0) {
    lines(active$age, active$real_annual_earnings, col = "#163E5F", lwd = 3.0)
  }
  hw_mixed_legend(
    "bottomright",
    inset = c(0.02, 0.08),
    labels = c("Gross wage/salary", "Employee contribution", "Net employer contribution"),
    key_type = c("line", "fill", "fill"),
    col = c("#163E5F", NA, NA),
    fill = c(NA, grDevices::adjustcolor("#F26B44", alpha.f = 0.50), grDevices::adjustcolor("#2F6F73", alpha.f = 0.58)),
    lwd = c(3.0, NA, NA),
    cex = 0.88
  )
}

plot_person_balance_path <- function(balance_profile, target_balance) {
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

  if (nrow(balance_profile) == 0) {
    plot.new()
    text(0.5, 0.5, "No pre-65 contribution years", col = "#163E5F")
    return(invisible(NULL))
  }

  y_max <- max(balance_profile$real_balance, target_balance, 1, na.rm = TRUE) * 1.12
  plot(
    balance_profile$age,
    balance_profile$real_balance,
    type = "n",
    xlim = c(20, 65),
    ylim = c(0, y_max),
    xlab = "",
    ylab = "Balance at year-end ($ real 2026)",
    bty = "l",
    axes = FALSE
  )
  x_ticks <- seq(20, 65, by = 5)
  axis(1, at = x_ticks, labels = x_ticks, col = "#152F43", col.axis = "#163E5F")
  title(xlab = "Age", line = 3.1, col.lab = "#163E5F")
  y_ticks <- axTicks(2)
  axis(2, at = y_ticks, labels = scales::dollar(y_ticks, accuracy = 1), col = "#152F43", col.axis = "#163E5F")
  grid(nx = NA, ny = NULL, col = "#D9D9D9", lty = 1)
  box(bty = "l", col = "#152F43")
  current_age <- min(balance_profile$age, na.rm = TRUE)
  if (is.finite(current_age)) {
    abline(v = current_age, col = "#6F899A", lty = 2, lwd = 1.4)
  }
  lines(balance_profile$age, balance_profile$real_balance, col = "#163E5F", lwd = 2)
  abline(h = target_balance, col = "#F26B44", lwd = 2, lty = 2)
  legend(
    "topleft",
    inset = c(0.02, 0.05),
    legend = c("Projected KiwiSaver balance", "Balance that would\noffset NZS reform"),
    col = c("#163E5F", "#F26B44"),
    lwd = 2,
    lty = c(1, 2),
    bty = "n",
    text.col = "#163E5F",
    cex = 0.88
  )
}

plot_person_retirement_income <- function(retirement_income) {
  plot_data <- retirement_income[retirement_income$age <= 90, ]
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

  stacks <- data.frame(
    `Non-NZS income` = plot_data$non_nzs_income_real,
    `KiwiSaver drawdown` = plot_data$kiwisaver_drawdown_real,
    `NZS under reform` = plot_data$reform_nzs_real,
    check.names = FALSE
  )
  total <- rowSums(stacks, na.rm = TRUE)
  y_max <- max(total, plot_data$status_quo_total_real, 1, na.rm = TRUE) * 1.22
  plot(
    plot_data$age,
    total,
    type = "n",
    xlim = c(65, 90),
    ylim = c(0, y_max),
    xlab = "",
    ylab = "Annual retirement income ($ real 2026)",
    bty = "l",
    axes = FALSE
  )
  x_ticks <- seq(65, 90, by = 5)
  axis(1, at = x_ticks, labels = x_ticks, col = "#152F43", col.axis = "#163E5F")
  title(xlab = "Age", line = 3.1, col.lab = "#163E5F")
  y_ticks <- axTicks(2)
  axis(2, at = y_ticks, labels = scales::dollar(y_ticks, accuracy = 1), col = "#152F43", col.axis = "#163E5F")
  grid(nx = NA, ny = NULL, col = "#D9D9D9", lty = 1)
  box(bty = "l", col = "#152F43")

  colours <- c("#2F6F73", "#F26B44", "#8C4A7C")
  lower <- rep(0, nrow(plot_data))
  for (i in seq_along(stacks)) {
    upper <- lower + stacks[[i]]
    polygon(
      c(plot_data$age, rev(plot_data$age)),
      c(lower, rev(upper)),
      col = grDevices::adjustcolor(colours[[i]], alpha.f = 0.50),
      border = NA
    )
    lower <- upper
  }
  lines(plot_data$age, plot_data$status_quo_total_real, col = "#163E5F", lwd = 2.4, lty = 2)
  lines(plot_data$age, plot_data$reform_total_real, col = "#152F43", lwd = 2.8)
  hw_mixed_legend(
    "topright",
    labels = c(names(stacks), "Status quo total (statutory KiwiSaver)", "Total under reform"),
    key_type = c(rep("fill", length(stacks)), "line", "line"),
    fill = c(grDevices::adjustcolor(colours, alpha.f = 0.50), NA, NA),
    lty = c(rep(1, 3), 2, 1),
    lwd = c(rep(NA, 3), 2.4, 2.8),
    col = c(rep(NA, 3), "#163E5F", "#152F43"),
    cex = 0.82
  )
}
