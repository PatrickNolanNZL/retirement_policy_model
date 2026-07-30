hw_chart_colours <- function(n) {
  rep(c("#163E5F", "#2F6F73", "#F26B44", "#8C4A7C"), length.out = n)
}

hw_mixed_legend <- function(
    position,
    labels,
    key_type,
    fill = rep(NA_character_, length(labels)),
    col = rep(NA_character_, length(labels)),
    lty = rep(1, length(labels)),
    lwd = rep(2, length(labels)),
    cex = 0.86,
    text_col = "#163E5F",
    inset = c(0.02, 0.03)) {
  usr <- par("usr")
  x_range <- diff(usr[1:2])
  y_range <- diff(usr[3:4])
  row_height <- strheight("M", cex = cex) * 1.45
  key_width <- x_range * 0.045
  key_height <- row_height * 0.42
  gap <- x_range * 0.012
  text_width <- max(strwidth(labels, cex = cex), na.rm = TRUE)
  legend_width <- key_width + gap + text_width
  legend_height <- row_height * length(labels)

  if (grepl("right", position)) {
    x0 <- usr[2] - inset[[1]] * x_range - legend_width
  } else {
    x0 <- usr[1] + inset[[1]] * x_range
  }
  if (grepl("bottom", position)) {
    y_top <- usr[3] + inset[[2]] * y_range + legend_height
  } else {
    y_top <- usr[4] - inset[[2]] * y_range
  }

  for (i in seq_along(labels)) {
    y <- y_top - (i - 0.5) * row_height
    if (identical(key_type[[i]], "fill")) {
      rect(
        x0,
        y - key_height / 2,
        x0 + key_width,
        y + key_height / 2,
        col = fill[[i]],
        border = NA
      )
    } else {
      segments(x0, y, x0 + key_width, y, col = col[[i]], lty = lty[[i]], lwd = lwd[[i]])
    }
    text(x0 + key_width + gap, y, labels[[i]], adj = c(0, 0.5), col = text_col, cex = cex)
  }
}
