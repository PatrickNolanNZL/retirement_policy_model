format_dollar <- function(x) {
  scales::dollar(x, accuracy = 1)
}

format_percent <- function(x, accuracy = 0.1) {
  scales::percent(x, accuracy = accuracy)
}

format_billion <- function(x) {
  paste0("$", scales::comma(x, accuracy = 0.1), "b")
}

format_dollar_k <- function(x) {
  accuracy <- ifelse(abs(x) < 1000, 0.1, 1)
  paste0("$", scales::comma(x / 1000, accuracy = accuracy), "k")
}

parse_numeric_input <- function(x) {
  cleaned <- gsub("[,$%[:space:]]", "", x)
  value <- suppressWarnings(as.numeric(cleaned))
  ifelse(is.na(value), 0, value)
}

parse_dollar_input <- function(x) {
  parse_numeric_input(x)
}

parse_percent_point_input <- function(x) {
  parse_numeric_input(x) / 100
}
