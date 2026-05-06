# scripts/theme_functions.R
# Reusable plotting functions for a data science portfolio

library(ggplot2)

theme_portfolio <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 8)),
      plot.caption = element_text(hjust = 0),
      panel.grid.minor = element_blank()
    )
}

scale_portfolio_y_continuous <- function(labels = scales::label_number()) {
  scale_y_continuous(labels = labels)
}

add_portfolio_labels <- function(
    plot,
    title,
    subtitle = NULL,
    x = NULL,
    y = NULL,
    caption = NULL
) {
  plot +
    labs(
      title = title,
      subtitle = subtitle,
      x = x,
      y = y,
      caption = caption
    )
}


summarize_numeric <- function(data, variable) {
  x <- data[[variable]]

  data.frame(
    variable = variable,
    mean = mean(x, na.rm = TRUE),
    standard_deviation = sd(x, na.rm = TRUE),
    minimum = min(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    maximum = max(x, na.rm = TRUE)
  )
}

make_histogram <- function(data, variable) {
  ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(bins = 20, color = "white") +
    labs(
      title = paste("Distribution of", variable),
      x = variable,
      y = "Count"
    ) +
    theme_minimal()
}

make_scatterplot <- function(data, x_variable, y_variable) {
  ggplot(data, aes(x = .data[[x_variable]], y = .data[[y_variable]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste(y_variable, "by", x_variable),
      x = x_variable,
      y = y_variable
    ) +
    theme_minimal()
}

fit_simple_regression <- function(data, outcome, predictor) {
  model_formula <- as.formula(
    paste(outcome, "~", predictor)
  )

  lm(model_formula, data = data)
}

summarize_model <- function(model) {
  coefficients <- summary(model)$coefficients

  data.frame(
    term = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    standard_error = coefficients[, "Std. Error"],
    test_statistic = coefficients[, "t value"],
    p_value = coefficients[, "Pr(>|t|)"],
    row.names = NULL
  )
}