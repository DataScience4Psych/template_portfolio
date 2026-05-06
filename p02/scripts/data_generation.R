output_file <- "p02/data/portfolio_example_data.csv"

# -------------------------------------------------------------------------
# Generate simulated air-quality data
# -------------------------------------------------------------------------

n_days <- 200

date <- seq.Date(
  from = as.Date("2025-01-01"),
  by = "day",
  length.out = n_days
)

neighborhood <- sample(
  c("North", "South", "East", "West", "Central"),
  size = n_days,
  replace = TRUE
)

temperature_f <- round(
  rnorm(n_days, mean = 68, sd = 12),
  1
)

humidity_percent <- round(
  pmin(
    pmax(
      rnorm(n_days, mean = 60, sd = 15),
      20
    ),
    100
  ),
  1
)

wind_speed_mph <- round(
  pmax(
    rnorm(n_days, mean = 8, sd = 3),
    0
  ),
  1
)

traffic_index <- round(
  pmin(
    pmax(
      rnorm(n_days, mean = 50, sd = 18),
      0
    ),
    100
  ),
  1
)

green_space_percent <- round(
  pmin(
    pmax(
      rnorm(n_days, mean = 30, sd = 12),
      0
    ),
    80
  ),
  1
)

pm25 <- round(
  pmax(
    4 +
      0.12 * temperature_f +
      0.08 * humidity_percent +
      0.18 * traffic_index -
      0.25 * wind_speed_mph -
      0.10 * green_space_percent +
      rnorm(n_days, mean = 0, sd = 4),
    1
  ),
  1
)

air_quality_category <- ifelse(
  pm25 < 12,
  "Good",
  ifelse(pm25 < 35.5, "Moderate", "Unhealthy")
)

# -------------------------------------------------------------------------
# Combine variables into a data frame
# -------------------------------------------------------------------------

portfolio_data <- data.frame(
  date = date,
  neighborhood = neighborhood,
  temperature_f = temperature_f,
  humidity_percent = humidity_percent,
  wind_speed_mph = wind_speed_mph,
  traffic_index = traffic_index,
  green_space_percent = green_space_percent,
  pm25 = pm25,
  air_quality_category = air_quality_category
)

# -------------------------------------------------------------------------
# Write data to CSV
# -------------------------------------------------------------------------

write.csv(
  portfolio_data,
  file = output_file,
  row.names = FALSE
)