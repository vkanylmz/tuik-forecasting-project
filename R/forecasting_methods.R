# forecasting_methods.R
# Purpose: Run required forecasting methods for the quarterly domestic tourism series.

run_forecasting_methods <- function(data) {
  data <- data |>
    dplyr::arrange(Year, Quarter) |>
    dplyr::mutate(Time_Index = dplyr::row_number())

  y <- as.numeric(data$Trips)
  start_year <- min(data$Year)
  start_quarter <- data$Quarter[which.min(data$Time_Index)]
  ts_trips <- stats::ts(y, start = c(start_year, start_quarter), frequency = 4)

  n <- length(y)
  next_year <- data$Year[n]
  next_quarter <- data$Quarter[n] + 1
  if (next_quarter == 5) {
    next_quarter <- 1
    next_year <- next_year + 1
  }
  target_period <- paste0(next_year, " Q", next_quarter)

  model_data <- data |>
    dplyr::mutate(
      Naive = dplyr::lag(Trips, 1),
      Moving_Average = NA_real_,
      Weighted_Moving_Average = NA_real_
    )

  # Four-quarter moving average: forecast current quarter from previous four quarters.
  if (n >= 5) {
    for (i in 5:n) {
      model_data$Moving_Average[i] <- mean(y[(i - 4):(i - 1)], na.rm = TRUE)
      model_data$Weighted_Moving_Average[i] <-
        0.40 * y[i - 1] + 0.30 * y[i - 2] + 0.20 * y[i - 3] + 0.10 * y[i - 4]
    }
  }

  next_ma <- mean(tail(y, 4), na.rm = TRUE)
  next_wma <- 0.40 * y[n] + 0.30 * y[n - 1] + 0.20 * y[n - 2] + 0.10 * y[n - 3]

  ses_fit <- forecast::ses(ts_trips, h = 1)
  holt_fit <- forecast::holt(ts_trips, h = 1)

  model_data$Exponential_Smoothing <- as.numeric(stats::fitted(ses_fit))
  model_data$Trend_Adjusted_Exponential_Smoothing <- as.numeric(stats::fitted(holt_fit))

  trend_model <- stats::lm(Trips ~ Time_Index, data = model_data)
  model_data$Linear_Trend_Projection <- as.numeric(stats::fitted(trend_model))
  next_trend <- as.numeric(stats::predict(trend_model, newdata = data.frame(Time_Index = n + 1)))

  # Seasonal indices using multiplicative trend-ratio approach.
  seasonal_trend_model <- stats::lm(Trips ~ Time_Index, data = model_data)
  trend_fitted <- as.numeric(stats::fitted(seasonal_trend_model))
  seasonal_raw <- model_data$Trips / trend_fitted
  seasonal_index_table <- model_data |>
    dplyr::mutate(Seasonal_Ratio = seasonal_raw) |>
    dplyr::group_by(Quarter) |>
    dplyr::summarise(Seasonal_Index = mean(Seasonal_Ratio, na.rm = TRUE), .groups = "drop")
  seasonal_index_table$Seasonal_Index <- seasonal_index_table$Seasonal_Index / mean(seasonal_index_table$Seasonal_Index, na.rm = TRUE)

  model_data <- model_data |>
    dplyr::left_join(seasonal_index_table, by = "Quarter") |>
    dplyr::mutate(Seasonal_Indices = trend_fitted * Seasonal_Index)

  next_seasonal_index <- seasonal_index_table$Seasonal_Index[seasonal_index_table$Quarter == next_quarter]
  next_seasonal_indices <- next_trend * next_seasonal_index

  # Classical decomposition, available because the series is quarterly and has multiple seasonal cycles.
  additive_decomp <- stats::decompose(ts_trips, type = "additive")
  multiplicative_decomp <- stats::decompose(ts_trips, type = "multiplicative")

  model_data$Additive_Decomposition <- as.numeric(additive_decomp$trend + additive_decomp$seasonal)
  model_data$Multiplicative_Decomposition <- as.numeric(multiplicative_decomp$trend * multiplicative_decomp$seasonal)

  # Forecast the next period using ETS as the extrapolation engine for decomposition-based methods.
  additive_forecast <- forecast::forecast(forecast::ets(ts_trips), h = 1)
  multiplicative_forecast <- forecast::forecast(forecast::ets(ts_trips, model = "ZZZ"), h = 1)

  next_additive <- as.numeric(additive_forecast$mean[1])
  next_multiplicative <- as.numeric(multiplicative_forecast$mean[1])

  # Regression with trend and quarterly seasonal dummy variables.
  seasonal_dummy_model <- stats::lm(Trips ~ Time_Index + factor(Quarter), data = model_data)
  model_data$Regression_Trend_Seasonal_Dummies <- as.numeric(stats::fitted(seasonal_dummy_model))
  next_regression <- as.numeric(stats::predict(
    seasonal_dummy_model,
    newdata = data.frame(Time_Index = n + 1, Quarter = next_quarter)
  ))

  next_forecasts <- data.frame(
    Method = c(
      "Naive Forecasting",
      "Moving Average",
      "Weighted Moving Average",
      "Exponential Smoothing",
      "Trend-Adjusted Exponential Smoothing",
      "Linear Trend Projection",
      "Seasonal Indices",
      "Additive Decomposition",
      "Multiplicative Decomposition",
      "Regression with Trend and Seasonal Dummies"
    ),
    Next_Period_Forecast = c(
      y[n],
      next_ma,
      next_wma,
      as.numeric(ses_fit$mean[1]),
      as.numeric(holt_fit$mean[1]),
      next_trend,
      next_seasonal_indices,
      next_additive,
      next_multiplicative,
      next_regression
    )
  )

  list(
    data_with_fits = model_data,
    ts_trips = ts_trips,
    next_forecasts = next_forecasts,
    target_period = target_period,
    next_year = next_year,
    next_quarter = next_quarter,
    ses_fit = ses_fit,
    holt_fit = holt_fit,
    trend_model = trend_model,
    seasonal_index_table = seasonal_index_table,
    additive_decomp = additive_decomp,
    multiplicative_decomp = multiplicative_decomp,
    seasonal_dummy_model = seasonal_dummy_model
  )
}
