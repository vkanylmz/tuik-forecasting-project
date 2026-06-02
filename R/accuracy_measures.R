# accuracy_measures.R
# Purpose: Calculate forecast accuracy and monitoring measures.

accuracy_measures <- function(actual, forecast) {
  actual <- as.numeric(actual)
  forecast <- as.numeric(forecast)

  valid <- is.finite(actual) & is.finite(forecast)
  actual <- actual[valid]
  forecast <- forecast[valid]

  if (length(actual) == 0) {
    return(data.frame(
      Bias = NA_real_, MAD = NA_real_, MSE = NA_real_, MAPE = NA_real_,
      RSFE = NA_real_, Tracking_Signal = NA_real_
    ))
  }

  error <- actual - forecast
  bias <- mean(error)
  mad <- mean(abs(error))
  mse <- mean(error^2)
  mape <- mean(abs(error / actual), na.rm = TRUE) * 100
  rsfe <- sum(error)
  tracking_signal <- ifelse(isTRUE(all.equal(mad, 0)), NA_real_, rsfe / mad)

  data.frame(
    Bias = bias,
    MAD = mad,
    MSE = mse,
    MAPE = mape,
    RSFE = rsfe,
    Tracking_Signal = tracking_signal
  )
}
