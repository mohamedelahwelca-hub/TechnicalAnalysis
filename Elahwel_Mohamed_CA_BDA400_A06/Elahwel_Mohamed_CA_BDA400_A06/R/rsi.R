rsi <- function(data, period) {
  if (!is.numeric(data) || length(data) <= period || period < 1 || period != as.integer(period)) stop("data length must exceed a positive whole-number period")
  if (anyNA(data)) stop("data must not contain NA values")
  diff_values <- diff(data)
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))
  for (i in (period + 1):length(data)) {
    if (i > period + 1) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    }
    if (avg_loss == 0 && avg_gain == 0) rsi_values[i] <- 50
    else if (avg_loss == 0) rsi_values[i] <- 100
    else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  return(rsi_values)
}
