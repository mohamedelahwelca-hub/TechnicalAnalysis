stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)
  stoch_values <- rep(NA_real_, length(rsi_values))
  first_valid <- period + 1
  if (k_period < 1 || d_period < 1) stop("k_period and d_period must be positive")
  for (i in seq_along(rsi_values)) {
    if (i >= first_valid + k_period - 1) {
      window <- rsi_values[(i - k_period + 1):i]
      if (!anyNA(window)) {
        low_rsi <- min(window); high_rsi <- max(window)
        if (high_rsi == low_rsi) stoch_values[i] <- 0
        else stoch_values[i] <- (rsi_values[i] - low_rsi) / (high_rsi - low_rsi)
      }
    }
  }
  k_line <- stoch_values
  d_line <- rep(NA_real_, length(k_line))
  for (i in seq_along(k_line)) {
    if (i >= d_period && !anyNA(k_line[(i - d_period + 1):i])) d_line[i] <- mean(k_line[(i - d_period + 1):i])
  }
  return(list(k_line = k_line, d_line = d_line))
}
