sma <- function(data, period) {
  if (!is.numeric(data) || length(data) < period || period < 1 || period != as.integer(period)) stop("Data length must be at least a positive whole-number period")
  if (anyNA(data)) stop("data must not contain NA values")
  sma_values <- numeric(length(data) - period + 1)
  for (i in seq_along(sma_values)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  return(sma_values)
}
