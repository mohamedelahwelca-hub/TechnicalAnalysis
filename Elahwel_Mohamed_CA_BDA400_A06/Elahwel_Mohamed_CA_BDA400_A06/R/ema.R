ema <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0 || period < 1 || period != as.integer(period)) stop("data must be numeric and period must be a positive whole number")
  if (anyNA(data)) stop("data must not contain NA values")
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  for (i in seq_along(data)) {
    if (i == 1) ema_values[i] <- data[i]
    else ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
  }
  return(ema_values)
}
