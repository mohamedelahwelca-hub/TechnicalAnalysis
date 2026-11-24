linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)
  if (!is.numeric(regressionSource) || anyNA(regressionSource)) stop("regressionSource must be numeric and contain no NA values")
  if (regressionLength < 2 || regressionLength > n) stop("regressionLength must be at least 2 and no greater than source length")
  if (regressionOffset < 0 || regressionOffset >= regressionLength) stop("regressionOffset must be non-negative and less than regressionLength")
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  if (denominator == 0) stop("Regression requires at least two points")
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept
  result <- list(slope = slope, intercept = intercept, predicted_values = predicted_values)
  return(result)
}
