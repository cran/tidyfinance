#' Trim a Numeric Vector
#'
#' Removes the values in a numeric vector that are beyond the specified
#' quantiles, effectively trimming the distribution based on the `cut`
#' parameter. This process reduces the length of the vector, excluding extreme
#' values from both tails of the distribution.
#'
#' @param x A numeric vector to be trimmed.
#' @param cut The proportion of data to be trimmed from both ends of the
#'   distribution. For example, a `cut` of 0.05 will remove the lowest and
#'   highest 5% of the data. Must be between \[0, 0.5\].
#'
#' @returns A numeric vector with the extreme values removed.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(100)
#' trimmed_data <- trim(x = data, cut = 0.05)
#'
trim <- function(x, cut) {
  if (cut < 0 || cut > 0.5) {
    cli::cli_abort("{.arg cut} must be inside [0, 0.5].")
  }

  lb <- quantile(x, cut, na.rm = TRUE)
  up <- quantile(x, 1 - cut, na.rm = TRUE)
  x <- replace(x, x > up, NA_real_)
  x <- replace(x, x < lb, NA_real_)
  x
}
