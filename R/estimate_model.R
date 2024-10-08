#' Estimate Model Coefficients
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function estimates the coefficients of a linear model specified by one
#' or more independent variables. It checks for the presence of the specified
#' independent variables in the dataset and whether the dataset has a sufficient
#' number of observations. It returns the model's coefficients as either a
#' numeric value (for a single independent variable) or a data frame (for
#' multiple independent variables).
#'
#' @param data A data frame containing the dependent variable and one or more
#'   independent variables.
#' @param model A character that describes the model to estimate (e.g.
#'   `"ret_excess ~ mkt_excess + hmb + sml"`).
#' @param min_obs The minimum number of observations required to estimate the
#'   model. Defaults to 1.
#'
#' @returns A data frame with a row for each coefficient and
#'   column names corresponding to the independent variables.
#'
#' @seealso [stats::lm()] for details on the underlying linear model fitting used.
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   ret_excess = rnorm(100),
#'   mkt_excess = rnorm(100),
#'   smb = rnorm(100),
#'   hml = rnorm(100)
#' )
#'
#' # Estimate model with a single independent variable
#' estimate_model(data, "ret_excess ~ mkt_excess")
#'
#' # Estimate model with multiple independent variables
#' estimate_model(data, "ret_excess ~ mkt_excess + smb + hml")
#'
#' # Estimate model without intercept
#' estimate_model(data, "ret_excess ~ mkt_excess - 1")
#'
estimate_model <- function(data, model, min_obs = 1) {
  model_parts <- strsplit(model, "~", fixed = TRUE)[[1]]
  response_var <- trimws(model_parts[1])
  independent_vars <- strsplit(trimws(model_parts[2]), "[ +]")[[1]]
  independent_vars <- independent_vars[nzchar(independent_vars)]
  independent_vars <- independent_vars[!independent_vars %in% c("-", "1")]

  if ("intercept" %in% independent_vars) {
    cli::cli_abort(
      "None of the columns in {.arg model} may be called 'intercept'. Please rename the column and try again."
    )
  }

  if (nrow(data) < min_obs || nrow(data) <= length(independent_vars)) {
    if (length(independent_vars) == 0) {
      return(NA_real_)
    }
    beta <- setNames(rep(NA_real_, length(independent_vars)), independent_vars)
    return(as_tibble(t(beta)))
  }

  missing_vars <- independent_vars[!independent_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    cli::cli_abort(
      "The following independent variables are missing in the data: {toString(missing_vars)}."
    )
  }

  fit <- stats::lm(stats::as.formula(model), data = data)
  beta <- stats::coef(fit)

  if ("(Intercept)" %in% names(beta)) {
    names(beta)[names(beta) == "(Intercept)"] <- "intercept"
  }

  as_tibble(t(beta))
}
