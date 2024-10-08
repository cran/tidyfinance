% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_model.R
\name{estimate_model}
\alias{estimate_model}
\title{Estimate Model Coefficients}
\usage{
estimate_model(data, model, min_obs = 1)
}
\arguments{
\item{data}{A data frame containing the dependent variable and one or more
independent variables.}

\item{model}{A character that describes the model to estimate (e.g.
\code{"ret_excess ~ mkt_excess + hmb + sml"}).}

\item{min_obs}{The minimum number of observations required to estimate the
model. Defaults to 1.}
}
\value{
A data frame with a row for each coefficient and
column names corresponding to the independent variables.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function estimates the coefficients of a linear model specified by one
or more independent variables. It checks for the presence of the specified
independent variables in the dataset and whether the dataset has a sufficient
number of observations. It returns the model's coefficients as either a
numeric value (for a single independent variable) or a data frame (for
multiple independent variables).
}
\examples{
data <- data.frame(
  ret_excess = rnorm(100),
  mkt_excess = rnorm(100),
  smb = rnorm(100),
  hml = rnorm(100)
)

# Estimate model with a single independent variable
estimate_model(data, "ret_excess ~ mkt_excess")

# Estimate model with multiple independent variables
estimate_model(data, "ret_excess ~ mkt_excess + smb + hml")

# Estimate model without intercept
estimate_model(data, "ret_excess ~ mkt_excess - 1")

}
\seealso{
\code{\link[stats:lm]{stats::lm()}} for details on the underlying linear model fitting used.
}
