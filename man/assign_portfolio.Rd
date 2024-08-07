% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_portfolio.R
\name{assign_portfolio}
\alias{assign_portfolio}
\title{Assign Portfolios Based on Sorting Variable}
\usage{
assign_portfolio(
  data,
  sorting_variable,
  n_portfolios = NULL,
  percentiles = NULL,
  exchanges = NULL
)
}
\arguments{
\item{data}{A data frame containing the dataset for portfolio assignment.}

\item{sorting_variable}{A string specifying the column name in \code{data} to be
used for sorting and determining portfolio assignments.}

\item{n_portfolios}{An optional integer specifying the number of equally
sized portfolios to create. This parameter is mutually exclusive with
\code{percentiles}.}

\item{percentiles}{An optional numeric vector specifying the percentiles for
determining the breakpoints of the portfolios. This parameter is mutually
exclusive with \code{n_portfolios}.}

\item{exchanges}{An optional character vector specifying exchange names to
filter the data before computing breakpoints and assigning portfolios.
Exchanges must be stored in a column named \code{exchange} in \code{data}. If \code{NULL},
no filtering is applied.}
}
\value{
A vector of portfolio assignments for each row in the input \code{data}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function assigns data points to portfolios based on a specified sorting
variable. It can optionally filter the data by exchanges before assignment.
The function requires either the number of portfolios to be created or
specific percentiles for the breakpoints, but not both.
}
\note{
This function will stop and throw an error if both \code{n_portfolios} and
\code{percentiles} are provided or if neither is provided. Ensure that you only
use one of these parameters for specifying portfolio breakpoints.
}
\examples{
data <- data.frame(
  id = 1:100,
  exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
  market_cap = runif(100, 1e6, 1e9)
)
assign_portfolio(data, "market_cap", n_portfolios = 5)
assign_portfolio(data, "market_cap", percentiles = c(0.2, 0.4, 0.6, 0.8), exchanges = c("NYSE"))
}
