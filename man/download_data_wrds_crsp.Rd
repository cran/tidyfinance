% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_data_wrds_crsp.R
\name{download_data_wrds_crsp}
\alias{download_data_wrds_crsp}
\title{Download Data from WRDS CRSP}
\usage{
download_data_wrds_crsp(
  type,
  start_date = NULL,
  end_date = NULL,
  batch_size = 500,
  version = "v2",
  additional_columns = NULL
)
}
\arguments{
\item{type}{A string specifying the type of CRSP data to download:
"crsp_monthly" or "crsp_daily".}

\item{start_date}{Optional. A character string or Date object in "YYYY-MM-DD" format
specifying the start date for the data. If not provided, a subset of the dataset is returned.}

\item{end_date}{Optional. A character string or Date object in "YYYY-MM-DD" format
specifying the end date for the data. If not provided, a subset of the dataset is returned.}

\item{batch_size}{An optional integer specifying the batch size for
processing daily data, with a default of 500.}

\item{version}{An optional character specifying which CRSP version to use.
"v2" (the default) uses the updated second version of CRSP, and "v1"
downloads the legacy version of CRSP.}

\item{additional_columns}{Additional columns from the CRSP monthly or
daily data as a character vector.}
}
\value{
A data frame containing CRSP stock returns, adjusted for delistings,
along with calculated market capitalization and excess returns over the
risk-free rate. The structure of the returned data frame depends on the
selected data type.
}
\description{
This function downloads and processes stock return data from the CRSP
database for a specified period. Users can choose between monthly and daily
data types. The function also adjusts returns for delisting and calculates
market capitalization and excess returns over the risk-free rate.
}
\examples{
\dontrun{
  crsp_monthly <- download_data_wrds_crsp("wrds_crsp_monthly", "2020-11-01", "2020-12-31")
  crsp_daily <- download_data_wrds_crsp("wrds_crsp_daily", "2020-12-01", "2020-12-31")

  # Add additional columns
  download_data_wrds_crsp("wrds_crsp_monthly", "2020-11-01", "2020-12-31",
                          additional_columns = c("mthvol", "mthvolflg"))
}
}
