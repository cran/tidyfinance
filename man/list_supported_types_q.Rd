% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_supported_types.R
\name{list_supported_types_q}
\alias{list_supported_types_q}
\title{List Supported Global Q Dataset Types}
\usage{
list_supported_types_q()
}
\value{
A tibble with columns: \code{type} (the type of dataset), \code{dataset_name}
(the file name of the dataset), and \code{domain} (the domain to which the
dataset belongs, always "Global Q").
}
\description{
This function returns a tibble with the supported Global Q dataset types,
including their names and frequencies (daily, weekly, weekly week-to-week,
monthly, quarterly, annual). Each dataset type is associated with the Global
Q model, specifically the q5 factors model for the year 2023. Additionally,
it annotates each dataset with the domain "Global Q".
}
