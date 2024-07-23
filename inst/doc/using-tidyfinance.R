## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#  install.packages("tidyfinance")

## -----------------------------------------------------------------------------
#  pak::pak("tidy-finance/r-tidyfinance")

## -----------------------------------------------------------------------------
library(tidyfinance)

## -----------------------------------------------------------------------------
list_supported_types()

## -----------------------------------------------------------------------------
download_data("factors_ff_3_monthly", "2020-01-01", "2020-12-31")

## -----------------------------------------------------------------------------
download_data("factors_q5_daily", "2020-01-01", "2020-12-31")

## -----------------------------------------------------------------------------
#  download_data("wrds_crsp_monthly", "2020-01-01", "2020-12-31")

## -----------------------------------------------------------------------------
#  download_data_wrds_crsp("wrds_crsp_monthly", "2020-01-01", "2020-12-31", additional_columns = "mthvol")

## -----------------------------------------------------------------------------
#  download_data_wrds_compustat("wrds_compustat_annual", "2000-01-01", "2020-12-31", additional_columns = c("acoxar", "amc", "aldo"))

## -----------------------------------------------------------------------------
list_tidy_finance_chapters()

## -----------------------------------------------------------------------------
#  open_tidy_finance_website("beta-estimation")

## -----------------------------------------------------------------------------
library(dplyr)

set.seed(123)
data <- tibble(x = rnorm(100)) |>
  arrange(x)

data |>
  mutate(x_winsorized = winsorize(x, 0.01))

## -----------------------------------------------------------------------------
data |>
  mutate(x_trimmed = trim(x, 0.01))

## -----------------------------------------------------------------------------
create_summary_statistics(data, x, detail = TRUE)

## -----------------------------------------------------------------------------
data <- tibble(
  id = 1:100,
  exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
  market_cap = runif(100, 1e6, 1e9)
)

data |>
  mutate(
    portfolio = assign_portfolio(
      pick(everything()), "market_cap", n_portfolios = 5, exchanges = "NYSE")
  )

## -----------------------------------------------------------------------------
data <- tibble(
  ret_excess = rnorm(100),
  mkt_excess = rnorm(100),
  smb = rnorm(100),
  hml = rnorm(100)
)

estimate_model(data, "ret_excess ~ mkt_excess + smb + hml")

