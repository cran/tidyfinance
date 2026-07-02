## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(tidyfinance)
library(dplyr)
library(tidyr)
library(lubridate)

## -----------------------------------------------------------------------------
date_start <- as.Date("1972-01-01")
date_end <- as.Date("2024-12-31")

## -----------------------------------------------------------------------------
crsp_monthly <- download_data(
  domain = "Pseudo Data",
  dataset = "crsp_monthly",
  start_date = date_start,
  end_date = date_end,
  add_ccm_links = TRUE
)
crsp_monthly

## -----------------------------------------------------------------------------
compustat_annual <- download_data(
  domain = "Pseudo Data",
  dataset = "compustat_annual",
  start_date = date_start,
  end_date = date_end,
  additional_columns = c("at", "ib"),
  only_usd = TRUE
) |>
  select(gvkey, date, at, ib)
compustat_annual

## -----------------------------------------------------------------------------
sorting_variable_data <- compustat_annual |>
  add_lagged_columns(
    cols = "at",
    lag = years(1),
    by = "gvkey"
  ) |>
  mutate(
    asset_growth = (at - at_lag) / at_lag,
    asset_growth = if_else(is.finite(asset_growth), asset_growth, NA_real_)
  ) |>
  group_by(date) |>
  mutate(asset_growth = winsorize(asset_growth, cut = 0.01)) |>
  ungroup() |>
  select(gvkey, date, asset_growth, ib)

## -----------------------------------------------------------------------------
sorting_variable_data |>
  drop_na(asset_growth) |>
  create_summary_statistics(asset_growth, detail = TRUE) |>
  select(variable, n, mean, sd, q05, q50, q95) |>
  knitr::kable(
    digits = 3,
    caption = "Cross-sectional distribution of asset growth."
  )

## -----------------------------------------------------------------------------
sorting_data <- crsp_monthly |>
  join_lagged_values(
    new_data = sorting_variable_data,
    id_keys = "gvkey",
    min_lag = months(7),
    max_lag = months(18),
    ff_adjustment = TRUE
  ) |>
  select(
    date,
    permno,
    exchange,
    siccd,
    ret_excess,
    mktcap_lag,
    asset_growth,
    ib
  ) |>
  filter(date >= date_start + months(12) + months(18))

## -----------------------------------------------------------------------------
options_baseline <- portfolio_sort_options(
  breakpoint_options_main = breakpoint_options(
    n_portfolios = 5,
    breakpoints_exchanges = "NYSE"
  )
)

portfolio_returns <- implement_portfolio_sort(
  data = sorting_data,
  sorting_variables = "asset_growth",
  sorting_method = "univariate",
  rebalancing_month = 7,
  portfolio_sort_options = options_baseline
)
portfolio_returns

## -----------------------------------------------------------------------------
factor_returns <- portfolio_returns |>
  compute_long_short_returns(direction = "bottom_minus_top")

## -----------------------------------------------------------------------------
portfolio_summary <- bind_rows(
  portfolio_returns |>
    mutate(portfolio = as.character(portfolio)),
  factor_returns |>
    mutate(portfolio = "Long-short")
) |>
  group_by(portfolio) |>
  summarize(
    mean_vw = mean(ret_excess_vw, na.rm = TRUE) * 12,
    mean_ew = mean(ret_excess_ew, na.rm = TRUE) * 12,
    .groups = "drop"
  )

portfolio_summary |>
  knitr::kable(
    digits = 4,
    col.names = c("Portfolio", "Value-weighted", "Equal-weighted"),
    caption = paste(
      "Annualized mean excess returns by asset growth quintile, plus the",
      "long-short spread. On real data the value-weighted mean declines",
      "across quintiles and the long-short row is a sizable premium; here",
      "the quintile ordering and the long-short value (and its sign) are",
      "sampling noise."
    )
  )

## -----------------------------------------------------------------------------
options_customized <- portfolio_sort_options(
  filter_options = filter_options(
    exclude_financials = TRUE,
    exclude_utilities = TRUE,
    exclude_negative_earnings = TRUE
  ),
  breakpoint_options_main = breakpoint_options(
    n_portfolios = 5,
    breakpoints_exchanges = "NYSE",
    breakpoints_min_size_threshold = 0.2
  ),
  breakpoint_options_secondary = breakpoint_options(
    n_portfolios = 2,
    breakpoints_exchanges = "NYSE"
  )
)

factor_returns_customized <- implement_portfolio_sort(
  data = sorting_data,
  sorting_variables = c("asset_growth", "mktcap_lag"),
  sorting_method = "bivariate-dependent",
  rebalancing_month = 7,
  portfolio_sort_options = options_customized,
  min_portfolio_size = 10,
  quiet = TRUE
) |>
  compute_long_short_returns(direction = "bottom_minus_top")

## -----------------------------------------------------------------------------
fm_data <- sorting_data |>
  drop_na(asset_growth, mktcap_lag, ret_excess) |>
  mutate(log_mktcap = log(mktcap_lag))

fm_results <- estimate_fama_macbeth(
  data = fm_data,
  model = "ret_excess ~ asset_growth + log_mktcap"
)

fm_results |>
  knitr::kable(
    digits = 4,
    caption = paste(
      "Fama-MacBeth regression of monthly excess returns on lagged asset",
      "growth and log market cap. On real data the asset growth slope is",
      "negative and significant; on pseudo data it is indistinguishable",
      "from zero. The large, 'significant' intercept is a mechanical",
      "artifact of the simulated returns' built-in positive drift, not an",
      "alpha."
    )
  )

