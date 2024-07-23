## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyfinance)
library(dplyr)
library(lubridate)

## -----------------------------------------------------------------------------
factors_ff_3_monthly <- download_data("factors_ff_3_monthly")
factors_ff_3_monthly

## -----------------------------------------------------------------------------
factors_ff_3_monthly |> 
  select(date) |> 
  mutate(date_lag3 = date %m-% months(3),
         date_difference = interval(date_lag3, date) %/% months(1))

## -----------------------------------------------------------------------------
factors_ff_3_daily <- download_data("factors_ff_3_daily")
factors_ff_3_daily

## -----------------------------------------------------------------------------
factors_ff_3_daily |> 
  select(date) |> 
  mutate(date_difference = interval(lag(date), date) %/% days(1)) 

## -----------------------------------------------------------------------------
#  crsp_daily <- download_data("wrds_crsp_daily")
#  crsp_daily |>
#    select(permno, date)

## -----------------------------------------------------------------------------
#  crsp_monthly <- download_data("wrds_crsp_monthly")
#  crsp_monthly |>
#    select(permno, date, calculation_date)

## -----------------------------------------------------------------------------
#  compustat_annual <- download_data("wrds_compustat_annual")
#  compustat_annual |>
#    select(gvkey, date, datadate)

## -----------------------------------------------------------------------------
#  compustat_annual |>
#    mutate(year = year(date),
#           month = month(date)) |>
#    select(gvkey, date, datadate, year, month)

## -----------------------------------------------------------------------------
#  compustat_quarterly <- download_data("wrds_compustat_quarterly")
#  compustat_quarterly |>
#    select(gvkey, date, datadate) |>
#    mutate(year = year(date),
#           month = month(date),
#           quarter = quarter(date))

## -----------------------------------------------------------------------------
#  crsp_monthly |>
#    left_join(factors_ff_3_monthly, join_by(date)) |>
#    select(permno, date, risk_free, mkt_excess, smb, hml)

## -----------------------------------------------------------------------------
#  crsp_daily |>
#    left_join(factors_ff_3_daily, join_by(date)) |>
#    select(permno, date, risk_free, mkt_excess, smb, hml)

## -----------------------------------------------------------------------------
#  ccm_links <- download_data("wrds_ccm_links")
#  crsp_monthly <- crsp_monthly |>
#    left_join(ccm_links, join_by(permno), relationship = "many-to-many") |>
#    filter(between(date, linkdt, linkenddt)) |>
#    select(-c(linkdt, linkenddt))

## -----------------------------------------------------------------------------
#  crsp_monthly |>
#    left_join(
#      compustat_annual, join_by(gvkey, date)
#    )

## -----------------------------------------------------------------------------
#  crsp_monthly |>
#    left_join(
#      compustat_annual |>
#        mutate(date = date %m+% months(6)),
#      join_by(gvkey, date)
#    )

## -----------------------------------------------------------------------------
#  crsp_monthly |>
#    left_join(
#      compustat_annual |>
#        mutate(date = ymd(paste0(year(date) + 1, "0701"))),
#      join_by(gvkey, date)
#    )

