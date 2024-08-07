# tidyfinance 0.3.0

## New features

* Added support for all available Fama-French datasets (check via `list_supported_types()`). All type names are created from a string cleaning algorithm and are hence more consistent. We kept implicit support for legacy type names to avoid breaking existing code.
* Added new function to download stock data from Yahoo Finance: `download_data_stocks()`.
* Added support for `wrds_compustat_quarterly`. 

## Bug fixes

* CRSP monthly data always contains the historically accurate stock characteristics instead of the oft misleading most recent information.
* Consistently implemented the `additional_columns` option for CRSP and Compustat instead of having the error prone option to pass columns via `...`.
* Added replacement of `-999` by NA in Fama-French types, which was missing in the initial implementation. 

## Improvements

* Refactored the column name cleaning procedure in `download_data_factors()` to support all available column names in the Fama-French universe.
* Made all `start_date` and `end_date` optional with a message to user which dates are used as defaults.
* Introduced automatic checks via GitHub Actions workflows.
* Synchronized `date` column and its references across WRDS types (see corresponding vignette for more information).
* Improved handling of imports with `tidyfinance-package.R` file. 
* Reformatted DESCRIPTION and roxygen comments for more consistency with `tidyverse` style.

# tidyfinance 0.2.1

## New features

* Added `domain` and `as_vector` parameters to `list_supported_types()`

## Bug fixes

* Replaced `...` with `additional_columns` parameter and ensured that CRSP and Compustat types consider it correctly
* Removed `mkt_excess` column from type "wrds_crsp_monthly"

## Improvements

* Added `fixed = TRUE` to `grepl()` calls with fixed strings
* Switched to `NA_real_` instead of `as.double(NA)`
* Switched to `toString()` instead of `paste0()` with collapse
* Switched to `dplyr::between()` instead of unequal signs

# tidyfinance 0.2.0

## New features

* Added `vignettes/using-tidyfinance`
* Added `set_wrds_credentials()` function for a guided tour to store login data
* Added support for `"factors_ff_industry_*"` data types

## Bug fixes

* Removed `hml` and `smb` columns from `"wrds_crsp_monthly"` output
* Fixed stock filters for `"v2"` of `"wrds_crsp_*"` data types

## Improvements

* Relaxed package version requirements as much as possible with the current set of packages
* Split up the `download_data*` functions into multiple files for better maintenance

# tidyfinance 0.1.0

* Initial CRAN submission.
