#' Download Stock Data
#'
#' Downloads historical stock data from Yahoo Finance for given symbols and date range.
#'
#' @param symbols A character vector of stock symbols to download data for. At least one
#'   symbol must be provided.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#'
#' @returns A tibble containing the downloaded stock data with columns: symbol,
#'   date, volume, open, low, high, close, and adjusted_close.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_stock_prices(c("AAPL", "MSFT"))
#'   download_data_stock_prices("GOOGL", "2021-01-01", "2022-01-01" )
#' }
download_data_stock_prices <- function(
  symbols,
  start_date = NULL,
  end_date = NULL
) {
  if (!is.character(symbols) || anyNA(symbols)) {
    cli::cli_abort(
      "{.arg symbols} must be character vector, not {.obj_type_friendly {symbols}}."
    )
  }

  rlang::check_installed(
    "httr2",
    reason = "to download type 'stock_prices'."
  )

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  dates <- validate_dates(start_date, end_date, use_default_range = TRUE)
  start_date <- dates$start_date
  end_date <- dates$end_date

  start_timestamp <- as.integer(as.POSIXct(start_date, tz = "UTC"))
  end_timestamp <- as.integer(as.POSIXct(end_date, tz = "UTC"))

  processed_data <- list()

  cli::cli_progress_bar(
    "Downloading symbols",
    total = length(symbols),
    clear = TRUE
  )

  for (j in seq_along(symbols)) {
    url <- paste0(
      "https://query2.finance.yahoo.com/v8/finance/chart/",
      symbols[j],
      "?period1=",
      start_timestamp,
      "&period2=",
      end_timestamp,
      "&interval=1d"
    )

    response <- httr2::request(url) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()

    if (response$status_code == 200) {
      raw_data <- httr2::resp_body_json(response)$chart$result

      replace_null_with_na <- function(x) {
        if (is.list(x)) {
          lapply(x, replace_null_with_na)
        } else if (is.null(x)) {
          NA
        } else {
          x
        }
      }

      ohlcv <- unlist(raw_data[[1]]$indicators$quote, recursive = FALSE)
      ohlcv <- replace_null_with_na(ohlcv)

      indicators <- raw_data[[1]]$indicators$adjclose
      indicators <- replace_null_with_na(indicators)

      processed_data_symbol <- tibble(
        "symbol" = symbols[j],
        "date" = as.Date(as.POSIXct(
          as.numeric(raw_data[[1]]$timestamp),
          origin = "1970-01-01"
        )),
        "volume" = as.numeric(unlist(ohlcv$volume)),
        "open" = as.numeric(unlist(ohlcv$open)),
        "low" = as.numeric(unlist(ohlcv$low)),
        "high" = as.numeric(unlist(ohlcv$high)),
        "close" = as.numeric(unlist(ohlcv$close)),
        "adjusted_close" = as.numeric(unlist(indicators))
      )

      processed_data[[j]] <- processed_data_symbol
    } else {
      error_message <- httr2::resp_body_json(response)$chart$error
      cli::cli_warn(
        "Failed to retrieve data for symbol {symbols[j]} with status code {response$status_code} ({error_message$code}): {error_message$description}"
      )
    }
    cli::cli_progress_update()
  }

  bind_rows(processed_data)
}
