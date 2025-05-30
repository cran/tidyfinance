#' Create WRDS Dummy Database
#'
#' Downloads the WRDS dummy database from the respective Tidy Finance GitHub
#' repository and saves it to the specified path. If the file already exists,
#' the user is prompted before it is replaced.
#'
#' @param path The file path where the SQLite database should be saved.
#' @param url The URL where the SQLite database is stored.
#'
#' @returns Invisible `NULL`. Side effect: downloads a file to the specified path.
#'
#' @export
#' @examples
#' path <- paste0(tempdir(), "/tidy_finance_r.sqlite")
#' create_wrds_dummy_database(path)
create_wrds_dummy_database <- function(
  path,
  url = paste0(
    "https://github.com/tidy-finance/website/tree/main/blog/",
    "tidy-finance-dummy-data/data/tidy_finance.sqlite"
  )
) {
  if (missing(path)) {
    cli::cli_abort(c(
      "Please provide a file path for the SQLite database.",
      i = "We recommend {.path data/tidy_finance_r.sqlite}"
    ))
  }

  if (file.exists(path)) {
    response <- readline(
      prompt = "The database file already exists at this path. Do you want to replace it? (Y/n): "
    )
    if (tolower(response) != "y") {
      cli::cli_inform("Operation aborted by the user.")
      return(invisible(NULL))
    }
  }

  handle_download_error(
    function()
      suppressWarnings(utils::download.file(
        url = url,
        destfile = path,
        quiet = TRUE
      ))
  )

  if (file.exists(path)) {
    cli::cli_inform("Downloaded WRDS dummy database to {.path {path}}.")
  }

  invisible(NULL)
}
