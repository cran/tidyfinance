#' Create WRDS Dummy Database
#'
#' Downloads the WRDS dummy database from the respective Tidy Finance GitHub
#' repository and saves it to the specified path. If the file already exists,
#' the user is prompted before it is replaced.
#'
#' @param path The file path where the SQLite database should be saved. If not
#'   provided, the default path is "data/tidy_finance_r.sqlite".
#'
#' @returns Invisible `NULL`. Side effect: downloads a file to the specified path.
#'
#' @export
#' @examples
#' path <- paste0(tempdir(), "/tidy_finance_r.sqlite")
#' create_wrds_dummy_database(path)
create_wrds_dummy_database <- function(path) {

  if (missing(path)) {
    stop("Please provide a file path for the SQLite database. We recommend 'data/tidy_finance_r.sqlite'.")
  }

  if (file.exists(path)) {
    response <- readline(prompt = "The database file already exists at this path. Do you want to replace it? (Y/n): ")
    if (tolower(response) != "y") {
      message("Operation aborted by the user.")
      return(invisible(NULL))
    }
  }

  url <- "https://github.com/tidy-finance/website/tree/main/blog/tidy-finance-dummy-data/data/tidy_finance.sqlite"
  utils::download.file(
    url = url,
    destfile = path,
    quiet = TRUE
  )

  message("Downloaded WRDS dummy database to ", path, ".")

  return(invisible(NULL))
}
