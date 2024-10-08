#' Download CCM Links from WRDS
#'
#' This function downloads data from the WRDS CRSP/Compustat Merged (CCM) links
#' database. It allows users to specify the type of links (`linktype`), the
#' primacy of the link (`linkprim`), and whether to use flagged links
#' (`usedflag`).
#'
#' @param linktype A character vector indicating the type of link to download.
#'   The default is `c("LU", "LC")`, where "LU" stands for "Link Up" and "LC"
#'   for "Link CRSP".
#' @param linkprim A character vector indicating the primacy of the link.
#'   Default is `c("P", "C")`, where "P" indicates primary and "C" indicates
#'   conditional links.
#' @param usedflag An integer indicating whether to use flagged links. The
#'   default is `1`, indicating that only flagged links should be used.
#'
#' @returns A data frame with the columns `permno`, `gvkey`, `linkdt`, and
#'   `linkenddt`, where `linkenddt` is the end date of the link, and missing end
#'   dates are replaced with today's date.
#'
#' @export
#' @examples
#' \donttest{
#'   ccm_links <- download_data_wrds_ccm_links(linktype = "LU", linkprim = "P", usedflag = 1)
#' }
download_data_wrds_ccm_links <- function(
    linktype = c("LU", "LC"), linkprim = c("P", "C"), usedflag = 1
) {

  rlang::check_installed(
    "dbplyr", reason = "to download type wrds_ccm_links."
  )

  con <- get_wrds_connection()

  ccmxpf_linktable_db <- tbl(con, I("crsp.ccmxpf_linktable"))

  ccm_links <- ccmxpf_linktable_db |>
    filter(linktype %in% local(linktype) &
             linkprim %in% local(linkprim) &
             usedflag == local(usedflag)) |>
    select(permno = lpermno, gvkey, linkdt, linkenddt) |>
    collect() |>
    mutate(linkenddt = tidyr::replace_na(linkenddt, today()))

  disconnection_connection(con)

  ccm_links
}
