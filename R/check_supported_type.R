#' Check if a Dataset Type is Supported
#'
#' This function checks if a given dataset type is supported by verifying
#' against a list of all supported dataset types from different domains. If the
#' specified type is not supported, it stops execution and returns an error
#' message listing all supported types.
#'
#' @param type The dataset type to check for support.
#'
#' @returns Does not return a value; instead, it either passes silently if the
#'   type is supported or stops execution with an error message if the type is
#'   unsupported.
check_supported_type <- function(type) {
  supported_types <- list_supported_types(as_vector = TRUE)
  supported_types_legacy <- list_supported_types_ff_legacy()$type
  if (!any(type %in% c(supported_types, supported_types_legacy))) {
    stop("Unsupported type specified. Call the function list_supported_types() to get all supported types.")
  }
}
