#' Perform validation on a data column
#'
#' @description
#' Perform validation on a data column to ensure it contains only numeric values.
#'
#' @param column Column of data to examine. Throws an error if column contains
#' non-numeric values.
#'
#' @return None. Throws an error if non-numeric values are found.
#'
#' @examples
#' # Should not throw an error
#' check_numeric_column(c(1, 2, 3.5))
#'
#' # Should throw an error
#' check_numeric_column(c(1, 2, "a"))

check_numeric_column <- function(column) {
  if (is.numeric(column)) {
    return(TRUE)
  }
  numeric_values <- suppressWarnings(as.numeric(as.character(column)))
  if (any(is.na(numeric_values) & !is.na(column))) {
    stop("Error: Non-numeric values found in the column.")
  }
  return(TRUE)
}
