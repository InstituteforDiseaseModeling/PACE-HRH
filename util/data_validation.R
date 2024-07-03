#' #' Perform validation on a data column
#'
#' @description
#' Perform validation on a data column to ensure it contains only numeric values
#'
#' @param column Column of data to examine. Returns error if column contains
#' non-numeric value
#'
#' @return Error code.

library(readr)

check_numeric_column <- function(column) {
  if (any(is.na(as.numeric(as.character(column))))) {
    stop("Error: Non-numeric values found in the column.")
  }
}
