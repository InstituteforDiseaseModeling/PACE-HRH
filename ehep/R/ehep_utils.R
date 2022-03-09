is.blank <- function(str){
  if (is.null(str)) {
    return(TRUE)
  }

  if (is.na(str)) {
    return(TRUE)
  }

  if (is.character(str)) {
    if (nchar(trimws(str)) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  return(FALSE)
}
