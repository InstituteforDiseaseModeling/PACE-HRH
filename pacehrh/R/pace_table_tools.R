#' Validate Tables Against Their Schemas
#' 
#' When a table is imported from a preadsheet we need to be careful to guard
#' against random challenges - valid and invalid - users might have introduced '
#' into the spreadsheet, such as extra columns, extra rows, invalid data types, 
#' etc.
#' 
#' @param table Tibble to be validated 
#' @param schema Schema data for the table 
#' @param convertType Attempt to convert columns to the type specified in the
#'   schema. Default =FALSE; warning raised if column types don't match schema.
#'
#' @return Validated tibble
#' 
#' @noRd
validateTableAgainstSchema <- function(table = NULL, schema = NULL, convertType = FALSE){
  return(.checkColumns(table, schema, convertType))
}

.checkColumns <- function(table, schema, convertType){
  e <- rlang::current_env()

  rCols <- .checkRequiredColumns(e)
  if (is.null(rCols)){
    return(NULL)
  }
  
  oCols <- .checkOptionalColumns(e)
  if (is.null(oCols)){
    return(NULL)
  }

  assertthat::assert_that(setequal(c(rCols, oCols), schema$cols))
    
  table <- table[c(rCols, oCols)]
  
  .removeBadRows(e)
  
  return(table)  
}

.removeBadRows <- function(e){
  if (is.null(e$schema$kcols)){
    return(invisible(NULL))
  }
  
  for (colName in e$schema$kcols){
    e$table <- tidyr::drop_na(e$table, all_of(colName))
  }
}

.checkRequiredColumns <- function(e){
  columns <- names(e$table)
  
  # Test that all required columns have shown up
  if (length(setdiff(e$schema$rcols, columns)) > 0){
    .raiseAlarm(setdiff(e$schema$rcols, columns), alarmType = "name")
    return(NULL)
  }
  
  # Test that all required columns have the correct types
  correctTypesMask <- (sapply(e$table[e$schema$rcols], typeof) == e$schema$rtypes)
  if (!all(correctTypesMask)){
    if (e$convertType){
      # Convert columns to correct types if possible
      indexes <- which(!correctTypesMask)
      for (i in indexes){
        .changeColumnType(e, e$schema$rcols[i], e$schema$rtype[i])
      }
    } else {
      .raiseAlarm(e$schema$rcols[!correctTypesMask], alarmType = "type")
      return(NULL)
    }
  }
  
  # Return full list of required columns
  return(e$schema$rcols)
}

.changeColumnType <- function(e, colName, colType){
  # TODO - catch "NAs introduced by coercion" warning
  
  if (colType == "double"){
    e$table[[colName]] <- as.numeric(e$table[[colName]])
  } else if (colType == "character") {
    e$table[[colName]] <- as.character(e$table[[colName]])
  }
}

.checkOptionalColumns <- function(e){
  out <- vector(mode = "character")

  if (is.null(e$schema$ocols)){
    return(out)
  }
  
  columns <- names(e$table)
  types <- sapply(e$table, typeof)
  badTypeColumnsList <- vector(mode = "character")
  
  for (i in seq_along(e$schema$ocols)){
    col <- e$schema$ocols[i]
    type <- e$schema$otypes[i]
    
    if (col %in% columns){ # If the table contains an optional column
      if (types[col] == type){ # If the column has the right type
        out <- c(out, col)
      } else {
        if (e$convertType){
          .changeColumnType(e, col, type)
          out <- c(out, col)
        } else {
          badTypeColumnsList <- c(badTypeColumnsList, col)
        }
      }
    } else { # Add optional column with default values
      e$table[[col]] <- vector(mode = type, length = NROW(e$table))
      out <- c(out, col)
    }
  }
  
  # Raise alarm if user has supplied optional columns of the wrong type
  if (length(badTypeColumnsList) > 0){
    .raiseAlarm(badTypeColumnsList, alarmType = "type")
    return(NULL)
  }
  
  # Return list of present and validated optional columns
  return(out)
}

.raiseAlarm <- function(columns, alarmType = "name"){
  colStr <- paste(columns, collapse = ", " )
  
  if (alarmType == "name"){
    errorMsg <- paste0("Missing required columns in table: ", colStr)
  } else if (alarmType == "type"){
    errorMsg <- paste0("Columns with incorrect types in table: ", colStr)
  }
  
  raiseMessage(errorMsg)
}
