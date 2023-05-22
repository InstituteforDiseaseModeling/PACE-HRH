readSheet <- function(path = GPE$inputExcelFile, sheetName, ...) {
  sheetData <- NULL

  if (file.exists(path)) {
    sheetData <- tryCatch({
      readxl::read_xlsx(path, sheet = sheetName)
    },
    warning = function(war) {
      traceMessage(paste("WARNING:", war))
    },
    error = function(err) {
      traceMessage(paste("ERROR:", err))
    },
    finally = {

    })
  } else {
    traceMessage(paste0("Could not find model input file ", path))
  }

  return(sheetData)
}

# ------------------------------------------------------------------------------

#' Read And Validate Tables Against Their Schemas
#'
#' Read spreadsheet sheets, then validate the data formats.
#'
#' @param file Spreadsheet location (default = GPE$inputExcelFile)
#' @param sheet Name of sheet to read
#' @param schema Schema data for the table (default = NULL)
#' @param convertType Attempt to convert columns to the type specified in the
#'   schema. Default =FALSE; warning raised if column types don't match schema.
#'
#' @return Validated tibble
#'
#' @noRd
loadTable <- function(file = GPE$inputExcelFile,
                      sheet,
                      schema = NULL,
                      convertType = FALSE) {
  data <- readSheet(path = file, sheetName = sheet)

  if (is.null(data)){
    traceMessage(paste0("Failed to load sheet ", sheet))
  }

  if (!is.null(data) & !is.null(schema)) {
    data <- validateTableAgainstSchema(data, schema, convertType)
  }

  return(data)
}
