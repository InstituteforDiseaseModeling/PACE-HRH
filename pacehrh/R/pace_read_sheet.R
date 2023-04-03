readSheet <- function(path = GPE$inputExcelFile, sheetName) {
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
