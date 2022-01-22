#' Load Global Configuration
#'
#' Finds and loads global configuration data from a JSON file, including
#' things like the default locations of data files.
#'
#' @param path Location of global configuration file
#'
#' @return NULL (invisible)
#'
loadGlobalConfig <- function(path = "./globalconfig.json"){
  if (file.exists(path)){
    out <- tryCatch(
      {
        configInfo <- jsonlite::read_json(path)

        if (!is.null(configInfo$configDirectoryLocation)){
          configDirPath <- configInfo$configDirectoryLocation
        } else {
          configDirPath <- "."
        }

        if (!is.null(configInfo$inputExcelFile)){
          GPE$inputExcelFile <-
            paste(configDirPath,
                  configInfo$inputExcelFile,
                  sep = "/")
        }

        # TODO: Add code to configure start/end years

      },
      warning = function(war)
      {
        ehep::TraceMessage(paste("WARNING:", war))
      },
      error = function(err)
      {
        ehep::TraceMessage(paste("ERROR:", err))
      },
      finally =
        {

        })
  }
  else {
    ehep::TraceMessage("Could not find global configuration file - using defaults")
  }

  invisible(NULL)
}

# Internal function used by the various InitializeXXXXX() functions to auto-
# magically load the global configuration.

# TODO: Add (...) support so the path= parameter can be passed through to allow
# for using files other than globalconfig.json

.checkAndLoadGlobalConfig <- function(){
  if (!GPE$globalConfigLoaded){
    loadGlobalConfig()
    GPE$globalConfigLoaded <- TRUE
  }
  invisible(NULL)
}

#' Set Global Start And End Year Parameters
#'
#' @param start Starting year
#' @param end Ending year (must be greater than \code{start} )
#'
#' @return Nothing
#'
#' @export
SetGlobalStartEndYears <- function(start = 2020, end = 2040) {
  if (!assertthat::is.number(start)) {
    return(invisible(NULL))
  }

  if (!assertthat::is.number(end)) {
    return(invisible(NULL))
  }

  if (end <= start) {
    return(invisible(NULL))
  }

  GPE$startYear <- start
  GPE$endYear <- end
  GPE$years <-
    seq(from = GPE$startYear,
        to = GPE$endYear,
        by = 1)

  return(invisible(NULL))
}



