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
          globalPackageEnvironment$inputExcelFile <-
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

.checkAndLoadGlobalConfig <- function(){
  if (!globalPackageEnvironment$globalConfigLoaded){
    loadGlobalConfig()
    globalPackageEnvironment$globalConfigLoaded <- TRUE
  }
}



