.defPreProcFunc <- function(dt){
  dt <- subset(
    dt,
    (Year < 2036 & Year > 2020) &
      (Service_time != 0) &
      (Task_ID != "Test additional") &
      (Trial_num < 101)
  )

  return(dt)
}

.defPostProcFunc <- function(dt){
  # Set the ServiceCat value to be the same as the CommonName for non-clinical tasks
  mask <- (is.na(dt$ClinicalOrNon) | dt$ClinicalOrNon != "Clinical")
  dt$ServiceCat[mask] <- dt$CommonName[mask]

  # Convert minutes to hours
  dt$Service_time <- dt$Service_time / 60.0

  # Remove excess rows due to the repetitive entry of overhead time
  keep <- vector(mode = "logical", length = nrow(dt))
  keep[] <- TRUE

  mask <- dt$DeliveryModel == "Merged" & (dt$Task_ID %in% c("Overhead_staff3", "Overhead_staff4", "Overhead_staff5", "Overhead_staff6"))
  keep[mask] <- FALSE

  mask <- dt$DeliveryModel == "Basic" & (dt$Task_ID %in% c("Overhead_staff3") & dt$Year < 2025)
  keep[mask] <- FALSE

  mask <- dt$DeliveryModel == "Basic" & (dt$Task_ID %in% c("Overhead_staff4") & dt$Year < 2030)
  keep[mask] <- FALSE

  mask <- dt$DeliveryModel == "Basic" & (dt$Task_ID %in% c("Overhead_staff5", "Overhead_staff6"))
  keep[mask] <- FALSE

  mask <- dt$DeliveryModel == "Comprehensive" & (dt$Task_ID %in% c("Overhead_staff3", "Overhead_staff4") & dt$Year < 2025)
  keep[mask] <- FALSE

  dt <- dt[keep]

  # Create stochastic hours per week value. Should only be used for small population estimates.

  sdvalue <- GPE$stochasticParams$p[GPE$stochasticParams$Value == "Hours per week"]

  StochasticHrs <-
    dt[, .(HrsPerWeek = mean(HrsPerWeek)), by = .(Scenario_ID, Trial_num, Year)]

  StochasticHrs$HPW_stochastic <-
    runif(
      nrow(StochasticHrs),
      min = StochasticHrs$HrsPerWeek * (1 - sdvalue),
      max = StochasticHrs$HrsPerWeek * (1 + sdvalue)
    )

  StochasticHrs$HPW_stochastic[StochasticHrs$HPW_stochastic > 40] <- 40
  StochasticHrs$HrsPerWeek <- NULL

  dt <-
    dplyr::left_join(
      dt,
      StochasticHrs,
      by = c(
        "Scenario_ID" = "Scenario_ID",
        "Trial_num" = "Trial_num",
        "Year" = "Year"
      )
    )

  return(dt)
}

#' Prepare Suite Results For Post-Processing
#'
#' Combine a family of suite results into one big file, and join with scenario
#' and task value data for further data analysis.
#'
#' @param files Vector of results files to read. (default = NULL)
#' @param dir Directory of results files to read. The __files__ parameter has
#' precedence over the __dir__ parameter. (default = NULL)
#' @param pattern Regular expression to select which results files will be read
#' from directory __dir__. (default = "^.*\\.csv", simple filter for CSV files)
#' @param configFile Input configuration Excel file. If no __configFile__ is
#' specified, the one declared during global initialization of the package
#' is used. (default = NULL)
#' @param preProcFunc (default = .defPreProcFunc)
#' @param postProcFunc (default = .defPostProcFunc)
#'
#' @return Data table.
#'
#' @import data.table
#'
#' @md
#' @export
#'
ReadAndCollateSuiteResults <- function(files = NULL,
                                       dir = NULL,
                                       pattern = "^.*\\.csv",
                                       configFile = NULL,
                                       preProcFunc = .defPreProcFunc,
                                       postProcFunc = .defPostProcFunc){
  DR <- NULL

  if (!.initReadAndCollateSuiteResults(files, dir, pattern, configFile)){
    return(NULL)
  }

  tasks <- .readAndPrepTaskValues(GPE$scenarios)

  # Load and collate all the results files
  l <- lapply(files, function(file){
    if (file.exists(file)){
      dt <- data.table::fread(file)

      # First filtering
      dt <- preProcFunc(dt)
      dt <- dplyr::left_join(dt, GPE$scenarios, by = c("Scenario_ID" = "UniqueID"))
    }

    return(dt)
  })

  DR <- data.table::rbindlist(l)
  DR <-
    dplyr::left_join(DR,
                     tasks,
                     by = c("sheet_TaskValues" = "SheetRef",
                            "Task_ID" = "Indicator"))

  return(postProcFunc(DR))
}

.initReadAndCollateSuiteResults <- function(files, dir, pattern, configFile){
  # Test that the files and dir parameters make sense
  if (is.null(files) & is.null(dir)){
    traceMessage("readAndCollateSuiteResults() requires either a file list or a directory")
    return(FALSE)
  }

  # The dir parameter is used if the file parameter is missing
  if (is.null(files)){
    if (!dir.exists(dir)){
      traceMessage(paste0("Directory ", dir, " does not exist"))
      return(FALSE)
    }

    files <- dir(path = dir, pattern = pattern, full.names = TRUE)
  }

  if (is.null(files) | length(files) == 0){
    traceMessage(paste0("No files to read: ", dir, " does not exist"))
    return(FALSE)
  }

  # Get scenarios information. If none has already been loaded, call the
  # appropriate initialization function.
  if (is.null(GPE$scenarios)){
    InitializeScenarios()
  }

  if (is.null(GPE$scenarios)){
    traceMessage(paste0("Failed to load scenarios info from ", GPE$inputExcelFile))
    return(FALSE)
  }

  geographies <- unique(GPE$scenarios$Geography_dontedit)
  print(geographies)

  if (length(geographies) > 1){
    traceMessage(paste0("Too many geographies in the scenarios table ..."))
    for (geo in geographies){
      traceMessage(paste0("* ", geo))
    }
    return(FALSE)
  }

  # Get stochastic parameters. If none has already been loaded, call the
  # appropriate initialization function.
  if (is.null(GPE$stochasticParams)){
    InitializeStochasticParameters()
  }

  if (is.null(GPE$stochasticParams)){
    traceMessage(paste0("Failed to load stochastic parameters from ", GPE$inputExcelFile))
    return(FALSE)
  }

  return(TRUE)
}

.readAndPrepTaskValues <- function(scenarios){
  # Load and collate all the referenced task value sheets (~150 lines per sheet),
  # adding a SheetRef value along the way.
  tvSheets <- unique(scenarios$sheet_TaskValues)
  l <- lapply(tvSheets, function(sheet){
    taskValues <- readxl::read_xlsx(GPE$inputExcelFile, sheet = sheet)
    taskValues$SheetRef <- sheet
    return(taskValues)
  })

  tasks <- data.table::rbindlist(l)

  # Compute a total number-of-contacts value
  tasks$NumContactsAnnual[is.na(tasks$NumContactsAnnual)] <- 0
  tasks$NumContactsPerUnit[is.na(tasks$NumContactsPerUnit)] <- 0
  tasks$NumContactsPer <-
    tasks$NumContactsPerUnit + tasks$NumContactsAnnual

  selectedTaskValues <-
    tasks[, .(
      SheetRef,
      Indicator,
      CommonName,
      ClinicalOrNon,
      ClinicalCat,
      ServiceCat,
      NumContactsPer
    )]

  return(selectedTaskValues)
}
