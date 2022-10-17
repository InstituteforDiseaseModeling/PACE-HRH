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

  sdvalue <- BVE$stochasticParams$p[BVE$stochasticParams$Value == "Hours per week"]

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
#' @examples
#' \dontrun{
#' resultsFiles <- c("results/results_BasicModel_Jul04.csv",
#'                   "results/results_ComprehensiveModel_Jul04.csv",
#'                   "results/results_MergedModel_Jul04.csv")
#'
#' DR <- pacehrh::ReadAndCollateSuiteResults(files = resultsFiles)
#'
#'
#' funcFilter1 <- function(dt){
#'   dt <- subset(dt, (Trial_num == 2))
#'   return(dt)
#' }
#'
#' funcFilter2 <- function(dt){
#'   keep <- vector(mode = "logical", length = nrow(dt))
#'   keep[] <- TRUE
#'
#'   mask <- dt$DeliveryModel == "Merged" & (dt$Task_ID %in% c("Overhead_staff3", "Overhead_staff4", "Overhead_staff5", "Overhead_staff6"))
#'   keep[mask] <- FALSE
#'
#'   mask <- dt$DeliveryModel == "Basic" & (dt$Task_ID %in% c("Overhead_staff3") & dt$Year < 2025)
#'   keep[mask] <- FALSE
#'
#'   mask <- dt$DeliveryModel == "Basic" & (dt$Task_ID %in% c("Overhead_staff4") & dt$Year < 2030)
#'   keep[mask] <- FALSE
#'
#'   mask <- dt$DeliveryModel == "Basic" & (dt$Task_ID %in% c("Overhead_staff5", "Overhead_staff6"))
#'   keep[mask] <- FALSE
#'
#'   mask <- dt$DeliveryModel == "Comprehensive" & (dt$Task_ID %in% c("Overhead_staff3", "Overhead_staff4") & dt$Year < 2025)
#'   keep[mask] <- FALSE
#'
#'   dt <- dt[keep]
#'   return(dt)
#' }
#'
#' DR <- pacehrh::ReadAndCollateSuiteResults(files = resultsFiles,
#'                                        preProcFunc = funcFilter1,
#'                                        postProcFunc = funcFilter2)
#' }
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

  # Check that the scenarios table only refers to one geography
  geographies <- unique(GPE$scenarios$Geography_dontedit)

  if (length(geographies) > 1){
    traceMessage(paste0("Too many geographies in the scenarios table ..."))
    for (geo in geographies){
      traceMessage(paste0("* ", geo))
    }
    return(FALSE)
  }

  # Get stochastic parameters. If none has already been loaded, call the
  # appropriate initialization function.
  if (is.null(BVE$stochasticParams)){
    InitializeStochasticParameters()
  }

  if (is.null(BVE$stochasticParams)){
    traceMessage(paste0("Failed to load stochastic parameters from ", GPE$inputExcelFile))
    return(FALSE)
  }

  return(TRUE)
}

.readAndPrepTaskValues <- function(scenarios){
  # Load and collate all the referenced task value sheets (~150 lines per sheet),
  # adding a SheetRef value along the way.


  ## <<< MOVE GEOGRAPHIES TESTS HERE


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

.initComputeCadreAllocations <- function(){
  if (!.checkScenarios()){
    return(FALSE)
  }

  return(TRUE)
}

#' Compute Cadre Allocations
#'
#' Different healthcare delivery models involve different mixtures of types
#' of healthcare workers ("Cadres"). \code{ComputeCadresAllocations()} reads
#' cadre information from the input Excel file, then combines the allocation
#' percentages with computed task times to produce a table showing how much
#' time each cadre spends per year on each healthcare task.
#'
#' @param DR Task time results from \code{ReadAndCollateSuiteResults()}
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' resultsFiles <- c("results/results_BasicModel_Jul04.csv",
#'                   "results/results_ComprehensiveModel_Jul04.csv",
#'                   "results/results_MergedModel_Jul04.csv")
#'
#' DR <- pacehrh::ReadAndCollateSuiteResults(files = resultsFiles)
#' CA <- pacehrh:::ComputeCadreAllocations(DR)
#' }
ComputeCadreAllocations <- function(DR = NULL){
  if (is.null(DR)){
    return(NULL)
  }

  if (!.initComputeCadreAllocations()){
    return(NULL)
  }

  # Read the cadre allocation sheets from Excel. The individual sheets are in
  # task x (year, allocation-model) format. The format is converted to a skinny
  # version so the sheets can be concatenated. Along the way the (year, allocation-model)
  # encoding is tokenized.
  cadreSheetNames <- GPE$scenarios$sheet_Cadre
  deliveryModels <- GPE$scenarios$DeliveryModel

  l <- lapply(seq_along(cadreSheetNames), function(i) {
    dt <-
      data.table::setDT(readxl::read_xlsx(GPE$inputExcelFile, sheet = cadreSheetNames[i]))

    suppressWarnings(
      dt_melt <-
        data.table::melt(
          dt,
          id.vars = c("Indicator", "CommonName"),
          variable.name = "Category",
          value.name = "allocation",
          variable.factor = FALSE
        )
    )

    # Some data manipulation ...
    # - Split the category values (e.g. "20_HEW") into two tokens, and use
    #   the tokens to populate new Cadre and Year columns
    # - Add a DeliveryModel column
    # - Remove rows with Cadre == TOTAL (it's a development diagnostic artifact)
    # - Convert allocation == NA to allocation == 0

    tokens <- tstrsplit(dt_melt[,Category], "_")

    dt_melt[, Year := as.numeric(tokens[[1]]) + 2000]
    dt_melt[, Cadre := tokens[[2]]]
    dt_melt[, DeliveryModel := deliveryModels[i]]

    dt_melt <- dt_melt[Cadre != "TOTAL"]
    dt_melt <- dt_melt[is.na(allocation), allocation := 0]

    return(dt_melt)
  })

  CadreAllocations <- data.table::rbindlist(l)

  # Make a wide version of the cadre allocations, with one row per task/model/year,
  # and separate columns for allocations to each worker type.

  caWide <-
    data.table::dcast(
      CadreAllocations,
      Indicator + DeliveryModel + Year ~ Cadre,
      value.var = "allocation",
      fill = 0
    )

  allocCalcColumns <- c("Task_ID",
                        "Scenario_ID",
                        "DeliveryModel",
                        "Trial_num",
                        "Year",
                        "Service_time",
                        "WeeksPerYr")

  AllocCalcs <- DR[ , ..allocCalcColumns]

  # Define match years for which delivery model will be in operation
  AllocCalcs[, MatchYear := 2020]
  AllocCalcs[Year > 2024, MatchYear := 2025]
  AllocCalcs[Year > 2029, MatchYear := 2030]
  AllocCalcs[Year > 2034, MatchYear := 2035]

  # Left-join AllocCalcs table to caWide table
  AllocCalcs <-
    AllocCalcs[caWide, on = c(
      "Task_ID" = "Indicator",
      "DeliveryModel" = "DeliveryModel",
      "MatchYear" = "Year"
    ), nomatch = NULL]

  workerTypes <- c(HEW = "HEW",
                   MW = "Midwife",
                   HO = "HealthOfficer",
                   FH = "FamilyHealth",
                   RN = "Nurse",
                   EH = "EnvironHealth",
                   UN = "Unassigned")

  # Convert allocation percentages into allocated time
  AllocCalcs[, HEW_Alloc := HEW * Service_time / 100.0]
  AllocCalcs[, MW_Alloc := Midwife * Service_time / 100.0]
  AllocCalcs[, HO_Alloc := HealthOfficer * Service_time / 100.0]
  AllocCalcs[, FH_Alloc := FamilyHealth * Service_time / 100.0]
  AllocCalcs[, RN_Alloc := Nurse * Service_time / 100.0]
  AllocCalcs[, EH_Alloc := EnvironHealth * Service_time / 100.0]
  AllocCalcs[, UN_Alloc := Unassigned * Service_time / 100.0]

  return(AllocCalcs)
}

#' Compute Summary Statistics
#'
#' @param DR From \code{ReadAndCollateSuiteResults()} function
#' @param CA From \code{ComputeCadreAllocations()} function
#'
#' @return List of computed summary tables
#' @export
#'
#' @examples
#' \dontrun{
#' resultsFiles <- c("results/results_BasicModel_Jul04.csv",
#'                   "results/results_ComprehensiveModel_Jul04.csv",
#'                   "results/results_MergedModel_Jul04.csv")
#'
#' DR <- pacehrh::ReadAndCollateSuiteResults(files = resultsFiles)
#' CA <- pacehrh::ComputeCadreAllocations(DR)
#' stats <- pacehrh::ComputeSummaryStats(DR,CA)
#' }
ComputeSummaryStats <- function(DR = NULL, CA = NULL){
  if (is.null(DR) | is.null(CA)){
    return(NULL)
  }

  traceMessage("Making ClinCat tables")
  ByRun_ClinCat <-
    DR[,
       .(TotHrs = sum(Service_time)),
       keyby = .(Scenario_ID, Trial_num, Year, ClinicalCat, ClinicalOrNon, WeeksPerYr)]


  Mean_ClinCat <-
    ByRun_ClinCat[,
                  .(
                    CI05 = quantile(TotHrs, probs = c(.05)),
                    CI25 = quantile(TotHrs, probs = c(.25)),
                    MeanHrs = mean(TotHrs),
                    CI75 = quantile(TotHrs, probs = c(.75)),
                    CI95 = quantile(TotHrs, probs = c(.95))
                  ),
                  keyby = .(Scenario_ID, Year, ClinicalCat, ClinicalOrNon, WeeksPerYr)]

  traceMessage("Making ClinMonth tables")
  ByRun_ClinMonth <-
    DR[ClinicalOrNon == "Clinical",
       .(TotHrs = sum(Service_time)),
       keyby = .(Scenario_ID, Trial_num, Year, Month, WeeksPerYr, HrsPerWeek)]

  Stats_ClinMonth <-
    ByRun_ClinMonth[,
                    .(
                      CI05 = quantile(TotHrs, probs = c(.05)),
                      CI25 = quantile(TotHrs, probs = c(.25)),
                      CI50 = mean(TotHrs),
                      CI75 = quantile(TotHrs, probs = c(.75)),
                      CI95 = quantile(TotHrs, probs = c(.95))
                    ),
                    keyby = .(Scenario_ID, Year, Month, WeeksPerYr, HrsPerWeek)]

  traceMessage("Making ServiceCat tables")
  DR2 <- data.table::setDT(DR)
  DR2[ServiceCat %in% c("Schisto MDA", "LLINs and IRS", "Vector control"), ServiceCat := "Campaign"]

  ByRun_ServiceCat <-
    DR2[,
        .(TotHrs = sum(Service_time)),
        keyby = .(Scenario_ID, Trial_num, Year, ServiceCat, ClinicalOrNon)]

  Mean_ServiceCat <-
    ByRun_ServiceCat[,
                     .(
                       CI05 = quantile(TotHrs, probs = c(.05)),
                       CI25 = quantile(TotHrs, probs = c(.25)),
                       MeanHrs = mean(TotHrs),
                       CI75 = quantile(TotHrs, probs = c(.75)),
                       CI95 = quantile(TotHrs, probs = c(.95))
                      ),
                     keyby = .(Scenario_ID, Year, ServiceCat, ClinicalOrNon)]

  traceMessage("Making AnnualTask tables")

  Mean_AnnualTask <-
    DR2[,
                     .(
                       CI05 = quantile(Service_time, probs = c(.05)),
                       CI25 = quantile(Service_time, probs = c(.25)),
                       MeanHrs = mean(Service_time),
                       CI75 = quantile(Service_time, probs = c(.75)),
                       CI95 = quantile(Service_time, probs = c(.95))
                     ),
                     keyby = .(Scenario_ID, Year, Task_ID, ServiceCat, ClinicalOrNon)]

  traceMessage("Making Total tables")
  ByRun_Total <-
    DR2[,
        .(TotHrs = sum(Service_time)),
        keyby = .(Scenario_ID, Trial_num, Year, WeeksPerYr, HrsPerWeek)]

  Mean_Total <-
    ByRun_Total[,
                .(
                  CI05 = quantile(TotHrs, probs = c(.05)),
                  CI25 = quantile(TotHrs, probs = c(.25)),
                  MeanHrs = mean(TotHrs),
                  CI75 = quantile(TotHrs, probs = c(.75)),
                  CI95 = quantile(TotHrs, probs = c(.95))
                ),
                keyby = .(Scenario_ID, Year, WeeksPerYr, HrsPerWeek)]

  traceMessage("Making TotClin tables")
  ByRun_TotClin <-
    DR2[ClinicalOrNon == "Clinical",
        .(AnnualHrs = sum(Service_time)),
        keyby = .(Scenario_ID, Trial_num, Year, WeeksPerYr, HrsPerWeek)]

  Stats_TotClin <-
    ByRun_TotClin[,
                  .(
                    CI05 = quantile(AnnualHrs, probs = c(.05)),
                    CI25 = quantile(AnnualHrs, probs = c(.25)),
                    CI50 = mean(AnnualHrs),
                    CI75 = quantile(AnnualHrs, probs = c(.75)),
                    CI95 = quantile(AnnualHrs, probs = c(.95))
                  ),
                  keyby = .(Scenario_ID, Year, WeeksPerYr, HrsPerWeek)]

  traceMessage("Making Alloc tables")
  ByRun_Alloc <-
    CA[,
       .(MW_hrs = sum(MW_Alloc),
         HO_hrs = sum(HO_Alloc),
         RN_hrs = sum(RN_Alloc),
         EH_hrs = sum(EH_Alloc),
         FH_hrs = sum(FH_Alloc),
         UN_hrs = sum(UN_Alloc),
         HEW_hrs = sum(HEW_Alloc)),
       keyby = .(Scenario_ID, DeliveryModel, Trial_num, Year, WeeksPerYr)]


  suppressWarnings(
    ByRun_Alloc_melt <-
      data.table::melt(
        ByRun_Alloc,
        id.vars = c("Scenario_ID", "DeliveryModel", "Trial_num", "Year", "WeeksPerYr"),
        variable.name = "Cadre",
        value.name = "AnnualHrs",
        variable.factor = FALSE
      )
  )

  ByRun_Alloc_melt <- ByRun_Alloc_melt[!is.na(AnnualHrs)]

  Mean_Alloc <-
    ByRun_Alloc_melt[,
                     .(
                       CI05 = quantile(AnnualHrs, probs = c(.05)),
                       CI25 = quantile(AnnualHrs, probs = c(.25)),
                       CI50 = mean(AnnualHrs),
                       CI75 = quantile(AnnualHrs, probs = c(.75)),
                       CI95 = quantile(AnnualHrs, probs = c(.95))
                       ),
                     keyby = .(Scenario_ID, DeliveryModel, Year, WeeksPerYr, Cadre)]

  return(list(
    "ByRun_ClinCat" = ByRun_ClinCat,
    "Mean_ClinCat" = Mean_ClinCat,
    "ByRun_ClinMonth" = ByRun_ClinMonth,
    "Stats_ClinMonth" = Stats_ClinMonth,
    "ByRun_ServiceCat" = ByRun_ServiceCat,
    "Mean_ServiceCat" = Mean_ServiceCat,
    "Mean_AnnualTask" = Mean_AnnualTask,
    "ByRun_Total" = ByRun_Total,
    "Mean_Total" = Mean_Total,
    "ByRun_TotClin" = ByRun_TotClin,
    "Stats_TotClin" = Stats_TotClin,
    "ByRun_Alloc" = ByRun_Alloc,
    "Mean_Alloc" = Mean_Alloc))
}
