computeCadreData <- function(scenario = NULL, roles = NULL) {
  if (is.null(scenario)) {
    return(NULL)
  }

  if (is.null(roles)) {
    return(NULL)
  }

  # Filter to the cadre roles for the specified scenario. Return immediately if there are none.
  roles <- roles[roles$ScenarioID == scenario$UniqueID, ]

  if (nrow(roles) == 0) {
    return(NULL)
  }

  # Beginning and ending years in the experiment range
  minYear <- BVE$years[1]
  maxYear <- BVE$years[length(BVE$years)]

  # Create matrix of annual values
  annualValuesMatrix <- matrix(data = 0, nrow = nrow(roles), ncol = length(BVE$years))

  for (i in seq_along(roles$ScenarioID)) {
    if (is.na(roles$StartYear[i])) {
      startYear <- minYear
    } else {
      startYear <- max(roles$StartYear[i], minYear)
    }

    if (is.na(roles$EndYear[i])) {
      endYear <- maxYear
    } else {
      endYear <- min(roles$EndYear[i], maxYear)
    }

    if (roles$StartYear[i] <= maxYear) {
      overheadTime <- roles$OverheadHoursPerWeek[i] * 60.0 * scenario$WeeksPerYr
      annualValuesMatrix[i, (startYear - minYear + 1):(endYear - minYear + 1)] <- overheadTime
    } else{
      warning(paste0("Role ", roles$RoleID[i], " has start year after the simulation end year, it will be ignored"))
    }
    
  }

  dimnames(annualValuesMatrix) <- list("Role" = roles$RoleID, "Year" = BVE$years)

  # Convert matrix of annual values to matrix of monthly manuals.
  # A more elaborate version of this computation is done for seasonality
  # processing, but here we assume that (1) overhead values apply to the entire
  # date range, and (2) overhead values aren't affected by month, so the
  # seasonality curve is flat.

  curve <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) / 12

  monthlyValuesMatrix <- matrix(data = 0, nrow = nrow(roles), ncol = (length(BVE$years) * length(curve)))

  for (i in seq_along(roles$ScenarioID)) {
    annual <- annualValuesMatrix[i, ]
    monthly <- as.vector(matrix(data = curve, ncol = 1) %*% matrix(data = annual, nrow = 1))
    monthlyValuesMatrix[i, ] <- monthly
  }

  monthStrs <- as.character(1:12)
  s <- sapply(as.character(BVE$years), function(year) {
    paste0(year, ".", monthStrs)
  })

  dimnames(monthlyValuesMatrix) <- list("Role" = roles$RoleID, "Month" = as.vector(s))

  return(list(annualOverheads = annualValuesMatrix, monthlyOverheads = monthlyValuesMatrix))
}

#' Compute And Save Suite Results In Extended Format
#'
#' @param results Results structure (as returned by [RunExperiments()]).
#' @param filepath Destination CSV file to write results. Default = NULL.
#' @param run Name of experiment run
#'
#' @return Data frame version of CSV file.
#' @export
#'
#' @md
#' @examples
#' \dontrun{
#' results <-
#'   pacehrh::RunExperiments(
#'     scenarioName = "MergedModel",
#'     trials = 20
#'   )
#'
#' SR <- pacehrh::SaveExtendedSuiteResults(results, filepath = "_SR.csv", run = "Run-1")
#' CA <- pacehrh::SaveCadreAllocations(SR, filepath = "_CA.csv")
#' cadreOverheadTimes <- pacehrh::SaveCadreOverheadData(filepath = "_COD.csv")
#' summaryStats <- pacehrh::ComputeSummaryStats(SR, CA)
#' }
SaveExtendedSuiteResults <- function(results = NULL, filepath = NULL, run = "Run-1") {
  if (is.null(results)) {
    return(NULL)
  }

  trialIds <- names(results)
  scenario <- BVE$scenario$UniqueID

  # Create the usual output CSV file format
  l <- lapply(seq_along(trialIds), function(index) {
    r <- results[[index]]
    if ("SeasonalityResults" %in% names(r)) {
      return(.saveSuiteResults2(r, scenario, index, run))
    } else {
      return(.saveSuiteResults1(r, scenario, index, run))
    }
  })

  out <- data.table::rbindlist(l)

  # Apply coverage rates by multiplying coverage with num services and service
  # time
  out <-
    merge(
      x = out,
      y = BVE$taskCoverageRates[, c("Indicator", "Year", "Coverage")],
      by.x = c("Task_ID", "Year"),
      by.y = c("Indicator", "Year"),
      all.x = TRUE
    )

  # Clear the key data, as it's no longer needed
  setkey(out, NULL)

  coverageNumServices <- out$Num_services * out$Coverage

  # Round Coverage_num_services to an integer, unless rounding is turned off
  if (GPE$roundingLaw != "none") {
    coverageNumServices <- round(coverageNumServices, 0)
  }

  out$Coverage_num_services <- coverageNumServices


  # Convert Service_time from minutes to hours
  out$Service_time <- out$Service_time / 60.0

  coverageServiceTime <- out$Service_time * out$Coverage
  out$Coverage_service_time <- coverageServiceTime

  # Grab the Tasks table
  tasks <- BVE$taskData
  data.table::setDT(tasks)

  # Add a total number-of-contacts value to the Tasks table
  tasks$NumContactsAnnual[is.na(tasks$NumContactsAnnual)] <- 0
  tasks$NumContactsPerUnit[is.na(tasks$NumContactsPerUnit)] <- 0
  tasks$NumContactsPer <-
    tasks$NumContactsPerUnit + tasks$NumContactsAnnual

  # nolint start

  # Filter the Tasks table column set
  tasks <-
    tasks[, .(
      Indicator,
      CommonName,
      ClinicalOrNon,
      ClinicalCat,
      ServiceCat,
      NumContactsPer
    )]

  # Join Scenario information to the output
  out <- dplyr::left_join(out,
    BVE$scenario[c(
      "UniqueID",
      "WeeksPerYr",
      "HrsPerWeek",
      "BaselinePop",
      "DeliveryModel"
    )],
    by = dplyr::join_by(Scenario_ID == UniqueID)
  )

  # Join Task information to the output
  out <- dplyr::left_join(out,
    tasks,
    by = dplyr::join_by(Task_ID == Indicator)
  )

  # Create a stochastic hours per week value that's different for each year.
  sdvalue <-
    BVE$stochasticParams$p[BVE$stochasticParams$Value == "Hours per week"]

  # TODO: find a more efficient calculation.
  stochasticHrs <- out[, .(Count = .N), by = .(Trial_num, Year)]

  stochasticHrs$HPW_stochastic <-
    runif(
      nrow(stochasticHrs),
      min = BVE$scenario$HrsPerWeek * (1 - sdvalue),
      max = BVE$scenario$HrsPerWeek * (1 + sdvalue)
    )

  stochasticHrs$HPW_stochastic[stochasticHrs$HPW_stochastic > 40] <- 40
  stochasticHrs$Count <- NULL

  out <- dplyr::left_join(out,
    stochasticHrs,
    by = dplyr::join_by(Trial_num == Trial_num, Year == Year)
  )

  # nolint end

  if (!is.null(filepath)) {
    data.table::fwrite(out,
      file = filepath,
      row.names = FALSE,
      na = "NA"
    )
  }

  return(out)
}

#' Compute And Save Cadre Allocations
#'
#' Different healthcare delivery models involve different mixtures of types of
#' healthcare workers ("Cadres"). [SaveCadreAllocations()] uses cadre
#' information read earlier as part of running an experiment, then combines the
#' allocation percentages with computed task times to produce a table showing
#' how much time each cadre spends per year on each healthcare task.
#'
#' @param suiteResults Extended suite results as returned by [SaveExtendedSuiteResults()]
#' @param filepath File location to save cadre allocations. Default = NULL
#' @param annual Report as annual times instead of monthly? Default = TRUE
#'
#' @return data.table
#' @export
#'
#' @md
#' @examples
#' \dontrun{
#' results <-
#'   pacehrh::RunExperiments(
#'     scenarioName = "MergedModel",
#'     trials = 20
#'   )
#'
#' SR <- pacehrh::SaveExtendedSuiteResults(results, filepath = "_SR.csv", run = "Run-1")
#' CA <- pacehrh::SaveCadreAllocations(SR, filepath = "_CA.csv")
#' cadreOverheadTimes <- pacehrh::SaveCadreOverheadData(filepath = "_COD.csv")
#' summaryStats <- pacehrh::ComputeSummaryStats(SR, CA)
#' }
SaveCadreAllocations <- function(suiteResults, filepath = NULL, annual = TRUE) {
  # Do some sanity checking
  if (!.scaSanityCheck(suiteResults)) {
    return(NULL)
  }

  # nolint start

  # Filter the extended suite results
  allocCalcColumns <- c(
    "Task_ID",
    "Scenario_ID",
    "DeliveryModel",
    "Trial_num",
    "Year",
    "Month",
    "Service_time",
    "Coverage_service_time",
    "WeeksPerYr"
  )

  suiteResults <- suiteResults[, ..allocCalcColumns]

  # Convert from per-month to per-year Coverage_service_time stats
  if (annual) {
    suiteResults <-
      suiteResults[, .(Coverage_service_time = sum(Coverage_service_time)), by = .(
        Task_ID,
        Year,
        Trial_num,
        Scenario_ID,
        DeliveryModel,
        WeeksPerYr
      )]
  }

  # nolint end

  # Build a list of values on which to join the cadre allocation table with the
  # suite results The cadre allocation table defines start years for each
  # allocation model. So if the list of start years in the cadre allocation
  # table are {2020, 2025, 2030}, then the 2020 values apply to years 2020-2024,
  # etc.
  refYears <- sort(unique(BVE$taskCadresData$Year))

  joinYear <- suiteResults$Year

  for (yr in unique(suiteResults$Year)) {
    # Select the largest refYear value less than or equal to yr
    ry <- refYears[refYears <= yr]
    if (length(ry) == 0) {
      refVal <- NA_integer_
    } else {
      refVal <- max(ry)
    }
    mask <- (suiteResults$Year == yr)
    joinYear[mask] <- refVal
  }

  # nolint start

  # Add the joinYear column to the suite results, then use it to join to the
  # cadre allocation table. Afterwards delete the joinYear column.
  suiteResults$joinYear <- joinYear
  suiteResults <-
    dplyr::left_join(
      suiteResults,
      BVE$taskCadresData,
      by = dplyr::join_by(Task_ID == Indicator, joinYear == Year)
    )
  suiteResults$joinYear <- NULL

  # nolint end

  # Take the allocation percentages, and combine with the time values to create
  # allocated times.
  srcFlds <- names(BVE$taskCadresData)[-c(1, 2)]
  dstFlds <- paste0(srcFlds, "_Alloc")

  for (i in seq_along(srcFlds)) {
    x <- suiteResults[[srcFlds[i]]]
    x <- x * (suiteResults$Coverage_service_time) / 100.0
    suiteResults[, (dstFlds[i]) := x]
  }

  if (!is.null(filepath)) {
    data.table::fwrite(suiteResults,
      file = filepath,
      row.names = FALSE,
      na = "NA"
    )
  }

  return(suiteResults)
}

.scaSanityCheck <- function(suiteResults) {
  scenarioName <- BVE$scenario$UniqueID

  if (isFALSE(all.equal(GPE$years, unique(suiteResults$Year)))) {
    traceMessage("Configured year range (GPE) does not match reported year range")
    return(FALSE)
  }

  if (length(unique(suiteResults$Scenario_ID)) != 1) {
    traceMessage("More than one scenario in suite results")
    return(FALSE)
  }

  if (unique(suiteResults$Scenario_ID) != scenarioName) {
    traceMessage(paste0("Suite results don't match experiment scenario (", scenarioName, ")"))
    return(FALSE)
  }

  if (is.null(BVE$taskCadresData)) {
    traceMessage(paste0("No task-to-cadre allocation data"))
    return(FALSE)
  }

  return(TRUE)
}
