#' Compute Summary Statistics
#'
#' @param DR From [ReadAndCollateSuiteResults()] function
#' @param CA From [ComputeCadreAllocations()] function
#'
#' @return List of computed summary tables
#' @export
#'
#' @examples
#' \dontrun{
#'
#' results <- pacehrh::RunExperiments(scenarioName = "scenario", trials = 10)
#' DR <- pacehrh::SaveExtendedSuiteResults(results)
#' CA <- pacehrh::SaveCadreAllocations(DR)
#' stats <- pacehrh::ComputeSummaryStats(DR, CA)
#' }
ComputeSummaryStats <- function(DR = NULL, CA = NULL) {
  if (is.null(DR) || is.null(CA)) {
    return(NULL)
  }

  traceMessage("Making ClinCat tables")

  # nolint start
  byRunClinCat <-
    DR[,
      .(TotHrs = sum(Coverage_service_time)),
      keyby = .(Scenario_ID, Trial_num, Year, ClinicalCat, ClinicalOrNon, WeeksPerYr)
    ]


  meanClinCat <-
    byRunClinCat[,
      .(
        CI05 = quantile(TotHrs, probs = c(.05)),
        CI25 = quantile(TotHrs, probs = c(.25)),
        MeanHrs = mean(TotHrs),
        CI75 = quantile(TotHrs, probs = c(.75)),
        CI95 = quantile(TotHrs, probs = c(.95))
      ),
      keyby = .(Scenario_ID, Year, ClinicalCat, ClinicalOrNon, WeeksPerYr)
    ]
  # nolint end


  traceMessage("Making ClinMonth tables")

  # nolint start
  byRunClinMonth <-
    DR[ClinicalOrNon == "Clinical",
      .(TotHrs = sum(Coverage_service_time)),
      keyby = .(Scenario_ID, Trial_num, Year, Month, WeeksPerYr, HrsPerWeek)
    ]

  statsClinMonth <-
    byRunClinMonth[,
      .(
        CI05 = quantile(TotHrs, probs = c(.05)),
        CI25 = quantile(TotHrs, probs = c(.25)),
        CI50 = mean(TotHrs),
        CI75 = quantile(TotHrs, probs = c(.75)),
        CI95 = quantile(TotHrs, probs = c(.95))
      ),
      keyby = .(Scenario_ID, Year, Month, WeeksPerYr, HrsPerWeek)
    ]
  # nolint end

  traceMessage("Making ServiceCat tables")

  # nolint start
  DR2 <- data.table::setDT(DR)
  DR2[ServiceCat %in% c("Schisto MDA", "LLINs and IRS", "Vector control"), ServiceCat := "Campaign"]

  byRunServiceCat <-
    DR2[,
      .(TotHrs = sum(Coverage_service_time)),
      keyby = .(Scenario_ID, Trial_num, Year, ServiceCat, ClinicalOrNon)
    ]

  meanServiceCat <-
    byRunServiceCat[,
      .(
        CI05 = quantile(TotHrs, probs = c(.05)),
        CI25 = quantile(TotHrs, probs = c(.25)),
        MeanHrs = mean(TotHrs),
        CI75 = quantile(TotHrs, probs = c(.75)),
        CI95 = quantile(TotHrs, probs = c(.95))
      ),
      keyby = .(Scenario_ID, Year, ServiceCat, ClinicalOrNon)
    ]
  # nolint end

  traceMessage("Making AnnualTask tables")

  # nolint start
  meanAnnualTask <-
    DR2[,
      .(
        CI05 = quantile(Coverage_service_time, probs = c(.05)),
        CI25 = quantile(Coverage_service_time, probs = c(.25)),
        MeanHrs = mean(Coverage_service_time),
        CI75 = quantile(Coverage_service_time, probs = c(.75)),
        CI95 = quantile(Coverage_service_time, probs = c(.95))
      ),
      keyby = .(Scenario_ID, Year, Task_ID, ServiceCat, ClinicalOrNon)
    ]
  # nolint end

  traceMessage("Making Total tables")

  # nolint start
  byRunTotal <-
    DR2[,
      .(TotHrs = sum(Coverage_service_time)),
      keyby = .(Scenario_ID, Trial_num, Year, WeeksPerYr, HrsPerWeek)
    ]

  meanTotal <-
    byRunTotal[,
      .(
        CI05 = quantile(TotHrs, probs = c(.05)),
        CI25 = quantile(TotHrs, probs = c(.25)),
        MeanHrs = mean(TotHrs),
        CI75 = quantile(TotHrs, probs = c(.75)),
        CI95 = quantile(TotHrs, probs = c(.95))
      ),
      keyby = .(Scenario_ID, Year, WeeksPerYr, HrsPerWeek)
    ]
  # nolint end

  traceMessage("Making TotClin tables")

  # nolint start
  byRunTotClin <-
    DR2[ClinicalOrNon == "Clinical",
      .(AnnualHrs = sum(Coverage_service_time)),
      keyby = .(Scenario_ID, Trial_num, Year, WeeksPerYr, HrsPerWeek)
    ]

  statsTotClin <-
    byRunTotClin[,
      .(
        CI05 = quantile(AnnualHrs, probs = c(.05)),
        CI25 = quantile(AnnualHrs, probs = c(.25)),
        CI50 = mean(AnnualHrs),
        CI75 = quantile(AnnualHrs, probs = c(.75)),
        CI95 = quantile(AnnualHrs, probs = c(.95))
      ),
      keyby = .(Scenario_ID, Year, WeeksPerYr, HrsPerWeek)
    ]
  # nolint end

  traceMessage("Making Alloc tables")

  # Find the time columns (suffix = "Alloc") and sum over them, aggregating by
  # year and trial number (run)
  cols <- colnames(CA)
  timeCols <- cols[grep("_Alloc", cols)]

  # nolint start
  byRunAlloc <- CA[,
    lapply(.SD, sum),
    keyby = .(Scenario_ID, DeliveryModel, Trial_num, Year, WeeksPerYr),
    .SDcols = timeCols
  ]

  cols <- colnames(byRunAlloc)
  destCols <- sub("_Alloc", "_hrs", cols)
  colnames(byRunAlloc) <- destCols

  # Melt the resulting table to convert multiple time columns each representing
  # a different cadre member into rows.
  suppressWarnings(
    byRunAllocMelt <-
      data.table::melt(
        byRunAlloc,
        id.vars = c("Scenario_ID", "DeliveryModel", "Trial_num", "Year", "WeeksPerYr"),
        variable.name = "Cadre",
        value.name = "AnnualHrs",
        variable.factor = FALSE
      )
  )
  # nolint end

  # nolint start
  byRunAllocMelt <- byRunAllocMelt[!is.na(AnnualHrs)]

  meanAlloc <-
    byRunAllocMelt[,
      .(
        CI05 = quantile(AnnualHrs, probs = c(.05)),
        CI25 = quantile(AnnualHrs, probs = c(.25)),
        CI50 = mean(AnnualHrs),
        CI75 = quantile(AnnualHrs, probs = c(.75)),
        CI95 = quantile(AnnualHrs, probs = c(.95))
      ),
      keyby = .(Scenario_ID, DeliveryModel, Year, WeeksPerYr, Cadre)
    ]
  # nolint end

  return(list(
    "ByRun_ClinCat" = byRunClinCat,
    "Mean_ClinCat" = meanClinCat,
    "ByRun_ClinMonth" = byRunClinMonth,
    "Stats_ClinMonth" = statsClinMonth,
    "ByRun_ServiceCat" = byRunServiceCat,
    "Mean_ServiceCat" = meanServiceCat,
    "Mean_AnnualTask" = meanAnnualTask,
    "ByRun_Total" = byRunTotal,
    "Mean_Total" = meanTotal,
    "ByRun_TotClin" = byRunTotClin,
    "Stats_TotClin" = statsTotClin,
    "ByRun_Alloc" = byRunAlloc,
    "Mean_Alloc" = meanAlloc
  ))
}
