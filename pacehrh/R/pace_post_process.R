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

  # Find the time columns (suffix = "Alloc") and sum over them, aggregating by
  # year and trial number (run)
  cols <- colnames(CA)
  timeCols <- cols[grep("_Alloc", cols)]

  ByRun_Alloc <- CA[,
                    lapply(.SD, sum),
                    keyby = .(Scenario_ID, DeliveryModel, Trial_num, Year, WeeksPerYr),
                    .SDcols = timeCols]

  cols <- colnames(ByRun_Alloc)
  destCols <- sub("_Alloc", "_hrs", cols)
  colnames(ByRun_Alloc) <- destCols

  # Melt the resulting table to convert multiple time columns each representing
  # a different cadre member into rows.
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
