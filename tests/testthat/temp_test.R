rm(list = ls())
plot_cadre <- function(Mean_Alloc, startyear, endyear){
  Cadre_labelled <- Mean_Alloc %>% 
    filter(CI50!=0 ) %>% 
    filter(Year >= startyear & Year <= endyear) %>% 
    dplyr::mutate(CadreLabel = case_when(
      Cadre == "HEW_hrs" ~ 1,
      Cadre == "FH_hrs" ~ 2,
      Cadre == "UN_hrs" ~ 3)) %>% 
    filter(CadreLabel!=3) %>% 
    group_by(Scenario_ID, Year) %>% 
    dplyr::mutate(sum_CI50 = sum(CI50), sum_CI05 = sum(CI05), sum_CI95 = sum(CI95))
  
  Cadre_labelled$CadreLabel <- factor(Cadre_labelled$CadreLabel, levels = c(1, 2), labels = c("Health Extension Worker", "Family Health Professional"))
  
  ggplot(data=Cadre_labelled) +
    geom_bar(aes(x=Year,y=CI50/WeeksPerYr,fill=CadreLabel),stat="identity",alpha=.9)+
    geom_line(aes(x=Year,y=sum_CI50/WeeksPerYr),linewidth=1.2)+
    geom_point(aes(x=Year,y=sum_CI50/WeeksPerYr))+
    geom_errorbar(aes(x=Year,ymin=sum_CI05/WeeksPerYr, ymax=sum_CI95/WeeksPerYr), colour="black", width=.3)+
    theme_bw()+
    scale_x_continuous(breaks =  c(2021,2025, 2030, 2035))+
    theme(legend.title=element_blank(),legend.position = c(0.02, 0.99), legend.justification = c(0.02, 0.99), 
          legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
    scale_fill_brewer(palette = "Paired", direction = -1)+
    facet_wrap(~Scenario_ID)+
    labs(x="Year", y="Hours per Week per 5,000 Pop", fill = "Cadre", title = "Panel A")
  
}


input_file <- "config/model_inputs_demo.xlsx"
# input_file <- "config/model_inputs.xlsx"
pacehrh::Trace(TRUE)
pacehrh::InitializePopulation()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()
pacehrh::SetRoundingLaw("Late")
pacehrh::SetInputExcelFile(input_file)


start = 2020
end = 2035
pacehrh::SetGlobalStartEndYears(2020, 2035)
numtrials <- 10
scenarios <- readxl::read_excel(input_file, sheet = "Scenarios")[1,]
list_CAs <- list()
list_REs <- list()
for (scenario in scenarios$UniqueID){
  results <- pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = TRUE)
  ER <-  pacehrh::SaveExtendedSuiteResults(results)
  CA <- ER %>% pacehrh::SaveCadreAllocations()
  list_CAs <- append(list_CAs, list(CA))
  list_REs <- append(list_REs, list(ER))
  if (exists("CAs")){
    CAs <- rbind(CAs, CA, fill=TRUE)
  }
  else
  {
    CAs <- CA
  }
  if (exists("ERs")){
    ERs <- rbind(ERs, ER)
  }
  else{
    ERs <- ER  
  }
}
CAs[is.na(CAs)] <- 0
ERs[is.na(ERs)] <- 0
SS <- pacehrh::ComputeSummaryStats(ERs, CAs)
Mean_Alloc <- SS$Mean_Alloc %>% inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr"))

# Try with individual compute:

for (i in seq(length(list_REs))){
  print(paste0("item:", i))
  ss <- pacehrh::ComputeSummaryStats(list_REs[[i]], list_CAs[[i]])
  if (exists("Mean_Alloc2")){
    Mean_Alloc2 <- rbind(Mean_Alloc2, ss$Mean_Alloc)
  }
  else{
    Mean_Alloc2 <- ss$Mean_Alloc
  }
}
# plot2 <- plot_cadre(Mean_Alloc2, start, end)
# print(plot2)

plot <- plot_cadre(Mean_Alloc, start, end)
print(plot)


  