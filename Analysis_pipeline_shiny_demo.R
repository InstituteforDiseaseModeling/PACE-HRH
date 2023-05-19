#libraries
library(readxl)
library(plyr)
library(dplyr)
library(pacehrh)
library(devtools)
library(beepr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(viridis)
library(ggrepel)
library(scales)
library(treemapify)
library(RColorBrewer)

##############################################################################################################################################
# Run simulations
##############################################################################################################################################
rm(list = ls())

pacehrh::Trace(TRUE)
pacehrh::SetInputExcelFile("./config/model_inputs_shiny_demo.xlsx")
pacehrh::InitializePopulation()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()
pacehrh::SetRoundingLaw("Late")

scenarios <- read_xlsx("config/model_inputs_shiny_demo.xlsx",sheet="Scenarios")

numtrials <- 10
date <- Sys.Date()
usefuldescription <- "demo"

# Run through the full scenario list.
for (i in 1:nrow(scenarios)){
  print(paste("Starting scenario",i))
  scenario <- scenarios$UniqueID[i]
  results <- pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)
  # extract population predictions of the model
  resultspop <- SaveSuiteDemographics(results) %>%
    pivot_longer(c("Female", "Male"), names_to ="Gender", values_to = "Population") %>%
    mutate(Scenario_ID = scenarios$UniqueID[i])
  if(!exists('popsummary')){
    popsummary <- resultspop
  }else{
    popsummary <- rbind(popsummary,resultspop)
  }
  # extract fertility rates predictions of the model
  resultsrates <- GetSuiteRates(results, "femaleFertility") %>%
    mutate(Scenario_ID = scenarios$UniqueID[i])
  if(!exists('fertilityrates')){
    fertilityrates <- resultsrates
  }else{
    fertilityrates <- rbind(fertilityrates,resultsrates)
  }
  #save simulation results to csv files, by scenario
  pacehrh::SaveSuiteResults(results, paste("results/results_",usefuldescription,"_",scenario,"_",date,".csv",sep=""), scenario, 1)
}

beep()

############################################################################################################################################
#Post-processing steps: read and collate simulation results into summary statistics tables to be used in analyses
############################################################################################################################################
resultsFiles <- vector(mode='list', 0)
for (i in 1:nrow(scenarios)){
  resultsFiles <- c(resultsFiles, paste("results/results_",usefuldescription,"_",scenarios$UniqueID[i],"_",date,".csv",sep=""))
}

print("Read and collate results")
DR_test <- pacehrh::ReadAndCollateSuiteResults(files = resultsFiles)
print("Compute Cadre allocation")
CA <- pacehrh:::ComputeCadreAllocations(DR_test)
print("Compute summary stats")
SS <- pacehrh:::ComputeSummaryStats(DR_test, CA)
print("Attach scenario details")
Mean_ServiceCat <- SS$Mean_ServiceCat %>%
  inner_join(scenarios, by= c("Scenario_ID" = "UniqueID"))
Mean_MonthlyTask <- SS$Mean_AnnualTask %>% 
  inner_join(scenarios, by=c("Scenario_ID" = "UniqueID"))
Stats_TotClin <- SS$Stats_TotClin %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))
Mean_ClinCat <- SS$Mean_ClinCat %>%
  inner_join(scenarios, by=c("Scenario_ID"="UniqueID", "WeeksPerYr"))
Mean_Total <- SS$Mean_Total %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))
Stats_ClinMonth <- SS$Stats_ClinMonth %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))
ByRun_ClinMonth <- SS$ByRun_ClinMonth %>% 
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr", "HrsPerWeek"))
Mean_Alloc <- SS$Mean_Alloc %>%
  inner_join(scenarios, by= c("Scenario_ID"="UniqueID", "WeeksPerYr"))

############################################################################################################################################
# Plots
############################################################################################################################################
StartYear = min(Mean_ClinCat$Year)
EndYear = max(Mean_ClinCat$Year)

# population predictions
pop_sim <- popsummary %>%
  group_by(Scenario_ID, Year, Gender, Age) %>%
  summarize(Population=mean(Population)) %>% 
  filter(Year == StartYear | Year == EndYear) %>% 
  group_by(Scenario_ID, Year) %>% 
  mutate(totalpop = sum(Population)) %>% 
  group_by(Scenario_ID) %>% 
  mutate(startpop = round(min(totalpop),0), endpop = round(max(totalpop),0)) %>% 
  mutate(Scenario_label = paste(Scenario_ID, 
                                format(startpop, big.mark = ","),
                                "Start Pop",
                                format(endpop, big.mark = ","),
                                "End Pop",
                                sep=" "))

plot <- ggplot()+
  theme_bw()+
  geom_point(data = pop_sim, aes(x=Age, y=Population, color=Gender, shape=as.factor(Year)), position = "jitter")+
  facet_wrap(~Scenario_label) +
  labs(color = "Gender", shape = "Year")
print(plot)

# fertility rates predictions
age20_39 <- c("AnnualBirthRate20_24", "AnnualBirthRate25_29", "AnnualBirthRate30_34", "AnnualBirthRate35_39")
rates_sim <- fertilityrates %>% 
  filter(Year >= StartYear & Year <= EndYear) %>% 
  filter(Label %in% age20_39)
plot <- ggplot(rates_sim, aes(x = Year, y = Rate, color = Label, group = Year)) +
  theme_bw() +
  geom_boxplot() +
  theme(legend.position = "none", axis.text.x = element_text(angle=-90, vjust = .5, hjust=1)) +
  facet_grid(Label ~ Scenario_ID)
print(plot)

# time allocation by Clinical Category
temp_clin <- Mean_ClinCat %>% 
  filter(Year >= StartYear & Year <= EndYear) %>% 
  mutate(Category = case_when(
    ClinicalOrNon != "Clinical" ~ ClinicalOrNon,
    ClinicalOrNon == "Clinical" ~ paste("Clinical -", ClinicalCat))) %>% 
  mutate(Alpha = case_when(
    ClinicalOrNon == "Clinical" ~ 0.3,
    ClinicalOrNon != "Clinical" ~ 1)) %>% 
  mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" "))  
temp_clin$Category <- factor(temp_clin$Category,ordered=TRUE,levels=unique(temp_clin$Category))

temp_total <- Mean_Total %>% 
  filter(Year >= StartYear & Year <= EndYear) %>% 
  mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" "))

ylabel <-  "Hours per Week per Catchment Pop"
maxyval <- max(Mean_Total$CI95/Mean_Total$WeeksPerYr)*1.05

plot <- ggplot()+
  geom_bar(data = temp_clin, aes(x=Year,y=MeanHrs/WeeksPerYr,fill=Category),stat="identity",alpha=.9)+
  geom_line(data = temp_total, aes(x=Year,y=MeanHrs/WeeksPerYr),linewidth=1.2)+
  geom_point(data = temp_total, aes(x=Year,y=MeanHrs/WeeksPerYr))+
  geom_errorbar(data =temp_total, aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
  ylim(0,maxyval)+
  theme_bw()+
  scale_x_continuous(breaks = seq(StartYear,EndYear))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
  scale_fill_viridis_d()+
  facet_wrap(~Scenario_label)+
  ylab(ylabel) + xlab("") + labs(title = paste("Time Allocation by Clinical Category"))

print(plot)

# time allocation by Service Category, Bar Plot
ServiceCat_Clinical <- Mean_ServiceCat %>%
  subset(ClinicalOrNon=="Clinical") %>%
  filter(Year >= StartYear & Year <= EndYear) %>% 
  mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" ")) %>% 
  group_by(Scenario_ID, Year) %>%
  dplyr::mutate(TotalHrs=sum(MeanHrs)) 
temp_TotClin <- Stats_TotClin %>% 
  filter(Year >= StartYear & Year <= EndYear) %>% 
  mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" "))
ymax <- max(temp_TotClin$CI95/temp_TotClin$WeeksPerYr)*1.05

plot <- ggplot() +
  theme_bw()+
  geom_bar(data=ServiceCat_Clinical,aes(x=Year,y=MeanHrs/WeeksPerYr,fill=ServiceCat),stat="identity",alpha=.9)+
  geom_line(data=temp_TotClin,aes(x=Year,y=CI50/WeeksPerYr),linewidth=1.2)+
  geom_point(data=temp_TotClin,aes(x=Year,y=CI50/WeeksPerYr))+
  geom_errorbar(data=temp_TotClin,aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
  ylim(0, ymax) +
  facet_wrap(~Scenario_label) +
  scale_x_continuous(breaks =  c(2021,2025, 2030, 2035))+
  theme(legend.title=element_blank(),legend.position = c(0.02, 1), legend.justification = c(0.02, 1), 
        legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
  #theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
  scale_fill_brewer(palette = "BrBG", direction = -1)+
  labs(x="Year", y="Hours per Week per Catchment Pop")
print(plot)  

# time allocation by Service Category, Tile Plot
unique(Mean_ServiceCat$ServiceCat)
temp_ServiceCat <- Mean_ServiceCat %>% 
  filter(Year == StartYear) %>% 
  mutate(ServiceLabel = case_when(
    ServiceCat == "Family planning" ~ "FP",
    ServiceCat == "Immunization" ~ "RI",
    ServiceCat == "Overhead 1" | ServiceCat == "Overhead 2" ~ "Admin",
    ServiceCat == "Record keeping" ~ "Records",
    ServiceCat == "Nutrition" ~ "Nutri",
    ServiceCat == "Sick child" ~ "IMNCI",
    ServiceCat == "Disease surveillance for reportable diseases" ~ "DS", 
    T ~ ServiceCat)) %>% 
  filter(ClinicalOrNon == "Clinical")
  
plot <- ggplot(temp_ServiceCat,aes(area=MeanHrs,fill=ServiceLabel,label=ServiceLabel,subgroup=ServiceLabel))+
    geom_treemap()+geom_treemap_text(color="black",place="center",size=16)+
    geom_treemap_subgroup_border(color="black",size=2.5)+
    facet_wrap(~Scenario_ID) +
    theme_bw()+theme(legend.position = "none")+
    scale_fill_viridis_d()
print(plot)
  
# service mix change over time, Line Plot

ServiceCat_Clinical <- Mean_ServiceCat %>%
  subset(ClinicalOrNon=="Clinical" & ServiceCat!="HIV") %>%
  filter(Year >= StartYear & Year <= EndYear) %>% 
  group_by(Scenario_ID, ServiceCat) %>% 
  dplyr::mutate(MeanHrs_Start = dplyr::first(MeanHrs), RatioTo1 = MeanHrs/MeanHrs_Start) %>% 
  dplyr::mutate(RatioLastYr = case_when(
    Year == max(Year) ~ RatioTo1)) %>% 
  dplyr::mutate(RatioLabel = case_when(
    Year == max(Year) ~ paste(ServiceCat, round(RatioTo1,1), sep = ","))) 
ServiceCat_Clinical$ServiceCat = as.factor(ServiceCat_Clinical$ServiceCat)

yplotmax = max(ServiceCat_Clinical$RatioTo1)*1.02
yplotmin = min(ServiceCat_Clinical$RatioTo1)*0.98

plot <- ggplot(ServiceCat_Clinical,aes(x=Year,y=RatioTo1,group=ServiceCat) )+
  geom_line(aes(color=ServiceCat),size=1.1) +
  geom_hline(yintercept = 1,color="black",linetype="dashed") +
  theme_bw() +
  scale_color_discrete()+
  geom_text(aes(x=max(Year)+.2,y=RatioLastYr,label=RatioLabel),color="darkgrey",size=3.5, hjust=0, nudge_x = 0.5) +
  facet_wrap(~Scenario_ID) +
  scale_x_continuous(breaks = seq(StartYear,EndYear),limits=c(StartYear,max(ServiceCat_Clinical$Year)+6)) +
  scale_y_continuous(limits = c(yplotmin,yplotmax)) +
  theme(legend.title = element_blank(), legend.position="bottom",axis.text.x = element_text(angle=-90, vjust = .5, hjust=1)) +
  labs(x = "", y = "Ratio to Baseline Year")

print(plot)

# time allocation by Cadre 
unique(Mean_Alloc$Cadre)

Cadre_labelled <- Mean_Alloc %>% 
  filter(CI50!=0 ) %>% 
  mutate(CadreLabel = case_when(
    Cadre == "HEW_hrs" ~ 1,
    Cadre == "FH_hrs" ~ 2,
    Cadre == "UN_hrs" ~ 3)) %>% 
  filter(CadreLabel!=3) %>% 
  group_by(Scenario_ID, Year) %>% 
  mutate(sum_CI50 = sum(CI50), sum_CI05 = sum(CI05), sum_CI95 = sum(CI95))

Cadre_labelled$CadreLabel <- factor(Cadre_labelled$CadreLabel, levels = c(1, 2), labels = c("Health Extension Worker", "Family Health Professional"))


plot <-  ggplot(data=Cadre_labelled)+
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
print(plot)

# seasonality plot with uncertainty bounds
Monthly_NonClinical <- Mean_ClinCat %>% 
  subset(ClinicalOrNon != "Clinical") %>% 
  group_by(Scenario_ID, Year) %>% 
  dplyr::summarize(NonClinical_Monthly = sum(MeanHrs)/12)

RatioToAvg_ByMonth <- ByRun_ClinMonth %>% 
  subset(Year == EndYear) %>% 
  left_join(Monthly_NonClinical, by = c("Scenario_ID", "Year"))  %>% 
  mutate(NonClinical_Monthly = replace_na(NonClinical_Monthly,0)) %>% 
  group_by(Scenario_ID, Trial_num, Year) %>% 
  dplyr::mutate(MeanMonthHrs = (mean(TotHrs)+NonClinical_Monthly), RatioToMean = (TotHrs+NonClinical_Monthly)/(MeanMonthHrs)) %>% 
  ungroup() %>% 
  group_by(Scenario_ID, Month) %>% 
  dplyr::summarize(RatioToMean_p05 = quantile(RatioToMean, 0.05),
            RatioToMean_p25 = quantile(RatioToMean, 0.25),
            RatioToMean_p50 = quantile(RatioToMean, 0.50),
            RatioToMean_p75 = quantile(RatioToMean, 0.75),
            RatioToMean_p95 = quantile(RatioToMean, 0.95))

plot <- ggplot(data=RatioToAvg_ByMonth)+
  theme_bw()+
  geom_ribbon(aes(x = Month, ymin = RatioToMean_p05, ymax = RatioToMean_p95), fill = "#80B1D3",  alpha = 0.25)+
#  geom_line(aes(x = Month, y=RatioToMean_p50),linewidth=1)+
  geom_smooth(aes(x = Month, y=RatioToMean_p50), method ="loess", fill = "transparent", span=0.5, alpha = 0.25)+
  geom_hline(yintercept = 1, color = "blue", linetype="dashed")+
  scale_color_manual("#80B1D3")+
  ylim(0.85, 1.15) +
  scale_x_continuous(breaks =  seq(1, 12))+
  facet_wrap(~Scenario_ID)+
  labs(x = "Month", y="Ratio of workload for the month to annual average")
print(plot)



