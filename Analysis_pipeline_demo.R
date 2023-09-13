#libraries
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(viridis)
library(ggrepel)
library(scales)
library(treemapify)
library(RColorBrewer)

date <- Sys.Date() #User should make sure this matches with date in names of csv files to be read in
usefuldescription <- "test" #User should make sure this matches with names of csv files to be read in
scenarios <- read_xlsx("config/model_inputs_demo.xlsx",sheet="Scenarios")


#read in summary statistics csv files generated from post-processing steps in "Run_simulations.R"
Mean_ServiceCat <- read.csv(paste("results/Mean_ServiceCat_",usefuldescription,"_",date,".csv",sep=""))
Stats_TotClin <- read.csv(paste("results/Stats_TotClin_",usefuldescription,"_",date,".csv",sep=""))
Mean_ClinCat <- read.csv(paste("results/Mean_ClinCat_",usefuldescription,"_",date,".csv",sep=""))
Mean_Total <- read.csv(paste("results/Mean_Total_",usefuldescription,"_",date,".csv",sep=""))
Stats_ClinMonth <- read.csv(paste("results/Stats_ClinMonth_",usefuldescription,"_",date,".csv",sep=""))
Mean_Alloc <- read.csv(paste("results/Mean_Alloc_",usefuldescription,"_",date,".csv",sep=""))


############################################################################################################################################

# time allocation by Clinical Category
temp_clin <- Mean_ClinCat %>% 
  filter(Year >= 2021 & Year <= 2035) %>% 
  dplyr::mutate(Category = case_when(
    ClinicalOrNon != "Clinical" ~ ClinicalOrNon,
    ClinicalOrNon == "Clinical" ~ paste("Clinical -", ClinicalCat))) %>% 
  dplyr::mutate(Alpha = case_when(
    ClinicalOrNon == "Clinical" ~ 0.3,
    ClinicalOrNon != "Clinical" ~ 1)) %>% 
  dplyr::mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" "))  
temp_clin$Category <- factor(temp_clin$Category,ordered=TRUE,levels=unique(temp_clin$Category))

temp_total <- Mean_Total %>% 
  filter(Year >= 2021 & Year <= 2035) %>% 
  dplyr::mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" "))

ylabel <-  "Hours per Week per Catchment Pop"
maxyval <- max(Mean_Total$CI95/Mean_Total$WeeksPerYr)*1.05

ggplot()+
  geom_bar(data = temp_clin, aes(x=Year,y=MeanHrs/WeeksPerYr,fill=Category),stat="identity",alpha=.9)+
  geom_line(data = temp_total, aes(x=Year,y=MeanHrs/WeeksPerYr),linewidth=1.2)+
  geom_point(data = temp_total, aes(x=Year,y=MeanHrs/WeeksPerYr))+
  geom_errorbar(data =temp_total, aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
  ylim(0,maxyval)+
  theme_bw()+
  scale_x_continuous(breaks = seq(2021,2035))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
  scale_fill_viridis_d()+
  facet_wrap(~Scenario_label)+
  ylab(ylabel) + xlab("") + labs(title = paste("Time Allocation by Clinical Category"))

####################################################################################################

# time allocation by Service Category, Bar Plot
ServiceCat_Clinical <- Mean_ServiceCat %>%
  subset(ClinicalOrNon=="Clinical") %>%
  filter(Year >= 2021 & Year <= 2035) %>% 
  dplyr::mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" ")) %>% 
  group_by(Scenario_ID, Year) %>%
  dplyr::mutate(TotalHrs=sum(MeanHrs)) 
temp_TotClin <- Stats_TotClin %>% 
  filter(Year >= 2021 & Year <= 2035) %>% 
  dplyr::mutate(Scenario_label = paste(Scenario_ID, format(BaselinePop, big.mark = ","),"Starting Pop", sep=" "))
ymax <- max(temp_TotClin$CI95/temp_TotClin$WeeksPerYr)*1.05

ggplot() +
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
  scale_fill_brewer(palette = "BrBG", direction = -1)+
  labs(x="Year", y="Hours per Week per Catchment Pop")

####################################################################################################

# time allocation by Service Category, Tile Plot
unique(Mean_ServiceCat$ServiceCat)
temp_ServiceCat <- Mean_ServiceCat %>% 
  filter(Year == 2021) %>% 
  filter(ClinicalOrNon == "Clinical") %>% 
  dplyr::mutate(ServiceLabel = case_when(
    ServiceCat == "Family planning" ~ "FP",
    ServiceCat == "Immunization" ~ "RI",
    ServiceCat == "Nutrition" ~ "Nutri",
    ServiceCat == "Sick child" ~ "IMNCI",
    T ~ ServiceCat)) 

ggplot(temp_ServiceCat,aes(area=MeanHrs,fill=ServiceLabel,label=ServiceLabel,subgroup=ServiceLabel))+
  geom_treemap()+geom_treemap_text(color="black",place="center",size=16)+
  geom_treemap_subgroup_border(color="black",size=2.5)+
  facet_wrap(~Scenario_ID) +
  theme_bw()+theme(legend.position = "none")+
  scale_fill_viridis_d()

####################################################################################################

# time allocation by Cadre 
unique(Mean_Alloc$RoleDescription)

Cadre_labelled <- Mean_Alloc %>% 
  filter(CI50!=0 & Year >= 2021 & Year <= 2035) %>% 
  group_by(Scenario_ID, Year) %>% 
  dplyr::mutate(sum_CI50 = sum(CI50), sum_CI05 = sum(CI05), sum_CI95 = sum(CI95))

ggplot(data=Cadre_labelled)+
  geom_bar(aes(x=Year,y=CI50/WeeksPerYr,fill=RoleDescription),stat="identity",alpha=.9)+
  geom_line(aes(x=Year,y=sum_CI50/WeeksPerYr),linewidth=1.2)+
  geom_point(aes(x=Year,y=sum_CI50/WeeksPerYr))+
  geom_errorbar(aes(x=Year,ymin=sum_CI05/WeeksPerYr, ymax=sum_CI95/WeeksPerYr), colour="black", width=.3)+
  theme_bw()+
  scale_x_continuous(breaks =  c(2021,2025, 2030, 2035))+
  theme(legend.title=element_blank(),legend.position = c(0.02, 0.99), legend.justification = c(0.02, 0.99), 
        legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
  scale_fill_brewer(palette = "Paired", direction = -1)+
  facet_wrap(~Scenario_ID)+
  labs(x="Year", y="Hours per Week per 5,000 Pop", fill = "Cadre", title = "Time allocation by Cadre")

####################################################################################################

# FTE calculation by Cadre
ymax = max(ceiling(Cadre_labelled$CI50/Cadre_labelled$WeeksPerYr/(Cadre_labelled$HrsPerWeek*Cadre_labelled$MaxUtilization))) + 1

ggplot(data=Cadre_labelled)+
  geom_bar(aes(x=Year,y=ceiling(CI50/WeeksPerYr/(HrsPerWeek*MaxUtilization))),stat="identity",position="stack")+
  theme_bw()+
  scale_x_continuous(breaks = c(2021,2025,2030,2035))+
  ylim(0,ymax)+
  facet_grid(~Scenario_ID)+
  labs(x="Year",y="Total required staff count",title="Required staff count (total)")

ggplot(data=Cadre_labelled)+
  geom_bar(aes(x=Year,y=ceiling(CI50/WeeksPerYr/(HrsPerWeek*MaxUtilization)),fill=RoleDescription),stat="identity",position="stack")+
  theme_bw()+
  scale_x_continuous(breaks = c(2021,2025,2030,2035))+
  ylim(0,ymax)+
  facet_grid(~Scenario_ID)+
  labs(x="Year",y="Minimum staff count",fill="Cadre",title="Minimum staff count by cadre")

ggplot(data=Cadre_labelled)+
  geom_bar(aes(x=Year,y=ceiling(CI50/WeeksPerYr/(HrsPerWeek*MaxUtilization)),fill=RoleDescription),stat="identity")+
  theme_bw()+
  scale_x_continuous(breaks = c(2021,2025,2030,2035))+
  ylim(0,ymax)+
  facet_grid(RoleDescription~Scenario_ID)+
  labs(x="Year",y="Minimum staff count",fill="Cadre",title="Minimum staff count by cadre")


####################################################################################################

# service mix change over time, Line Plot

ServiceCat_Clinical <- Mean_ServiceCat %>%
  subset(ClinicalOrNon=="Clinical" & ServiceCat!="HIV") %>%
  filter(Year >= 2021 & Year <= 2035) %>% 
  group_by(Scenario_ID, ServiceCat) %>% 
  dplyr::mutate(MeanHrs_Start = dplyr::first(MeanHrs), RatioTo1 = MeanHrs/MeanHrs_Start) %>% 
  dplyr::mutate(RatioLastYr = case_when(
    Year == max(Year) ~ RatioTo1)) %>% 
  dplyr::mutate(RatioLabel = case_when(
    Year == max(Year) ~ paste(ServiceCat, round(RatioTo1,1), sep = ","))) 
ServiceCat_Clinical$ServiceCat = as.factor(ServiceCat_Clinical$ServiceCat)

yplotmax = max(ServiceCat_Clinical$RatioTo1)*1.02
yplotmin = min(ServiceCat_Clinical$RatioTo1)*0.98

ggplot(ServiceCat_Clinical,aes(x=Year,y=RatioTo1,group=ServiceCat) )+
  geom_line(aes(color=ServiceCat), linewidth=1.1) +
  geom_hline(yintercept = 1,color="black",linetype="dashed") +
  theme_bw() +
  scale_color_discrete()+
  geom_text_repel(aes(x=max(Year)+.2,y=RatioLastYr,label=RatioLabel),color="darkgrey", max.overlaps =200, size=3.5,hjust=0)+
#  geom_text(aes(x=max(Year)+.2,y=RatioLastYr,label=RatioLabel),color="darkgrey",size=3.5, hjust=0, nudge_x = 0.5) +
  facet_wrap(~Scenario_ID) +
  scale_x_continuous(breaks = seq(2021,2035),limits=c(2021,max(ServiceCat_Clinical$Year)+6)) +
  scale_y_continuous(limits = c(yplotmin,yplotmax)) +
  theme(legend.title = element_blank(), legend.position="bottom",axis.text.x = element_text(angle=-90, vjust = .5, hjust=1)) +
  labs(x = "", y = "Ratio to Baseline Year")

####################################################################################################

# seasonality plot with uncertainty bounds
Monthly_NonClinical <- Mean_ClinCat %>% 
  subset(ClinicalOrNon != "Clinical") %>% 
  group_by(Scenario_ID, Year) %>% 
  dplyr::summarize(NonClinical_Monthly = sum(MeanHrs)/12)

RatioToAvg_ByMonth <- ByRun_ClinMonth %>% 
  subset(Year == 2035) %>% 
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

ggplot(data=RatioToAvg_ByMonth)+
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

