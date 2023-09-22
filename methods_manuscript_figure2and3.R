#libraries
library(devtools)
library(readxl)
library(plyr)
library(dplyr)
library(pacehrh)
library(ggplot2)
library(stringr)
library(reshape2)
library(viridis)
library(ggrepel)
library(scales)
library(treemapify)
library(RColorBrewer)

date <- Sys.Date()-28
GeoName <- "Ethiopia"

Mean_ServiceCat <- read.csv(paste("results/Mean_ServiceCat_",GeoName,"_",date,".csv",sep=""))
Mean_AnnualTask <- read.csv(paste("results/Mean_AnnualTask_",GeoName,"_",date,".csv",sep=""))
Stats_TotClin <- read.csv(paste("results/Stats_TotClin_",GeoName,"_",date,".csv",sep=""))
Mean_ClinCat <- read.csv(paste("results/Mean_ClinCat_",GeoName,"_",date,".csv",sep=""))
Mean_Total <- read.csv(paste("results/Mean_Total_",GeoName,"_",date,".csv",sep=""))
Stats_ClinMonth <- read.csv(paste("results/Stats_ClinMonth_",GeoName,"_",date,".csv",sep=""))
ByRun_ClinMonth <- read.csv(paste("results/ByRun_ClinMonth_",GeoName,"_",date,".csv",sep=""))
Mean_Alloc <- read.csv(paste("results/Mean_Alloc_",GeoName,"_",date,".csv",sep=""))

WeeksPerYr <- 48

#Figure 2 Panel A & Panel B Total workload by clinical category 2021-2035
Mean_NonProductive_labelled <- Mean_ServiceCat %>% 
  filter(Scenario_ID == "Expanded" & ClinicalOrNon != "Clinical") %>% 
  mutate(ClinicalCat = case_when(
    ServiceCat == "Overhead 1" | ServiceCat == "Overhead 2" ~ "Staff overhead",
    ServiceCat == "Record keeping" ~ "Record keeping",
    ServiceCat == "Disease surveillance for reportable diseases" ~ "Disease surveillance")) %>% 
  group_by(Scenario_ID, Year, ClinicalOrNon, ClinicalCat) %>% 
  summarise(CI05 = sum(CI05), CI25 = sum(CI25), MeanHrs = sum(MeanHrs), CI75 = sum(CI75), CI95 = sum(CI95))
  
Mean_ClinCat_labelled <- Mean_ClinCat %>%
  filter(Scenario_ID == "Expanded" & ClinicalOrNon == "Clinical") %>% 
  select(Scenario_ID, Year, ClinicalOrNon, ClinicalCat, CI05, CI25, MeanHrs, CI75, CI95) %>% 
  rbind(Mean_NonProductive_labelled) %>% 
  mutate(ClinicalLabel = case_when(
    ClinicalOrNon == "Non-productive" & ClinicalCat == "Record keeping" ~ 1,
    ClinicalOrNon == "Non-productive" & ClinicalCat == "Staff overhead" ~ 2,
    ClinicalOrNon == "Clinical" & ClinicalCat == "Acute" ~ 3,
    ClinicalOrNon == "Clinical" & ClinicalCat == "Preventive" ~ 4,
    ClinicalOrNon == "Clinical" & ClinicalCat == "Public Health" ~ 5,
    ClinicalOrNon == "Non-clinical" ~ 6)) %>% 
  group_by(Scenario_ID, Year, ClinicalLabel) %>% 
  summarize(MeanHrs = sum(MeanHrs)) %>% 
  ungroup() %>% 
  filter(ClinicalLabel != 6) #taking "Non-clinical" out because the workload from "disease surveillance" is negligible)

Mean_ClinCat_labelled$ClinicalLabel <- factor(Mean_ClinCat_labelled$ClinicalLabel, levels = c(1, 2, 3, 4, 5),
                                              labels = c("Record keeping" , "Staff overhead", "Clinical - Acute", "Clinical - Preventive", "Clinical - Public Health"))
Total_temp <- Mean_Total %>% 
  filter(Scenario_ID == "Expanded")

Mean_ClinicalOrNon <- Mean_ClinCat %>% 
  filter(Scenario_ID == "Expanded" & ClinicalOrNon != "Non-clinical") #taking "Non-clinical" out because the workload from "disease surveillance" is negligible)

Mean_ClinicalOrNon$ClinicalOrNon <- factor(Mean_ClinicalOrNon$ClinicalOrNon, ordered=TRUE, levels = c("Non-productive", "Clinical"))

plot2A <-  ggplot()+
  geom_bar(data=Mean_ClinicalOrNon,aes(x=Year,y=MeanHrs/WeeksPerYr,fill=ClinicalOrNon),stat="identity",alpha=.9)+
  geom_line(data=Total_temp,aes(x=Year,y=MeanHrs/WeeksPerYr),linewidth=1.2)+
  geom_point(data=Total_temp,aes(x=Year,y=MeanHrs/WeeksPerYr))+
  geom_errorbar(data=Total_temp,aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
  theme_bw()+
  scale_x_continuous(breaks =  c(2021,2025, 2030, 2035))+
  theme(legend.title=element_blank(),legend.position = c(0.02, 0.99), legend.justification = c(0.02, 0.99), 
        legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
  scale_fill_brewer(palette = "Set2", direction = -1)+
  labs(x="Year", y="Hours per Week per 5,000 Pop", fill = "Clinical or Non-clinical", title = "Panel A")
print(plot2A)

plot2B <-  ggplot()+
  geom_bar(data=Mean_ClinCat_labelled,aes(x=Year,y=MeanHrs/WeeksPerYr,fill=ClinicalLabel),stat="identity",alpha=.9)+
  geom_line(data=Total_temp,aes(x=Year,y=MeanHrs/WeeksPerYr),linewidth=1.2)+
  geom_point(data=Total_temp,aes(x=Year,y=MeanHrs/WeeksPerYr))+
  geom_errorbar(data=Total_temp,aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
  ylim(0, 50) +
  theme_bw()+
  scale_x_continuous(breaks =  c(2021,2025, 2030, 2035))+
  theme(legend.title=element_blank(),legend.position = c(0.02, 1), legend.justification = c(0.02, 1), 
        legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
  #theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
  scale_fill_viridis_d()+
  labs(x="Year", y="Hours per Week per 5,000 Pop", fill = "Clinical Category", title = "Panel B")
print(plot2B)

#Figure 2 Panel C Clinical workload by service category 2021-2035
unique(Mean_ServiceCat$ServiceCat[Mean_ServiceCat$ClinicalOrNon=="Clinical" & Mean_ServiceCat$Scenario_ID=="Expanded"])

ServiceCat_Clinical <- Mean_ServiceCat %>%
  subset(Scenario_ID == "Expanded" & ClinicalOrNon=="Clinical") %>%
  group_by(Scenario_ID, Year) %>%
  mutate(TotalHrs=sum(MeanHrs)) %>%
  arrange(Geography_dontedit, Scenario_ID, Year) %>%
  ungroup()

plot2C <- ggplot() +
  geom_bar(data=ServiceCat_Clinical,aes(x=Year,y=MeanHrs/WeeksPerYr,fill=ServiceCat),stat="identity",alpha=.9)+
  geom_line(data=subset(Stats_TotClin, Stats_TotClin$Scenario_ID=="Expanded"),aes(x=Year,y=CI50/WeeksPerYr),linewidth=1.2)+
  geom_point(data=subset(Stats_TotClin, Stats_TotClin$Scenario_ID=="Expanded"),aes(x=Year,y=CI50/WeeksPerYr))+
  geom_errorbar(data=subset(Stats_TotClin, Stats_TotClin$Scenario_ID=="Expanded"),aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
  ylim(0, 50) +
  theme_bw()+
  scale_x_continuous(breaks =  c(2021,2025, 2030, 2035))+
  theme(legend.title=element_blank(),legend.position = c(0.02, 1), legend.justification = c(0.02, 1), 
        legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
  #theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
  scale_fill_brewer(palette = "BrBG", direction = -1)+
  labs(x="Year", y="Hours per Week per 5,000 Pop", fill = "Clinical Category", title = "Panel C")
print(plot2C)  

ggpubr::ggarrange(plot2A,plot2B,plot2C, ncol=3,nrow=1,widths=c(1,1,1),align="h")

#Figure 3 Panel A Total workload by cadre 2021-2035
unique(Mean_Alloc$Cadre[Mean_Alloc$Scenario_ID=="Expanded" & Mean_Alloc$CI50!=0])

Cadre_labelled <- Mean_Alloc %>% 
  subset(Scenario_ID=="Expanded") %>% 
  filter(CI50!=0 ) %>% 
  mutate(CadreLabel = case_when(
    Cadre == "HEW_hrs" ~ 1,
    Cadre == "FH_hrs" ~ 2,
    Cadre == "UN_hrs" ~ 3)) %>% 
  filter(CadreLabel!=3) %>% 
  group_by(Scenario_ID, Year) %>% 
  mutate(sum_CI50 = sum(CI50), sum_CI05 = sum(CI05), sum_CI95 = sum(CI95))

Cadre_labelled$CadreLabel <- factor(Cadre_labelled$CadreLabel, levels = c(1, 2), labels = c("Health Extension Worker", "Family Health Professional"))


plot3A <-  ggplot(data=Cadre_labelled)+
  geom_bar(aes(x=Year,y=CI50/WeeksPerYr,fill=CadreLabel),stat="identity",alpha=.9)+
  geom_line(aes(x=Year,y=sum_CI50/WeeksPerYr),linewidth=1.2)+
  geom_point(aes(x=Year,y=sum_CI50/WeeksPerYr))+
  geom_errorbar(aes(x=Year,ymin=sum_CI05/WeeksPerYr, ymax=sum_CI95/WeeksPerYr), colour="black", width=.3)+
  theme_bw()+
  scale_x_continuous(breaks =  c(2021,2025, 2030, 2035))+
  theme(legend.title=element_blank(),legend.position = c(0.02, 0.99), legend.justification = c(0.02, 0.99), 
        legend.key.size=unit(0.3, 'cm'), legend.direction="vertical", legend.background = element_rect(fill = 'transparent'))+
  scale_fill_brewer(palette = "Paired", direction = -1)+
  labs(x="Year", y="Hours per Week per 5,000 Pop", fill = "Cadre", title = "Panel A")
print(plot3A)

#Figure 3 Panel B Total workload by month, ratio to average workload, 2035
Monthly_NonClinical <- Mean_ClinCat %>% 
  subset(ClinicalOrNon != "Clinical") %>% 
  group_by(Scenario_ID, Year) %>% 
  summarize(NonClinical_Monthly = sum(MeanHrs)/12, NonClinical_CI95 = sum(CI95)/12)

RatioToAvg_ByMonth <- ByRun_ClinMonth %>% 
  subset(Scenario_ID=="Expanded" & Year == 2035) %>% 
  merge(Monthly_NonClinical, by = c("Scenario_ID", "Year")) %>% 
  group_by(Trial_num, Year) %>% 
  mutate(MeanMonthHrs = mean(TotHrs+NonClinical_Monthly), RatioToMean = (TotHrs+NonClinical_Monthly)/(MeanMonthHrs)) %>% 
  ungroup() %>% 
  group_by(Month) %>% 
  summarize(RatioToMean_p05 = quantile(RatioToMean, 0.05),
            RatioToMean_p25 = quantile(RatioToMean, 0.25),
            RatioToMean_p50 = quantile(RatioToMean, 0.50),
            RatioToMean_p75 = quantile(RatioToMean, 0.75),
            RatioToMean_p95 = quantile(RatioToMean, 0.95))

plot3B <- ggplot(data=RatioToAvg_ByMonth, aes(x = Month, y=RatioToMean_p50))+
  theme_bw()+
  geom_smooth(method ="loess", fill = "transparent", span=0.5, alpha = 0.25)+
  #geom_line(linewidth=1)+
  geom_hline(yintercept = 1, color = "blue", linetype="dashed")+
  ylim(0.85, 1.15) +
  scale_x_continuous(breaks =  seq(1, 12))+
  labs(x = "Month", y="Ratio of workload for the month to annual average", title = "Panel B")
print(plot3B)
ggpubr::ggarrange(plot3A,plot3B, ncol=2,nrow=1,widths=c(1,1),align="h")


plot3B_alt <- ggplot(data=RatioToAvg_ByMonth)+
  theme_bw()+
  geom_ribbon(aes(x = Month, ymin = RatioToMean_p05, ymax = RatioToMean_p95), fill = "#80B1D3",  alpha = 0.25)+
  geom_line(aes(x = Month, y=RatioToMean_p50),linewidth=1)+
  scale_color_manual("#80B1D3")+
  ylim(0.9, 1.1) +
  scale_x_continuous(breaks =  seq(1, 12))+
  labs(x = "Month", y="Ratio of workload for the month to annual average")
print(plot3B_alt)





