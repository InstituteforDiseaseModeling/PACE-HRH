
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

### How and when to run this ###
# Please set baseline population to 100,000 or above in the scenario manager for by-age workload simulations and analyses
# Use this script for by-age workload simulations and analyses only#
# Trial-by-trial results from pacehrh::RunExperiments are not saved and will be not accessible after terminating the R session#
# Summary statistics aggregated from all trials are saved as csv files at the end of the session#

rm(list = ls())

configfilepath <- "config/model_inputs.xlsx"

pacehrh::Trace(TRUE)
pacehrh::SetInputExcelFile(configfilepath)
pacehrh::InitializePopulation()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()
pacehrh::SetRoundingLaw("Late")
pacehrh::SetPerAgeStats(TRUE)

scenarios <- read_xlsx(configfilepath,sheet="Scenarios")

numtrials <- 50
date <- Sys.Date()
usefuldescription <- scenarios$Geography_dontedit[1]

birthtasks <- c("FH.MN.ANC.1", "FH.MN.ANC.2", "FH.MN.D.3", "FH.MN.D.4", "FH.MN.D.5", 
                "FH.MN.PNC.7", "FH.MN.PNC.8", "FH.MN.PNC.9", "FH.MN.PNC.10", "FH.MN.PNC.11",
                "FH.FP.38B", "FH.FP.43", "FH.FP.44", "FH.Ntr.68")
OHtasks <- c("MHH_HEH", "MHH_HEP", "Record keeping", "Travel_HEH", "Travel_HEP",
                  "Overhead_staff1", "Overhead_staff2", "Overhead_staff3", "Overhead_staff4", "Overhead_staff5", "Overhead_staff6")

# Run through the full scenario list.
for (i in 1:nrow(scenarios)){
  print(paste("Starting scenario",i))
  scenario <- scenarios$UniqueID[i]
  geoname <- scenarios$Geography_dontedit[i]
  results <- pacehrh::RunExperiments(scenarioName = scenario, trials = numtrials, debug = FALSE)

  # By-age results cleaning
  resultspop <- SaveSuiteDemographics(results) %>% 
    select(-AgeBucket) %>% 
    rename(Pop_F = Female, Pop_M = Male)
  # By Trial, Year, and Age: aggregate total time (in minutes) across all tasks excluding overhead tasks
  df_f <- lapply(1:numtrials, function(x) {
    pivot_longer(
      setDT(
        as.data.frame(
          colSums(results[[x]][["AnnualPerAge"]][["Female"]][setdiff(rownames(results[[x]][["AnnualPerAge"]][["Female"]]),OHtasks),,], dims=1)
        ),
      keep.rownames = "Age"),
    col=!c("Age"), names_to = "Year", values_to = "TotMins_F")
    })
  workload_f <- rbindlist(df_f, use.names = TRUE, idcol = TRUE) %>% 
    rename(Trial = .id)
  
  df_m <- lapply(1:numtrials, function(x) {
    pivot_longer(
      setDT(
        as.data.frame(
          colSums(results[[x]][["AnnualPerAge"]][["Male"]][setdiff(rownames(results[[x]][["AnnualPerAge"]][["Male"]]),OHtasks),,], dims=1)
        ),
        keep.rownames = "Age"),
      col=!c("Age"), names_to = "Year", values_to = "TotMins_M")
  })
  workload_m <- rbindlist(df_m, use.names = TRUE, idcol = TRUE) %>% 
    rename(Trial = .id)
  
  workload_all <- inner_join(workload_f, workload_m, by = c("Trial", "Year", "Age")) 
  
  # Keep track of workload on birthtasks separately
  birthminutes_f <- lapply(1:numtrials, function(x) {
    setDT(
      data.frame(BirthMins_F = colSums(results[[x]][["AnnualPerAge"]][["Female"]][birthtasks,1,])),
      keep.rownames = "Year"
      )
  })
  workload_births_f <- rbindlist(birthminutes_f, use.names = TRUE, idcol = TRUE) %>% 
    rename(Trial = .id)
  
  totalrecordkeeping_f <- lapply(1:numtrials, function(x){
    a <- results[[x]][["MonthlyPerAge"]][["Times"]][["Female"]]["Record keeping",,]
    sum(a)
    })
  totalrecordkeeping_m <- lapply(1:numtrials, function(x){
    a <- results[[x]][["MonthlyPerAge"]][["Times"]][["Male"]]["Record keeping",,]
    sum(a)
  })
  
  birthminutes_m <- lapply(1:numtrials, function(x) {
    setDT(
      data.frame(BirthMins_M = colSums(results[[x]][["AnnualPerAge"]][["Male"]][birthtasks,1,])),
      keep.rownames = "Year"
    )
  })
  workload_births_m <- rbindlist(birthminutes_m, use.names = TRUE, idcol = TRUE) %>% 
    rename(Trial = .id) 

  DS_byAge <- resultspop %>% 
    mutate(Year = as.character(Year), Age = as.character(Age)) %>% 
    inner_join(workload_all, by = c("Trial", "Year", "Age")) %>% 
    inner_join(workload_births_f, by = c("Trial", "Year")) %>% 
    inner_join(workload_births_m, by = c("Trial", "Year")) %>% 
    mutate(Region = geoname, Scenario_ID = scenario)
  
  write.csv(DS_byAge,paste("results/ByAgeWorkloadv3_",geoname,"_",scenario,"_",date,".csv",sep=""), row.names=FALSE)
  }


################################################################################
# aggregate across regions
################################################################################
regions <- c("Ethiopia","Tigray", "Afar", "Amhara", "Oromia", "Somali", "Benishangul Gumuz", "SNNPR", "Gambela", "Harari", "Addis Ababa", "Dire Dawa")
excludethese <- c("Afar", "Tigray", "Ethiopia")
Geogroup1 <- c("Ethiopia", "Addis Ababa", "Amhara", "Oromia", "SNNPR")
Geogroup2 <- c("Dire Dawa", "Addis Ababa", "Amhara", "Oromia", "SNNPR", "Benishangul Gumuz")
Geogroup3 <- c("Oromia", "Amhara", "SNNPR")

remove(ByAgeWorkload_all)
sc <- "ComprehensiveModel"
for (GeoName in Geogroup3){
  print(paste("Loading region",GeoName))
  ByAgeWorkload_temp <- read.csv(paste("results/ByAgeWorkloadv3_",GeoName,"_",sc,"_",date,".csv",sep=""))
  if(!exists('ByAgeWorkload_all')){
    ByAgeWorkload_all <- ByAgeWorkload_temp
  }else{
    ByAgeWorkload_all <- rbind(ByAgeWorkload_all,ByAgeWorkload_temp)
  }
  remove(ByAgeWorkload_temp)
}

destination = paste("results/ByAgeWorkloadv2", sc, date, ".pdf", sep="_")
pdf(file=destination)

DS <- ByAgeWorkload_all %>% 
  mutate(MinsPerCap_F = TotMins_F/Pop_F, MinsPerCap_M = TotMins_M/Pop_M)
DS$MinsPerCap_F[DS$Pop_F==0]=0
DS$MinsPerCap_M[DS$Pop_M==0]=0

DS_Mean <- DS %>% 
  group_by(Region, Scenario_ID, Year, Age) %>% 
  summarise(TotMins_F=mean(TotMins_F), TotMins_M=mean(TotMins_M), 
            Pop_F=mean(Pop_F), Pop_M=mean(Pop_M), 
            MinsPerCap_F=mean(MinsPerCap_F), MinsPerCap_M=mean(MinsPerCap_M))

DS_regions <- DS_Mean %>% 
  filter(Year %in% c(2021, 2025, 2030, 2035)) %>% 
  #filter(Region %in% Geogroup2) %>% 
  group_by(Region, Scenario_ID, Year) %>% 
  mutate(pct_F=TotMins_F/sum(TotMins_F), pct_M=TotMins_M/sum(TotMins_M))  
 # subset(Age!=0)

plot_TotHrs <- ggplot(data = DS_regions, aes(x=Age, y=TotMins_F)) + 
  theme_bw()+
  geom_area()

plot_byage <- ggplot(data=DS_regions)+
  theme_bw()+
  geom_bar(aes(x=Age, y=pct_F*100),stat="identity",fill='lightblue', alpha=0.8)+
  geom_bar(aes(x=Age, y=pct_M*100),stat="identity",fill="lightpink", alpha=0.8)+
  facet_grid(DS_regions$Region~DS_regions$Year)+
  labs(title=paste(sc,"workload by age"), x="Age", y="% of total workload")
print(plot_byage)

plot_percap <- ggplot(data=DS_regions)+
  theme_bw()+
  geom_bar(aes(x=Age, y=MinsPerCap_F),stat="identity",fill='lightblue', alpha=0.8)+
  geom_bar(aes(x=Age, y=MinsPerCap_M),stat="identity",fill="lightpink", alpha=0.8)+
  facet_grid(DS_regions$Region~DS_regions$Year)+
  labs(title=paste(sc,"per capita workload by age"), x="Age", y="per capita workload in minutes")
print(plot_percap)
dev.off()

#Define age groups and reallocate age0 workload to reproductive-age women
DS_grouped <- ByAgeWorkload_all %>% 
  mutate(Pop_group = case_when(
    Age == 0 ~ 1,
    Age >= 1 & Age <5 ~ 2,
    Age >= 5 & Age < 15 ~ 3,
    Age >= 15 & Age < 25 ~ 4,
    Age >= 25 & Age < 35 ~ 5,
    Age >= 35 & Age < 45 ~ 6,
    Age >= 45 & Age < 55 ~ 7,
    Age >= 55 & Age < 65 ~ 8,
    Age >= 65  ~ 9)) %>% 
  group_by(Trial, Region, Scenario_ID, Year, Pop_group) %>% 
  summarise(Pop_F=sum(Pop_F), Pop_M=sum(Pop_M), 
            TotMins_F=sum(TotMins_F), TotMins_M=sum(TotMins_M), 
            BirthMins_F=mean(BirthMins_F), BirthMins_M=mean(BirthMins_M)) %>% 
  ungroup() %>% 
  mutate(MinsPerCap_F = TotMins_F/Pop_F, MinsPerCap_M = TotMins_M/Pop_M)

DS_grouped_regions <- DS_grouped %>% 
  filter(Year %in% c(2021, 2035)) %>% 
  filter(Region %in% Geogroup3) %>% 
  group_by(Trial, Region, Scenario_ID, Year) %>% 
  mutate(pct_F=TotMins_F/(sum(TotMins_F)+sum(TotMins_M)), pct_M=TotMins_M/(sum(TotMins_F)+sum(TotMins_M))) %>% 
  ungroup() %>% 
  group_by(Region, Scenario_ID, Year, Pop_group) %>% 
  summarise(TotMins_F=mean(TotMins_F), TotMins_M=mean(TotMins_M),
            Pop_F=mean(Pop_F), Pop_M=mean(Pop_M),
            pct_F=mean(pct_F), pct_M=mean(pct_M),
            BirthMins_F=mean(BirthMins_F), BirthMins_M=mean(BirthMins_M),
            MinsPerCap_F=mean(MinsPerCap_F), MinsPerCap_M=mean(MinsPerCap_M)) 
  #filter(Pop_group!=1)

DS_pct <- DS_grouped_regions %>% 
  select(Scenario_ID, Region, Year, Pop_group, pct_F, pct_M) %>% 
  pivot_longer(cols = starts_with("pct"), names_to = "Y_series", values_to = "pct_workload") %>% 
  mutate(valstxt = case_when(pct_workload*100>10 ~ round(pct_workload*100, 0), TRUE ~ pct_workload*100)) 
ymax_pct = max(DS_pct$pct_workload)*100*1.1
DS_pct$Pop_group <- factor(DS_pct$Pop_group, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                       labels = c("Infants", "Under 5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-100"))

DS_percap <- DS_grouped_regions %>% 
  select(Scenario_ID, Region, Year, Pop_group, MinsPerCap_F, MinsPerCap_M) %>% 
  pivot_longer(cols = starts_with("MinsPerCap"), names_to = "Y_series", values_to = "MinsPerCap") %>% 
  filter(Pop_group!=1)
ymax_percap = max(DS_percap$MinsPerCap)*1.2
DS_percap$Pop_group <- factor(DS_percap$Pop_group, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                           labels = c("Infants", "Under 5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-100"))

destination = paste("results/ByAgeGroupWorkloadv3", sc, date, ".pdf", sep="_")
pdf(file=destination)
# sub-national manuscript Figure 6 Panel A
plot_byagegrp <- ggplot(data=DS_pct, aes(x=Pop_group, y=pct_workload*100, fill=Y_series))+
  theme_bw()+
  geom_bar(width=0.75, position=position_dodge(width = 0.9),stat="identity")+
  ylim(0,ymax_pct)+
  scale_fill_brewer(palette = "PuBu", labels=c('Female', 'Male'))+
  geom_text(aes(label = round(valstxt,1)), position=position_dodge(width=0.9), vjust=-0.5, size=3, color="black", fontface='bold')+
  facet_grid(Region~Year, switch='y')+
  guides(x=guide_axis(n.dodge=2))+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.title = element_blank(), legend.position = "bottom") +
  labs(title=paste("Panel A: predicted proportion of total clinical workload by age groups"), x="", y="% of total clinical workload")
print(plot_byagegrp)

# sub-national manuscript Figure 6 Panel B
plot_agegrppercap <- ggplot(data=DS_percap, aes(x=Pop_group, y=MinsPerCap, fill=Y_series))+
  theme_bw()+
  geom_bar(width=0.75,position=position_dodge(width = 0.9),stat="identity")+
  ylim(0,ymax_percap)+
  scale_fill_brewer(palette = "PuBu", labels=c('Female', 'Male'))+
  geom_text(aes(label = round(MinsPerCap,0)), position=position_dodge(width=1), vjust=-0.5, size=3, color="black", fontface='bold')+
  facet_grid(Region~Year, switch='y')+
  guides(x=guide_axis(n.dodge=2))+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.title = element_blank(), legend.position = "bottom") +
  labs(title=paste("Panel B: predicted annual clinical workload per capita by age groups"), x="Age groups", y="Average annual clinical workload per capita in minutes")
print(plot_agegrppercap)

ggpubr::ggarrange(plot_byagegrp,plot_agegrppercap,ncol=1,nrow=2,heights=c(1,1),align="v", common.legend = TRUE, legend = "bottom")
dev.off()

#per capital workload by age-group
Pop_female_2020 <- read_xlsx("config/model_inputs.xlsx",sheet="Pop_female_2020") %>% 
  select(Age, pop_spline, adminCode, adminNameAsUsed) %>% 
  mutate(Gender = "F")

Pop_male_2020 <- read_xlsx("config/model_inputs.xlsx",sheet="Pop_male_2020") %>% 
  select(Age, pop_spline, adminCode, adminNameAsUsed) %>% 
  mutate(Gender = "M")

Pop_2020 <- Pop_female_2020 %>% 
  rbind(Pop_male_2020) %>% 
  group_by(adminCode) %>% 
  mutate(Pop_Total=sum(pop_spline), Pop_pct = pop_spline/Pop_Total) %>% 
  mutate(Pop_group = case_when(
    Age == "<1" ~ 1,
    as.numeric(Age) >= 1 & as.numeric(Age) <=5 ~ 2,
    as.numeric(Age) > 5 & as.numeric(Age) <= 15 ~ 3,
    as.numeric(Age) > 15 & as.numeric(Age) <= 49 ~ 4,
    as.numeric(Age) > 49 & as.numeric(Age) <= 65 ~ 5,
    as.numeric(Age) > 65 ~ 6 
  ))
Pop_2020$Pop_group <- factor(Pop_2020$Pop_group, levels = c(1, 2, 3, 4, 5, 6), labels = c("Newborn", "Under 5", "School-age", "Reproductive-age", "Middle-age", "Elderly"))

Pop_2020_grouped <- Pop_2020 %>% 
  group_by(adminNameAsUsed, Pop_group) %>% 
  summarise(Pop_pct = sum(Pop_pct)) %>% 
  mutate(Popsize=Pop_pct*5000)

test <- DS_grouped %>% 
  inner_join(Pop_2020_grouped, by = c("Region"="adminNameAsUsed", "Pop_group")) %>% 
  mutate(percapitaMinutes = 60*(TotHrs/Popsize)) 
  filter(Pop_group!="Newborn")

plot_test <- ggplot(data=test)+
  theme_bw()+
  geom_bar(aes(x=Pop_group, y=percapitaMinutes, fill=Pop_group),stat="identity")+
  facet_wrap(~test$Region, nrow=1)+
  #facet_grid(DS_grouped$Region)+
  scale_fill_brewer(palette = "Set3")+
  labs(title=paste(sc,"workload by age-group"), x="Age", y="Estimated per capita workload for an average person in each age-group")
print(plot_test)

#Population pyramids by region [Figure in manuscript supplement]
Pop_female_2020 <- read_xlsx("config/model_inputs.xlsx",sheet="Pop_female_2020") %>% 
  select(Age, pop_spline, adminCode, adminNameAsUsed) %>% 
  mutate(Gender = "F")

Pop_male_2020 <- read_xlsx("config/model_inputs.xlsx",sheet="Pop_male_2020") %>% 
  select(Age, pop_spline, adminCode, adminNameAsUsed) %>% 
  mutate(Gender = "M")

Pop_2020 <- Pop_female_2020 %>% 
  rbind(Pop_male_2020) %>% 
  group_by(adminCode) %>% 
  mutate(Pop_Total=sum(pop_spline), Pop_pct = pop_spline/Pop_Total) %>% 
  mutate(Pop_group = case_when(
    Age == "<1" ~ 1,
    as.numeric(Age) >= 1 & as.numeric(Age) <=5 ~ 2,
    as.numeric(Age) > 5 & as.numeric(Age) <= 15 ~ 3,
    as.numeric(Age) > 15 & as.numeric(Age) <= 35 ~ 4,
    as.numeric(Age) > 35 & as.numeric(Age) <= 65 ~ 5,
    as.numeric(Age) > 65 ~ 6 
  ))

Pop_2020$Pop_group <- factor(Pop_2020$Pop_group, levels = c(1, 2, 3, 4, 5, 6), labels = c("Newborn", "Under 5", "School-age", "Young-adult", "Middle-age", "Elderly"))

Pop_group_2020 <- Pop_2020 %>% 
  group_by(adminNameAsUsed, Pop_group, Gender) %>% 
  summarise(Pop_pct = sum(Pop_pct))

PopulationPlot2 <-  ggplot()+
  theme_bw()+
  geom_bar(data=subset(Pop_group_2020, Gender=="M"), stat="identity", aes(x=Pop_group, y=round(Pop_pct,2), fill=Gender, alpha=0.5))+
  geom_bar(data=subset(Pop_group_2020, Gender=="F"), stat="identity", aes(x=Pop_group, y=round(Pop_pct,2), fill=Gender, alpha=0.5))+
  geom_text(data=subset(Pop_group_2020, Gender=="F"), aes(x=Pop_group, y=round(Pop_pct,2), label = round(Pop_pct,2)), size=3, vjust=-0.8, color = "#66C2A5")+
  scale_fill_brewer(palette = "Set2")+
  guides(x=guide_axis(n.dodge=2))+
  facet_wrap(vars(adminNameAsUsed))+
  labs(x = NULL, y = "% of total population")
print(PopulationPlot2)
