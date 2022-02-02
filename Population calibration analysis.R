
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


UNpop <- read_excel("PopProjections_UN.xlsx",sheet="Sheet1")
UNpop <- melt(UNpop,variable.name = "Year",value.name = "PopEstimate")
UNpop$Year <- as.numeric(as.character(UNpop$Year))

scenario <- "PopCheck_NationalTotal"
append <- "_2_1_15_1_2" #fertility p, mortality p, delta fertility p, delta mortality p, delta q
calib1 <- read.csv(paste("results/results_",scenario,"_",append,".csv",sep=""))
#Calculate modeled 0-4 pop to match UN estimates.
#append_temp <- ddply(subset(calib1,Task_ID=="PopCheck_1t4" | Task_ID=="PopCheck_births"),.(Scenario_ID,Run_num,Trial_num,Year,Month),summarize,Num_services=sum(Num_services),Task_ID="PopCheck_0t4",Service_time=0,Health_benefit=NA)
#calib1 <- rbind(calib1,append_temp)
#Summarize calibration runs
simstats1 <- ddply(calib1,.(Task_ID,Run_num,Trial_num,Year),summarize,popvalue=sum(Num_services))
simCI1 <- ddply(simstats1,.(Task_ID,Year),summarize,CI20=quantile(popvalue,probs=c(.20)),CI80=quantile(popvalue,probs=c(.80)),CI50 = quantile(popvalue,probs=c(.50)),CI95 = quantile(popvalue,probs=c(.95)),CI05 = quantile(popvalue,probs=c(.05)))
simCImelt1 <- melt(simCI1,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopEstimate")

scenario <- "PopCheck_NationalTotal"
append <- "_2_1_15_1_125" #fertility p, mortality p, delta fertility p, delta mortality p, delta q
calib2 <- read.csv(paste("results/results_",scenario,"_",append,".csv",sep=""))
#Calculate modeled 0-4 pop to match UN estimates.
#append_temp <- ddply(subset(calib2,Task_ID=="PopCheck_1t4" | Task_ID=="PopCheck_births"),.(Scenario_ID,Run_num,Trial_num,Year,Month),summarize,Num_services=sum(Num_services),Task_ID="PopCheck_0t4",Service_time=0,Health_benefit=NA)
#calib2 <- rbind(calib2,append_temp)
#Summarize calibration runs
simstats2 <- ddply(calib2,.(Task_ID,Run_num,Trial_num,Year),summarize,popvalue=sum(Num_services))
simCI2 <- ddply(simstats2,.(Task_ID,Year),summarize,CI20=quantile(popvalue,probs=c(.20)),CI80=quantile(popvalue,probs=c(.80)),CI50 = quantile(popvalue,probs=c(.50)),CI95 = quantile(popvalue,probs=c(.95)),CI05 = quantile(popvalue,probs=c(.05)))
simCImelt2 <- melt(simCI2,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopEstimate")

scenario <- "PopCheck_NationalTotal"
append <- "_1_1_15_1_125" #fertility p, mortality p, delta fertility p, delta mortality p, delta q
calib3 <- read.csv(paste("results/results_",scenario,"_",append,".csv",sep=""))
#Calculate modeled 0-4 pop to match UN estimates.
#append_temp <- ddply(subset(calib2,Task_ID=="PopCheck_1t4" | Task_ID=="PopCheck_births"),.(Scenario_ID,Run_num,Trial_num,Year,Month),summarize,Num_services=sum(Num_services),Task_ID="PopCheck_0t4",Service_time=0,Health_benefit=NA)
#calib2 <- rbind(calib2,append_temp)
#Summarize calibration runs
simstats3 <- ddply(calib3,.(Task_ID,Run_num,Trial_num,Year),summarize,popvalue=sum(Num_services))
simCI3 <- ddply(simstats3,.(Task_ID,Year),summarize,CI20=quantile(popvalue,probs=c(.20)),CI80=quantile(popvalue,probs=c(.80)),CI50 = quantile(popvalue,probs=c(.50)),CI95 = quantile(popvalue,probs=c(.95)),CI05 = quantile(popvalue,probs=c(.05)))
simCImelt3 <- melt(simCI3,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopEstimate")

# scenario <- "PopCheck_NationalTotal"
# append <- "_2_1_15_1_2_2011" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertility calc year
# calib4 <- read.csv(paste("results/results_",scenario,"_",append,".csv",sep=""))
# simstats4 <- ddply(calib4,.(Task_ID,Run_num,Trial_num,Year),summarize,popvalue=sum(Num_services))
# simCI4 <- ddply(simstats4,.(Task_ID,Year),summarize,CI20=quantile(popvalue,probs=c(.20)),CI80=quantile(popvalue,probs=c(.80)),CI50 = quantile(popvalue,probs=c(.50)),CI95 = quantile(popvalue,probs=c(.95)),CI05 = quantile(popvalue,probs=c(.05)))
# simCImelt4 <- melt(simCI4,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopEstimate")

scenario <- "PopCheck_NationalTotal"
append <- "_2_1_15_1_2_2005" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertility calc year
calib5 <- read.csv(paste("results/results_",scenario,"_",append,".csv",sep=""))
simstats5 <- ddply(calib5,.(Task_ID,Run_num,Trial_num,Year),summarize,popvalue=sum(Num_services))
simCI5 <- ddply(simstats5,.(Task_ID,Year),summarize,CI20=quantile(popvalue,probs=c(.20)),CI80=quantile(popvalue,probs=c(.80)),CI50 = quantile(popvalue,probs=c(.50)),CI95 = quantile(popvalue,probs=c(.95)),CI05 = quantile(popvalue,probs=c(.05)))
simCImelt5 <- melt(simCI5,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopEstimate")

# scenario <- "PopCheck_NationalTotal"
# append <- "_2_1_15_1_2_2000" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertility calc year
# calib6 <- read.csv(paste("results/results_",scenario,"_",append,".csv",sep=""))
# simstats6 <- ddply(calib6,.(Task_ID,Run_num,Trial_num,Year),summarize,popvalue=sum(Num_services))
# simCI6 <- ddply(simstats6,.(Task_ID,Year),summarize,CI20=quantile(popvalue,probs=c(.20)),CI80=quantile(popvalue,probs=c(.80)),CI50 = quantile(popvalue,probs=c(.50)),CI95 = quantile(popvalue,probs=c(.95)),CI05 = quantile(popvalue,probs=c(.05)))
# simCImelt6 <- melt(simCI6,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopEstimate")

scenario <- "PopCheck_NationalTotal"
append <- "_15_1_15_1_125_2005" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertility calc year
calib7 <- read.csv(paste("results/results_",scenario,"_",append,".csv",sep=""))
simstats7 <- ddply(calib7,.(Task_ID,Run_num,Trial_num,Year),summarize,popvalue=sum(Num_services))
simCI7 <- ddply(simstats7,.(Task_ID,Year),summarize,CI20=quantile(popvalue,probs=c(.20)),CI80=quantile(popvalue,probs=c(.80)),CI50 = quantile(popvalue,probs=c(.50)),CI95 = quantile(popvalue,probs=c(.95)),CI05 = quantile(popvalue,probs=c(.05)))
simCImelt7 <- melt(simCI7,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopEstimate")

#Select a calibration run to plot
simCImelt <- simCImelt7

#Total Population
ggplot()+theme_bw()+
  geom_line(data=subset(simCImelt,Task_ID=="PopCheck_total" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
  geom_point(data=subset(UNpop,Type=="Total" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Total Population",subtitle="Lines=Simulated, Dots=UN Estimate")

#Women 15 to 49 Population
ggplot()+theme_bw()+
  geom_line(data=subset(simCImelt,Task_ID=="PopCheck_wom15t49" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
  geom_point(data=subset(UNpop,Type=="Women_15t49" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Women 15 to 49",subtitle="Lines=Simulated, Dots=UN Estimate")

#Annual births
ggplot()+theme_bw()+
  geom_line(data=subset(simCImelt,Task_ID=="PopCheck_births" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
  geom_point(data=subset(UNpop,Type=="Births_peryr" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Annual births",subtitle="Lines=Simulated, Dots=UN Estimate")


