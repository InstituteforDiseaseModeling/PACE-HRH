
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

date <- "Feb7"
scenario <- "PopTest_NtlTotal"


UNpop <- read_excel("PopProjections_UN.xlsx",sheet="Sheet1")
UNpop <- melt(UNpop,variable.name = "Year",value.name = "PopEstimate")
UNpop$Year <- as.numeric(as.character(UNpop$Year))

make_plots <- function(scenarioname,appendtext){

  calib <- read.csv(paste("results/results_",scenarioname,"_",date,"_",appendtext,".csv",sep=""))
  
  
  ######################
  ### Format data sets
  
  #Summarize calibration runs
  simstats <- ddply(calib,.(Task_ID,Run_num,Trial_num,Year),summarize,popvalue=sum(Num_services))
  simCI <- ddply(simstats,.(Task_ID,Year),summarize,CI20=quantile(popvalue,probs=c(.20)),CI80=quantile(popvalue,probs=c(.80)),CI50 = quantile(popvalue,probs=c(.50)),CI95 = quantile(popvalue,probs=c(.95)),CI05 = quantile(popvalue,probs=c(.05)))
  simCImelt <- melt(simCI,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopEstimate")
  
  #Calculate 0-4 range, adding up births + 1-4. This will slightly overestimate because births are not a mid-year estimate of kids age 0 alive.
  years1t4 <- subset(simCImelt,Task_ID=="PopCheck_1t4")
  births <- subset(simCImelt,Task_ID=="PopCheck_births")
  years1t4$Task_ID <- "PopCheck_0t4"
  births$Task_ID <- "PopCheck_0t4"
  temp0t4 <- left_join(years1t4,births,by=c("Year","StatType","Task_ID"))
  temp0t4$PopEstimate <- temp0t4$PopEstimate.x+temp0t4$PopEstimate.y
  temp0t4$PopEstimate.x <- NULL
  temp0t4$PopEstimate.y <- NULL
  temp0t4cast <- dcast(temp0t4,Task_ID+Year~StatType)

  #Attatch to simCI and simCImelt
  simCImelt <- rbind(simCImelt,temp0t4)
  simCI <- rbind(simCI,temp0t4cast)
    
  #Calculate differential to median value, simulations
  simDiff <- simCI
  simDiff$CI20m50 <- simDiff$CI20 - simDiff$CI50
  simDiff$CI80m50 <- simDiff$CI80 - simDiff$CI50
  simDiff$CI05m50 <- simDiff$CI05 - simDiff$CI50
  simDiff$CI95m50 <- simDiff$CI95 - simDiff$CI50
  
  simDiffmelt <- melt(simDiff,id.vars=c("Task_ID","Year"),variable.name = "StatType", value.name="PopDiff2median")
  simDiffmelt <- subset(simDiffmelt,StatType!= "CI20" & StatType!= "CI80" & StatType!= "CI50" & StatType!= "CI05" & StatType!= "CI95")
  simDiffmelt$StatType <- factor(simDiffmelt$StatType,labels=unique(simDiffmelt$StatType))
  
  #Calculate differential to median value, UN pop
  UNpop_cast <- dcast(UNpop,Type + Year ~ Stat)
  UNpop_cast$CI20m50 <- UNpop_cast$Lower20th - UNpop_cast$Median
  UNpop_cast$CI80m50 <- UNpop_cast$Upper80th - UNpop_cast$Median
  UNpop_cast$CI05m50 <- UNpop_cast$Lower5th - UNpop_cast$Median
  UNpop_cast$CI95m50 <- UNpop_cast$Upper95th - UNpop_cast$Median
  
  UNdiffmelt <- melt(UNpop_cast,id.vars=c("Type","Year"),variable.name="Stat",value.name="PopDiff2median")
  UNdiffmelt <- subset(UNdiffmelt,Stat!="Lower20th" & Stat!="Lower5th" & Stat!="Upper95th" & Stat!="Upper80th" & Stat!="Median")
  
  
  #########################
  ### Make Plots
  
  pdf(paste("results/popcalibration_",scenarioname,"_",date,"_",appendtext,".pdf",sep=""),width=8,height=5)
  
  ### RELATIVE TO MEDIAN
  
  #Annual births
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simDiffmelt,Task_ID=="PopCheck_births" & Year < 2036 & Year > 2020),aes(x=Year,y=PopDiff2median,color=StatType)) + 
    geom_point(data=subset(UNdiffmelt,Type=="Births_peryr" & Year < 2036),aes(x=Year,y=PopDiff2median*1000,shape=Stat))+labs(title="Annual births",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Children 0-4
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simDiffmelt,Task_ID=="PopCheck_0t4" & Year < 2036 & Year > 2020),aes(x=Year,y=PopDiff2median,color=StatType)) + 
    geom_point(data=subset(UNdiffmelt,Type=="Children_0t4" & Year < 2036),aes(x=Year,y=PopDiff2median*1000,shape=Stat))+labs(title="Children 0-4",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Children 5-9
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simDiffmelt,Task_ID=="PopCheck_5t9" & Year < 2036 & Year > 2020),aes(x=Year,y=PopDiff2median,color=StatType)) + 
    geom_point(data=subset(UNdiffmelt,Type=="Children_5t9" & Year < 2036),aes(x=Year,y=PopDiff2median*1000,shape=Stat))+labs(title="Children 5-9",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Children 10-14
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simDiffmelt,Task_ID=="PopCheck_10t14" & Year < 2036 & Year > 2020),aes(x=Year,y=PopDiff2median,color=StatType)) + 
    geom_point(data=subset(UNdiffmelt,Type=="Children_10t14" & Year < 2036),aes(x=Year,y=PopDiff2median*1000,shape=Stat))+labs(title="Children 10-14",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Population 15-19
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simDiffmelt,Task_ID=="PopCheck_15t19" & Year < 2036 & Year > 2020),aes(x=Year,y=PopDiff2median,color=StatType)) + 
    geom_point(data=subset(UNdiffmelt,Type=="Adults_15t19" & Year < 2036),aes(x=Year,y=PopDiff2median*1000,shape=Stat))+labs(title="Population 15-19",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Women 15 to 49 Population
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simDiffmelt,Task_ID=="PopCheck_wom15t49" & Year < 2036 & Year > 2020),aes(x=Year,y=PopDiff2median,color=StatType)) + 
    geom_point(data=subset(UNdiffmelt,Type=="Women_15t49" & Year < 2036),aes(x=Year,y=PopDiff2median*1000,shape=Stat))+labs(title="Women 15 to 49",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Total Population
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simDiffmelt,Task_ID=="PopCheck_total" & Year < 2036 & Year > 2020),aes(x=Year,y=PopDiff2median,color=StatType)) + 
    geom_point(data=subset(UNdiffmelt,Type=="Total" & Year < 2036),aes(x=Year,y=PopDiff2median*1000,shape=Stat))+labs(title="Total Population",subtitle="Lines=Simulated, Dots=UN Estimate"))

  ### ABSOLUTE VALUES
  
  #Annual births
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simCImelt,Task_ID=="PopCheck_births" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
    geom_point(data=subset(UNpop,Type=="Births_peryr" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Annual births",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Children 0-4
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simCImelt,Task_ID=="PopCheck_0t4" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
    geom_point(data=subset(UNpop,Type=="Children_0t4" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Children 0-4",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Children 5-9
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simCImelt,Task_ID=="PopCheck_5t9" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
    geom_point(data=subset(UNpop,Type=="Children_5t9" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Children 5-9",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Children 10-14
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simCImelt,Task_ID=="PopCheck_10t14" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
    geom_point(data=subset(UNpop,Type=="Children_10t14" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Children 10-14",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Population 15-19
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simCImelt,Task_ID=="PopCheck_15t19" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
    geom_point(data=subset(UNpop,Type=="Adults_15t19" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Population 15-19",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Women 15 to 49 Population
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simCImelt,Task_ID=="PopCheck_wom15t49" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
    geom_point(data=subset(UNpop,Type=="Women_15t49" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Women 15 to 49",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  #Total Population
  print(ggplot()+theme_bw()+
    geom_line(data=subset(simCImelt,Task_ID=="PopCheck_total" & Year < 2036 & Year > 2020),aes(x=Year,y=PopEstimate,color=StatType)) + 
    geom_point(data=subset(UNpop,Type=="Total" & Year < 2036),aes(x=Year,y=PopEstimate*1000,shape=Stat))+labs(title="Total Population",subtitle="Lines=Simulated, Dots=UN Estimate"))
  
  dev.off()
}


append <- "_15_1_15_1_125_2005" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertilty year
make_plots(scenario,append)

append <- "_12_12_15_15_125_2005" #fertility p, mortality p, delta fertility p, delta mortality p, delta q, baseline fertilty year
make_plots(scenario,append)

append <- "_12_12_12_20_125_15_2005"
make_plots(scenario,append)

append <- "_12_12_15_20_15_15_2005"
make_plots(scenario,append)

append <- "_12_12_135_20_135_15_2005"
make_plots(scenario,append)

append <- "_12_12_135_20_135_15_2005 MortEstimated2020"
make_plots(scenario,append)

append <- "_12_12_135_20_135_15_2005 MortReported2019"
make_plots(scenario,append)

append <- "_12_12_135_22_135_16_2005 MortReported2019"
make_plots(scenario,append)





