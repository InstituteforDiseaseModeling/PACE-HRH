
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

dirs = "C:/Users/brittanyha/OneDrive - Bill & Melinda Gates Foundation/Projects/HEP capacity projection/Model"
setwd(dirs)

#import demonstration data
DR <- read.csv("Random values for developing analytics.csv")
YearlyValues <- data.frame(read_xlsx("Output CSV file design.xlsx",sheet="YearlyValues"))

#import permanent data
taskvalues <- read_xlsx("R Model Inputs.xlsx",sheet="TaskValues")
taskvalues <- subset(taskvalues,Geography=="National")
taskvalues$StartingRateInPop[is.na(taskvalues$StartingRateInPop)] = 0
taskvalues$NumContactsAnnual[is.na(taskvalues$NumContactsAnnual)] = 0
taskvalues$NumContactsPerUnit[is.na(taskvalues$NumContactsPerUnit)] = 0
taskvalues$MinsPerContact[is.na(taskvalues$MinsPerContact)] = 0
taskvalues$HoursPerWeek[is.na(taskvalues$HoursPerWeek)] = 0
taskvalues$FTEratio[is.na(taskvalues$FTEratio)] = 0

scenarios <- read_xlsx("R Model Inputs.xlsx",sheet="Scenarios")

#cleaning
DR$Year = DR$Year+2020
BaselineScenario <- "ScenarioA" #ExpectedValue_RuralRatio
StableFertility <- "ScenarioB" #NoFertilityChange_RuralRatio

#Merge together data sets
scenarios$Scenario_ID <- scenarios$UniqueID
DR <- left_join(DR,scenarios,by=c("Scenario_ID"))
taskvalues$Task_ID <- taskvalues$Indicator
DR$CommonName <- taskvalues$CommonName[match(DR$Task_ID,taskvalues$Task_ID)]
DR$ClinicalOrNon <- taskvalues$ClinicalOrNon[match(DR$Task_ID,taskvalues$Task_ID)]
DR$ClinicalCat <- taskvalues$ClinicalCat[match(DR$Task_ID,taskvalues$Task_ID)]
DR$ServiceCat <- taskvalues$ServiceCat[match(DR$Task_ID,taskvalues$Task_ID)]
DR$NumContactsPer <- taskvalues$NumContactsPerUnit[match(DR$Task_ID,taskvalues$Task_ID)] + taskvalues$NumContactsAnnual[match(DR$Task_ID,taskvalues$Task_ID)]

DR <- subset(DR,PopType=="National")
DR$ServiceCat[DR$ClinicalOrNon!="Clinical"]=DR$CommonName[DR$ClinicalOrNon!="Clinical"]

# Will want to do these same analyses (all of below) for rural only vs. national to see how different they are, but probably rural-only in the GR slide deck.

############################################################################################################################################
############################################################################################################################################

#Create summary tables
ByRun_ServiceCat <- ddply(DR,.(Scenario_ID,Trial_num,Run_num,Year,ServiceCat,ClinicalOrNon),summarize,TotHrs=sum(Service_time))
Mean_ServiceCat <- ddply(ByRun_ServiceCat,.(Scenario_ID,Year,ServiceCat,ClinicalOrNon),summarize,MeanHrs=mean(TotHrs),CI95 = quantile(TotHrs,probs=c(.95)))

ByRun_ClinCat <- ddply(DR, .(Scenario_ID,Trial_num,Run_num,Year,ClinicalCat,ClinicalOrNon),summarize,TotHrs = sum(Service_time))
Mean_ClinCat <- ddply(ByRun_ClinCat, .(Scenario_ID,Year,ClinicalCat,ClinicalOrNon),summarize,MeanHrs = mean(TotHrs))

ByRun_Total <- ddply(DR, .(Scenario_ID,Trial_num,Run_num,Year),summarize,TotHrs = sum(Service_time))
Mean_Total <- ddply(ByRun_Total, .(Scenario_ID,Year),summarize,MeanHrs = mean(TotHrs), CI95 = quantile(TotHrs,probs=c(.95)))

ByRun_TotClin <- ddply(subset(DR,ClinicalOrNon=="Clinical"),.(Scenario_ID,Trial_num,Run_num,Year,WeeksPerYr,HrsPerWeek),summarize,AnnualHrs=sum(Service_time))
Stats_TotClin <- ddply(ByRun_TotClin,.(Scenario_ID,Year,WeeksPerYr,HrsPerWeek),summarize,CI05 = quantile(AnnualHrs,probs=c(.05)),CI50 = mean(AnnualHrs),CI95 = quantile(AnnualHrs,probs=c(.95)))

ByRun_ClinMonth <- ddply(subset(DR,ClinicalOrNon=="Clinical"),.(Scenario_ID,Trial_num,Run_num,Year,Month,WeeksPerYr,HrsPerWeek),summarize,TotHrs=sum(Service_time))
Stats_ClinMonth <- ddply(ByRun_ClinMonth,.(Scenario_ID,Year,Month,WeeksPerYr,HrsPerWeek),summarize,CI05 = quantile(TotHrs,probs=c(.05)),CI50 = mean(TotHrs),CI95 = quantile(TotHrs,probs=c(.95)))

############################################################################################################################################
############################################################################################################################################

#service numbers for new context slide

#calculations of how many CHW are needed, clinical and total

#calculation hours per week need to work at current staffing ratios (up to 2/10k)


############################################################################################################################################

#graphic for slide 5 (hours per week on clinical, development, and total work)
#dashed line is actual hours worked per week, according to time and motion study
#solid line is the 95th percentile, simulated
#bars are the simulated expected value for time required, best case with perfect scheduling

temp <- subset(Mean_ClinCat,Scenario_ID==BaselineScenario) ## FIX THIS TO THE CORRECT BASELINE SCENARIO
weeksperyear = mean(DR$WeeksPerYr[DR$Scenario_ID==BaselineScenario])
hoursperweek = mean(DR$HrsPerWeek[DR$Scenario_ID==BaselineScenario])
temp$Category <- paste(temp$ClinicalCat)
temp$Category[temp$ClinicalOrNon=="Clinical"] = paste("Clinical -",temp$Category[temp$ClinicalOrNon=="Clinical"])
temp$Category[temp$ClinicalOrNon!="Clinical"] = temp$ClinicalOrNon[temp$ClinicalOrNon!="Clinical"]
temp$Category <- factor(temp$Category,ordered=TRUE,levels=unique(temp$Category))
temp$Alpha <- 1
temp$Alpha[temp$ClinicalOrNon!="Clinical"] = .3

ggplot()+
  geom_bar(data=temp,aes(x=Year,y=MeanHrs,fill=Category),stat="identity",alpha=temp$Alpha)+
  geom_line(data=subset(Mean_Total,Scenario_ID==BaselineScenario),aes(x=Year,y=CI95))+
  theme_bw()+scale_x_continuous(breaks = seq(2021,2035))+theme(legend.title=element_blank())+scale_fill_viridis_d()+ylab("Annual Hours")

## VALIDATE THAT A DIVISION BY 2 is CORRECT
ggplot()+
  geom_bar(data=temp,aes(x=Year,y=MeanHrs/weeksperyear/2,fill=Category),stat="identity",alpha=temp$Alpha)+
  geom_hline(linetype="dashed",color="darkgrey",yintercept=hoursperweek)+
  geom_line(data=subset(Mean_Total,Scenario_ID=="ScenarioA"),aes(x=Year,y=CI95/weeksperyear/2))+
  theme_bw()+scale_x_continuous(breaks = seq(2021,2035))+
  theme(legend.title=element_blank())+scale_fill_viridis_d()+ylab("Hours per Week per Staff")

############################################################################################################################################

#pie chart for slide 6 (mix of services in a given year

temp <- subset(Mean_ServiceCat,Year==2021 & Scenario_ID==BaselineScenario)
temp$ServiceCat[temp$ClinicalOrNon!="Clinical"]=paste("*",temp$ServiceCat[temp$ClinicalOrNon!="Clinical"])

#calculate data label positions
temp <- temp %>%
  arrange(desc(ServiceCat)) %>%
  mutate(prop=MeanHrs/sum(temp$MeanHrs)*100) %>%
  mutate(ypos=cumsum(MeanHrs)-0.5*MeanHrs)

ggplot(temp,aes(x="",y=MeanHrs,fill=ServiceCat))+geom_bar(stat="identity",width=1)+
  theme_void()+coord_polar("y",start=0)+scale_fill_viridis_d()+
  labs(xlab="",ylab="")+geom_label_repel(aes(label=ServiceCat,y=ypos),nudge_x=.3)

ggplot(temp,aes(x="",y=MeanHrs,fill=ServiceCat))+geom_bar(stat="identity",width=1)+
  theme_void()+coord_polar("y",start=0)+scale_fill_viridis_d()+
  labs(xlab="",ylab="")+geom_text(aes(y=ypos,label=ServiceCat),color="white",size=4)

temp <- subset(Mean_ServiceCat,ClinicalOrNon=="Clinical" & Scenario_ID==BaselineScenario)
tempyr1 <- subset(temp,Year==2021)
temp$RatioTo1 <- temp$MeanHrs/tempyr1$MeanHrs[match(temp$ServiceCat,tempyr1$ServiceCat)]
temp$ServiceCat <- as.factor(temp$ServiceCat)
#temp$RatioTo1 <- temp$RatioTo1*runif(nrow(temp)) #just for temporary plotting purposes
LastYear<-subset(temp,Year==max(temp$Year))
temp$RatioLastYr <- LastYear$RatioTo1[match(temp$ServiceCat,LastYear$ServiceCat)]
temp$Label <- ""
temp$Label[temp$Year==max(temp$Year)] = paste(temp$ServiceCat[temp$Year==max(temp$Year)],", ",round(temp$RatioLastYr[temp$Year==max(temp$Year)],2),sep="")

ggplot(temp,aes(x=Year,y=RatioTo1,group=ServiceCat))+geom_line(aes(color=ServiceCat),size=1.1)+geom_hline(yintercept = 1,color="darkgrey",linetype="dashed")+
  theme_bw()+xlab("")+ylab("Ratio to Baseline Year")+theme(legend.position="none")+scale_color_discrete()+
  geom_text(aes(x=max(Year)+.4,y=RatioLastYr,label=Label),color="darkgrey",size=4,hjust=0)+scale_x_continuous(breaks = seq(2021,2035),limits=c(2021,max(temp$Year)+2.2))

############################################################################################################################################

#fertility sensitivity analysis

Stats_TotClin$FTE05 <- round(Stats_TotClin$CI05/Stats_TotClin$WeeksPerYr/Stats_TotClin$HrsPerWeek,2)
Stats_TotClin$FTE50 <- round(Stats_TotClin$CI50/Stats_TotClin$WeeksPerYr/Stats_TotClin$HrsPerWeek,2)
Stats_TotClin$FTE95 <- round(Stats_TotClin$CI95/Stats_TotClin$WeeksPerYr/Stats_TotClin$HrsPerWeek,2)

#clinical FTE calcs
#First/Last Year Total clinical FTE, Baseline/Stable fertility
subset(Stats_TotClin,(Year==min(Stats_TotClin$Year) | Year==max(Stats_TotClin$Year)) & Scenario_ID==BaselineScenario)
subset(Stats_TotClin,(Year==min(Stats_TotClin$Year) | Year==max(Stats_TotClin$Year)) & Scenario_ID==StableFertility)

#Annual hours of clinical work, fertility sensitivity plot
temp <- subset(Mean_ServiceCat,ClinicalOrNon=="Clinical")
#temp$MeanHrs[temp$Scenario_ID==StableFertility] = temp$MeanHrs[temp$Scenario_ID==StableFertility]*1.2 #just for demo purposes to get a difference between scenarios

#Box outline is stable fertility 95th percentile
#Light green is stable fertility scenario
#Dark green is continuing decline in fertility scenario (expected/baseline)
ggplot()+
  geom_bar(data=subset(temp,Scenario_ID==StableFertility),aes(x=ServiceCat,y=CI95,group=Year),stat="identity",position="dodge",fill="white",color="darkgrey")+
  geom_bar(data=temp,aes(x=ServiceCat,y=MeanHrs,group=Year),stat="identity",position="dodge",alpha=.65,fill="darkslategray")+
  theme_bw()+xlab("")+ylab("Mean Annual Hours (simulated)")+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5))+
  scale_y_continuous(labels=comma)

############################################################################################################################################

#Seasonality impact analysis

temp <- subset(Stats_ClinMonth,Scenario_ID==BaselineScenario)
temp <- subset(temp,Year==min(temp$Year) | Year==max(temp$Year))

ggplot(temp,aes(x=Month,y=CI50,group=as.factor(Year)))+theme_bw()+ylab("Mean Clinical Hours (simulated)")+
  geom_ribbon(aes(ymin=CI05,ymax=CI95,fill=as.factor(Year)),alpha=.3)+geom_line(aes(color=as.factor(Year)))+
  scale_fill_viridis_d()+scale_color_viridis_d()+
  scale_x_continuous(breaks=seq(1,12))+theme(legend.title=element_blank())+
  scale_y_continuous(labels=comma)

#calculate FTE need differences
temp$FTE50 <- round(temp$CI05/temp$WeeksPerYr/temp$HrsPerWeek,2)
temp$FTE95 <- round(temp$CI95/temp$WeeksPerYr/temp$HrsPerWeek,2)

mean(subset(temp,Scenario_ID==BaselineScenario & Year==min(temp$Year))$FTE50) #calculated on monthly average, expected value 2021
max(subset(temp,Scenario_ID==BaselineScenario & Year==min(temp$Year))$FTE50) #calculated on monthly maximum, expected value 2021
max(subset(temp,Scenario_ID==BaselineScenario & Year==min(temp$Year))$FTE95) #calculated on monthly maximum, 95th CI value 2021

mean(subset(temp,Scenario_ID==BaselineScenario & Year==max(temp$Year))$FTE50) #calculated on monthly average, expected value 2035
max(subset(temp,Scenario_ID==BaselineScenario & Year==max(temp$Year))$FTE50) #calculated on monthly maximum, expected value 2035
max(subset(temp,Scenario_ID==BaselineScenario & Year==max(temp$Year))$FTE95) #calculated on monthly maximum, 95th CI value 2035

############################################################################################################################################

#Sensitivity components plot - impact of various contributors to capacity needs

#Scenarios list
SC <- data.frame(sc_list=c("NoChangeBaseline_RR","p_Seasonality_RR","p_Demographics_RR","p_ChildHealthImp_RR","NoFertilityChange_RuralRatio","ExpectedValue_RuralRatio"),
                 sc_name=c("Baseline 2021","+ Seasonality effect","+ Pop growth & pyramid","+ Ongoing decrease U5 fever, diarh & pneumo","+ Ongoing decrease TB, HIV & malaria","+ Ongoing decrease fertility"))

#Calculation of differences for plot
SC$TotalTime_mean <- 0 # Will need to pull these in once we have more scenarios actually run.
SC$TotalTime_95 <- 0

SC$TotalFTEs_mean <- 0
SC$TotalFTEs_95 <- 0

SC$TotalFTEs_mean = c(1.57,1.57,2.54,2.43,2.33,1.74) #For Demo Purposes ONLY
SC$TotalFTEs_95 = SC$TotalFTEs_mean * runif(6,min=1.1,max=1.4) #For Demo Purposes ONLY

SC$Scenario_diff <- 0
SC$Base_value <- 0
SC$Delta_bar <- 0

for (rw in 1:nrow(SC)){
  if(rw==1){
    SC$Scenario_diff[1] = SC$TotalFTEs_mean[1]
    SC$Delta_bar[1] = SC$TotalFTEs_mean[1]
  }else{
    SC$Scenario_diff[rw] = SC$TotalFTEs_mean[rw]-SC$TotalFTEs_mean[rw-1]
    if(SC$Scenario_diff[rw]>0){
      SC$Base_value[rw] = SC$TotalFTEs_mean[rw-1]
    }else{
      SC$Base_value[rw] = SC$TotalFTEs_mean[rw]
    }
    SC$Delta_bar[rw] = abs(SC$Scenario_diff[rw])
  }
}

SC$PosNeg <- "positive"
SC$PosNeg[SC$Scenario_diff < 0] = "negative"
SCM <- melt(SC,id=c("sc_list","sc_name","PosNeg"))
SCM <- subset(SCM,variable=="Base_value" | variable=="Delta_bar")
SCM$color <- ""
SCM$alpha <- 1
SCM$color[SCM$PosNeg=="negative"] = "darkgreen"
SCM$color[SCM$PosNeg=="positive"] = "tomato4"
SCM$color[SCM$variable=="Base_value" | SCM$sc_name==SC$sc_name[1]] = "grey32"
SCM$alpha[SCM$sc_name!=SC$sc_name[1] & SCM$variable=="Base_value"] = .35
SCM$sc_name = factor(SCM$sc_name,levels=SC$sc_name,ordered=TRUE)

ggplot()+geom_bar(data=SCM,aes(x=sc_name,y=value),stat="identity",alpha=SCM$alpha,fill=SCM$color)+theme_bw()+xlab("")+ylab("FTEs per Catchment")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_y_continuous(labels=scales::number_format(accuracy = 0.1),breaks=seq(0,trunc(max(SC$TotalFTEs_95))+1,.5))+
  geom_line(data=SC,aes(x=seq(1,6),y=TotalFTEs_95),size=1)




