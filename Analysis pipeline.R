
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





READ_IN = FALSE #Read in .csv files for summary stats, yes/no
WRITE_OUT = TRUE #Write out .csv files for summary stats, yes/no
date <- "Jan26"
BaselineScenario <- "ExpectedValue_NtlRatio" #ExpectedValue_RuralRatio
NationalBaseline <- "ExpectedValue_NtlRatio"
StableFertility <- "NoFertilityChange_NR" #NoFertilityChange_RuralRatio







scenarios <- read_xlsx("config/R Model Inputs.xlsx",sheet="Scenarios")
params <- read_xlsx("config/R Model Inputs.xlsx",sheet="StochasticParamaters")

#load simulation data
remove(DR)
for (i in 1:nrow(scenarios)){
  print(paste("Loading scenario",i))
  scenario <- scenarios$UniqueID[i]
  tempupload <- read.csv(paste("results/results_",scenario,"_",date,".csv",sep=""))
  if(!exists('DR')){
    DR <- tempupload
  }else{
    DR <- rbind(DR,tempupload)
  }
  remove(tempupload)
}

#import permanent data
taskvalues <- read_xlsx("config/R Model Inputs.xlsx",sheet="TaskValues")
taskvalues <- subset(taskvalues,Geography=="National")
taskvalues$StartingRateInPop[is.na(taskvalues$StartingRateInPop)] = 0
taskvalues$NumContactsAnnual[is.na(taskvalues$NumContactsAnnual)] = 0
taskvalues$NumContactsPerUnit[is.na(taskvalues$NumContactsPerUnit)] = 0
taskvalues$MinsPerContact[is.na(taskvalues$MinsPerContact)] = 0
taskvalues$HoursPerWeek[is.na(taskvalues$HoursPerWeek)] = 0
taskvalues$FTEratio[is.na(taskvalues$FTEratio)] = 0

#cleaning
DR$Service_time <- DR$Service_time/60 #minutes to hours

#Merge together data sets
scenarios$Scenario_ID <- scenarios$UniqueID
DR <- left_join(DR,scenarios,by=c("Scenario_ID"))
taskvalues$Task_ID <- taskvalues$Indicator
DR$CommonName <- taskvalues$CommonName[match(DR$Task_ID,taskvalues$Task_ID)]
DR$ClinicalOrNon <- taskvalues$ClinicalOrNon[match(DR$Task_ID,taskvalues$Task_ID)]
DR$ClinicalCat <- taskvalues$ClinicalCat[match(DR$Task_ID,taskvalues$Task_ID)]
DR$ServiceCat <- taskvalues$ServiceCat[match(DR$Task_ID,taskvalues$Task_ID)]
DR$NumContactsPer <- taskvalues$NumContactsPerUnit[match(DR$Task_ID,taskvalues$Task_ID)] + taskvalues$NumContactsAnnual[match(DR$Task_ID,taskvalues$Task_ID)]

DR <- subset(DR,Year<2036 & Year>2020) #Don't use year 2020 because it's an artifact. Subsetting to < 2036 to align with 15-year plan horizon.
DR$ServiceCat[DR$ClinicalOrNon!="Clinical"]=DR$CommonName[DR$ClinicalOrNon!="Clinical"]

# Will want to do these same analyses (all of below) for rural only vs. national to see how different they are, but probably rural-only in the GR slide deck.

############################################################################################################################################
############################################################################################################################################

# Create stochastic hours per week value. Should only be used for small population estimates.

sdvalue <- params$p[params$Value=="Hours per week"]
StochasticHrs <- ddply(DR,.(Scenario_ID,Trial_num,Year,HrsPerWeek),summarize)
StochasticHrs$HPW_stochastic <- runif(nrow(StochasticHrs),min=StochasticHrs$HrsPerWeek-StochasticHrs$HrsPerWeek*sdvalue,max=StochasticHrs$HrsPerWeek+StochasticHrs$HrsPerWeek*sdvalue)
StochasticHrs$HPW_stochastic[StochasticHrs$HPW_stochastic > 40] = 40

StochasticHrs$Lookup <- paste(StochasticHrs$Scenario_ID,StochasticHrs$Trial_num,StochasticHrs$Year)
DR$Lookup <- paste(DR$Scenario_ID,DR$Trial_num,DR$Year)

DR$HPW_stochastic <- StochasticHrs$HPW_stochastic[match(DR$Lookup,StochasticHrs$Lookup)]

############################################################################################################################################
############################################################################################################################################


if(READ_IN==FALSE){
  #Create summary tables
  sumsub <- subset(DR,Scenario_ID==BaselineScenario | Scenario_ID==StableFertility)
  
  ByRun_ServiceCat <- ddply(sumsub,.(Scenario_ID,Trial_num,Year,ServiceCat,ClinicalOrNon),summarize,TotHrs=sum(Service_time))
  Mean_ServiceCat <- ddply(ByRun_ServiceCat,.(Scenario_ID,Year,ServiceCat,ClinicalOrNon),summarize,MeanHrs=mean(TotHrs),CI95 = quantile(TotHrs,probs=c(.95)))
  
  sumsub <- subset(sumsub, ClinicalOrNon=="Clinical")
  
  ByRun_TotClin <- ddply(sumsub,.(Scenario_ID,Trial_num,Year,WeeksPerYr,HrsPerWeek),summarize,AnnualHrs=sum(Service_time))
  Stats_TotClin <- ddply(ByRun_TotClin,.(Scenario_ID,Year,WeeksPerYr,HrsPerWeek),summarize,CI05 = quantile(AnnualHrs,probs=c(.05)),CI50 = mean(AnnualHrs),CI95 = quantile(AnnualHrs,probs=c(.95)))
  
  sumsub <- subset(DR,Scenario_ID==BaselineScenario)
  
  ByRun_ClinCat <- ddply(sumsub, .(Scenario_ID,Trial_num,Year,ClinicalCat,ClinicalOrNon),summarize,TotHrs = sum(Service_time))
  Mean_ClinCat <- ddply(ByRun_ClinCat, .(Scenario_ID,Year,ClinicalCat,ClinicalOrNon),summarize,MeanHrs = mean(TotHrs))
  
  ByRun_Total <- ddply(sumsub, .(Scenario_ID,Trial_num,Year,WeeksPerYr,HrsPerWeek),summarize,TotHrs = sum(Service_time))
  Mean_Total <- ddply(ByRun_Total, .(Scenario_ID,Year,WeeksPerYr,HrsPerWeek),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))
  
  ByRun_ClinMonth <- ddply(subset(sumsub,ClinicalOrNon=="Clinical"),.(Scenario_ID,Trial_num,Year,Month,WeeksPerYr,HrsPerWeek),summarize,TotHrs=sum(Service_time))
  Stats_ClinMonth <- ddply(ByRun_ClinMonth,.(Scenario_ID,Year,Month,WeeksPerYr,HrsPerWeek),summarize,CI05 = quantile(TotHrs,probs=c(.05)),CI50 = mean(TotHrs),CI95 = quantile(TotHrs,probs=c(.95)))
}else{
  Mean_ServiceCat <- read.csv("results/Mean_ServiceCat.csv")
  Stats_TotClin <- read.csv("results/Stats_TotClin.csv")
  Mean_ClinCat <- read.csv("results/Mean_ClinCat.csv")
  Mean_Total <- read.csv("results/Mean_Total.csv")
  Stats_ClinMonth <- read.csv("results/Stats_ClinMonth.csv")
}

if(WRITE_OUT==TRUE){
  write.csv(Mean_ServiceCat,"results/Mean_ServiceCat.csv")
  write.csv(Stats_TotClin,"results/Stats_TotClin.csv")
  write.csv(Mean_ClinCat,"results/Mean_ClinCat.csv")
  write.csv(Mean_Total,"results/Mean_Total.csv")
  write.csv(Stats_ClinMonth,"results/Stats_ClinMonth.csv")
}


# Only need this for testing plots at top of list below.
#ByRun_ByClinType <- ddply(sumsub,.(Scenario_ID,ServiceCat,Trial_num,Run_num,Year),summarize,AnnualHrs=sum(Service_time))
#Stats_ByClinType <- ddply(ByRun_ByClinType,.(Scenario_ID,ServiceCat,Year),summarize,CI05 = quantile(AnnualHrs,probs=c(.05)),CI50 = mean(AnnualHrs),CI95 = quantile(AnnualHrs,probs=c(.95)))

# Possible speed up code.
#ddply(df, .(id), summarise, percent_total = sum(percent))

#df <- data.table(df, key = c("id"))
#df[, list(percent_total = sum(percent)), by = key(df)]

#tdf <- data.table(tdf, key=c("Scenario_ID","Trial_num","Year","ServiceCat","ClinicalOrNon"))
#ByRun_ServiceCat <- tdf[, list(TotHrs=sum(Service_time)), by=key(tdf)]

############################################################################################################################################
############################################################################################################################################

#calculations of how many CHW are needed, clinical and total

popratio <- 115000000 / 3000

#Stats_TotClin$MeanHrs[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario]
#Stats_TotClin$CI05[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario]
#Stats_TotClin$CI95[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2021 & Stats_TotClin$Scenario_ID==BaselineScenario]
popratio * Stats_TotClin$CI50[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline]
popratio * Stats_TotClin$CI05[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline]
popratio * Stats_TotClin$CI95[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==NationalBaseline]

#Mean_Total$MeanHrs[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario] / Mean_Total$WeeksPerYr[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario] / Mean_Total$HrsPerWeek[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario]
#Mean_Total$CI05[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario] / Mean_Total$WeeksPerYr[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario] / Mean_Total$HrsPerWeek[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario]
#Mean_Total$CI95[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario] / Mean_Total$WeeksPerYr[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario] / Mean_Total$HrsPerWeek[Mean_Total$Year==2021 & Mean_Total$Scenario_ID==BaselineScenario]
popratio * Mean_Total$MeanHrs[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline] / Mean_Total$WeeksPerYr[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline] / Mean_Total$HrsPerWeek[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline]
popratio * Mean_Total$CI05[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline] / Mean_Total$WeeksPerYr[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline] / Mean_Total$HrsPerWeek[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline]
popratio * Mean_Total$CI95[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline] / Mean_Total$WeeksPerYr[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline] / Mean_Total$HrsPerWeek[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==NationalBaseline]


############################################################################################################################################

# Test of uncertainty in outcomes

#test <- subset(Stats_TotClin,Scenario_ID==BaselineScenario)
#ggplot(data=test)+theme_bw()+geom_line(aes(x=Year,y=CI05))+geom_line(aes(x=Year,y=CI50))+geom_line(aes(x=Year,y=CI95))

#test <- subset(Stats_ByClinType,Scenario_ID==BaselineScenario)
#ggplot(data=test)+facet_wrap(~ServiceCat, scales = "free")+geom_line(aes(x=Year,y=CI05))+geom_line(aes(x=Year,y=CI50))+geom_line(aes(x=Year,y=CI95))+theme_bw()


############################################################################################################################################

#graphic for slide 4 (hours per week on clinical, development, and total work)
#dashed line is actual hours worked per week, according to time and motion study
#solid line is the 95th percentile, simulated
#bars are the simulated expected value for time required, best case with perfect scheduling

temp <- subset(Mean_ClinCat,Scenario_ID==BaselineScenario)
weeksperyear = mean(DR$WeeksPerYr[DR$Scenario_ID==BaselineScenario])
hoursperweek = mean(DR$HrsPerWeek[DR$Scenario_ID==BaselineScenario])
temp$Category <- paste(temp$ClinicalCat)
temp$Category[temp$ClinicalOrNon=="Clinical"] = paste("Clinical -",temp$Category[temp$ClinicalOrNon=="Clinical"])
temp$Category[temp$ClinicalOrNon!="Clinical"] = temp$ClinicalOrNon[temp$ClinicalOrNon!="Clinical"]
temp$Category <- factor(temp$Category,ordered=TRUE,levels=unique(temp$Category))
temp$Alpha <- 1
temp$Alpha[temp$ClinicalOrNon!="Clinical"] = .3
temp <- subset(temp,Year<2036)

# plot1 <- ggplot()+
#   geom_bar(data=temp,aes(x=Year,y=MeanHrs/weeksperyear,fill=Category),stat="identity",alpha=temp$Alpha)+
#   geom_hline(linetype="dashed",color="black",yintercept=hoursperweek)+
#   geom_line(data=subset(Mean_Total,Scenario_ID==BaselineScenario),aes(x=Year,y=MeanHrs/weeksperyear))+
#   geom_point(data=subset(Mean_Total,Scenario_ID==BaselineScenario),aes(x=Year,y=MeanHrs/weeksperyear))+
#   geom_errorbar(data=subset(Mean_Total,Scenario_ID==BaselineScenario),aes(x=Year,ymin=CI05/weeksperyear, ymax=CI95/weeksperyear), colour="black", width=.25)+
#   theme_bw()+
#   scale_x_continuous(breaks = seq(2021,2035))+
#   theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
#   scale_fill_viridis_d()+
#   ylab("Hours per Week per Staff") + xlab("")

plot1 <- ggplot()+
  geom_bar(data=temp,aes(x=Year,y=MeanHrs/weeksperyear,fill=Category),stat="identity",alpha=.9)+
  geom_line(data=subset(Mean_Total,Scenario_ID==BaselineScenario),aes(x=Year,y=MeanHrs/weeksperyear),size=1.2)+
  geom_point(data=subset(Mean_Total,Scenario_ID==BaselineScenario),aes(x=Year,y=MeanHrs/weeksperyear))+
  geom_errorbar(data=subset(Mean_Total,Scenario_ID==BaselineScenario),aes(x=Year,ymin=CI05/weeksperyear, ymax=CI95/weeksperyear), colour="black", width=.3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(2021,2035))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
  scale_fill_viridis_d()+
  ylab("Hours per Week per HEW") + xlab("")

print(plot1)

jpeg(paste("results/Weekly workload per HEW",date,".jpeg",sep=""), width = 6, height = 4, units = 'in', res = 700)
print(plot1)
dev.off()

############################################################################################################################################

# chart for slide 5 (mix of services in a given year)

temp <- subset(Mean_ServiceCat,Year==2021 & Scenario_ID==BaselineScenario)
temp$ServiceCat[temp$ServiceCat=="MHH and outreach"] = "MHH & Outreach"
temp$ServiceCat[temp$ServiceCat=="Administration"] = "Admin"
temp$ServiceCat[temp$ServiceCat=="Record keeping"] = "Records"
temp$ServiceCat[temp$ServiceCat=="Tuberculosis"] = "TB"
temp$ServiceCat[temp$ServiceCat=="Family planning"] = "FP"
temp$ServiceCat[temp$ServiceCat=="Immunization"] = "RI"
temp$ServiceCat[temp$ServiceCat=="Mental health"] = "Mental"

plot2 <- ggplot(temp,aes(area=MeanHrs,fill=ClinicalOrNon,label=ServiceCat,subgroup=ClinicalOrNon))+
  geom_treemap()+geom_treemap_text(color="darkgrey",place="center",size=14)+
  geom_treemap_subgroup_border(color="black",size=2.5)+
  theme_bw()+theme(legend.position = "none")+
  scale_fill_viridis_d()
print(plot2)

# Treemap reference: https://r-charts.com/part-whole/treemapify/



jpeg(paste("results/Time Mix 2021",date,".jpeg",sep=""), width = 5, height = 4, units = 'in', res = 700)
print(plot2)
dev.off()


#line chart for slide 5 - service mix change over time

temp <- subset(Mean_ServiceCat,ClinicalOrNon=="Clinical" & Scenario_ID==BaselineScenario)
temp <- subset(temp,Year<2036)
tempyr1 <- subset(temp,Year==2021)
temp$RatioTo1 <- temp$MeanHrs/tempyr1$MeanHrs[match(temp$ServiceCat,tempyr1$ServiceCat)]
temp$ServiceCat <- as.factor(temp$ServiceCat)
temp <- subset(temp,ServiceCat!="NTDs (LF)")

#make label positions
LastYear<-subset(temp,Year==max(temp$Year))
temp$RatioLastYr <- LastYear$RatioTo1[match(temp$ServiceCat,LastYear$ServiceCat)]
temp$RatioLastYr[temp$ServiceCat=="Mental health"] = temp$RatioLastYr[temp$ServiceCat=="Mental health"]-.03 #separate overlapping labels
temp$RatioLastYr[temp$ServiceCat=="First Aid"] = temp$RatioLastYr[temp$ServiceCat=="First Aid"]+.02 #separate overlapping labels
temp$RatioLastYr[temp$ServiceCat=="Sick child"] = temp$RatioLastYr[temp$ServiceCat=="Sick child"]-.08 #separate overlapping labels
temp$RatioLastYr[temp$ServiceCat=="Malaria"] = temp$RatioLastYr[temp$ServiceCat=="Malaria"]+.08 #separate overlapping labels
temp$RatioLastYr[temp$ServiceCat=="Pregnancy"] = temp$RatioLastYr[temp$ServiceCat=="Pregnancy"]+.01 #separate overlapping labels
temp$RatioLastYr[temp$ServiceCat=="Nutrition"] = temp$RatioLastYr[temp$ServiceCat=="Nutrition"]-.07 #separate overlapping labels
temp$RatioLastYr[temp$ServiceCat=="Immunization"] = temp$RatioLastYr[temp$ServiceCat=="Immunization"]+.04 #separate overlapping labels
temp$RatioLastYr[temp$ServiceCat=="NTDs (LF)"] = temp$RatioLastYr[temp$ServiceCat=="NTDs (LF)"]+.06 #separate overlapping labels
temp$RatioLastYr[temp$ServiceCat=="HIV"] = temp$RatioLastYr[temp$ServiceCat=="HIV"]-.04 #separate overlapping labels

temp$Label <- ""
temp$Label[temp$Year==max(temp$Year)] = paste(temp$ServiceCat[temp$Year==max(temp$Year)],", ",round(temp$RatioLastYr[temp$Year==max(temp$Year)],1),sep="")

plot3 <- ggplot(temp,aes(x=Year,y=RatioTo1,group=ServiceCat))+geom_line(aes(color=ServiceCat),size=1.1)+
  geom_hline(yintercept = 1,color="black",linetype="dashed")+
  theme_bw()+xlab("")+ylab("Ratio to Baseline Year")+theme(legend.position="none",axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
  scale_color_discrete()+
  geom_text(aes(x=max(Year)+.2,y=RatioLastYr,label=Label),color="darkgrey",size=2.8,hjust=0)+
  scale_x_continuous(breaks = seq(2021,2035),limits=c(2021,max(temp$Year)+6))
print(plot3)

jpeg(paste("results/Time Mix Change Over Time",date,".jpeg",sep=""), width = 5*4.2/5, height = 4*3.5/5, units = 'in', res = 700)
print(plot3)
dev.off()


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
temp$alphavalue <- 0
temp$alphavalue[temp$Scenario_ID==BaselineScenario] = 1
temp$alphavalue[temp$Scenario_ID==StableFertility] = .6
#temp$MeanHrs[temp$Scenario_ID==StableFertility] = temp$MeanHrs[temp$Scenario_ID==StableFertility]*1.2 #just for demo purposes to get a difference between scenarios

#Box outline is stable fertility 95th percentile
#Light green is stable fertility scenario
#Dark green is continuing decline in fertility scenario (expected/baseline)
plot4 <- ggplot(temp,aes(x=ServiceCat,y=MeanHrs,group=Year))+
  geom_bar(data=subset(temp,Scenario_ID==StableFertility),aes(x=ServiceCat,y=CI95,group=Year),stat="identity",position="dodge",fill="grey",alpha=.65)+
  geom_bar(alpha=temp$alphavalue,stat="identity",position="dodge",fill="darkslategray")+
#  geom_line(data=subset(temp,Scenario_ID==StableFertility),aes(x=ServiceCat,y=CI95,group=Year),color="darkgrey")+
  theme_bw()+
  xlab("")+ylab("Annual Hours")+
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5))+
  scale_y_continuous(labels=comma)
plot(plot4)

jpeg(paste("results/Fertility Effects Over Time",date,".jpeg",sep=""), width = 5*4/5, height = 4*4/5, units = 'in', res = 700)
print(plot4)
dev.off()


############################################################################################################################################

#Seasonality impact analysis

temp <- subset(Stats_ClinMonth,Scenario_ID==BaselineScenario)
temp <- subset(temp,Year==min(temp$Year) | Year==max(temp$Year))

#calculate FTE need differences
temp$FTE50 <- round(temp$CI50/temp$WeeksPerYr/temp$HrsPerWeek,2)
temp$FTE95 <- round(temp$CI95/temp$WeeksPerYr/temp$HrsPerWeek,2)

mean(subset(temp,Scenario_ID==BaselineScenario & Year==min(temp$Year))$FTE50) #calculated on monthly average, expected value 2021
max(subset(temp,Scenario_ID==BaselineScenario & Year==min(temp$Year))$FTE50) #calculated on monthly maximum, expected value 2021
max(subset(temp,Scenario_ID==BaselineScenario & Year==min(temp$Year))$FTE95) #calculated on monthly maximum, 95th CI value 2021

mean(subset(temp,Scenario_ID==BaselineScenario & Year==max(temp$Year))$FTE50) #calculated on monthly average, expected value 2035
max(subset(temp,Scenario_ID==BaselineScenario & Year==max(temp$Year))$FTE50) #calculated on monthly maximum, expected value 2035
max(subset(temp,Scenario_ID==BaselineScenario & Year==max(temp$Year))$FTE95) #calculated on monthly maximum, 95th CI value 2035

monthly_avg_2021 <- mean(subset(temp,Year==2021)$CI50)
monthly_avg_2035 <- mean(subset(temp,Year==2035)$CI50)

temp$MonthlyAvg <- 0
temp$MonthlyAvg[temp$Year==2021] = monthly_avg_2021
temp$MonthlyAvg[temp$Year==2035] = monthly_avg_2035

#Normalize everything to hours per week
temp$CI05 <- temp$CI05/(temp$WeeksPerYr/12)
temp$CI95 <- temp$CI95/(temp$WeeksPerYr/12)
temp$CI50 <- temp$CI50/(temp$WeeksPerYr/12)
temp$MonthlyAvg <- temp$MonthlyAvg/(temp$WeeksPerYr/12)

ggplot(temp,aes(x=Month,y=CI50,group=as.factor(Year)))+theme_bw()+ylab("Mean Clinical Hours per Week per Staff")+
  geom_ribbon(aes(ymin=CI05,ymax=CI95,fill=as.factor(Year)),alpha=.3)+geom_line(aes(color=as.factor(Year)))+
  scale_x_continuous(breaks=seq(1,12))+theme(legend.title=element_blank())+
  scale_y_continuous(labels=comma) + facet_wrap(~Year)

temp <- subset(temp,Year==2021)
plot5 <- ggplot(temp,aes(x=Month,y=CI50/MonthlyAvg))+theme_bw()+ylab("Ratio: Clinical Time / Monthly Mean") +
  geom_hline(yintercept=1,color="black",linetype="dashed")+
  geom_ribbon(aes(ymin=CI05/MonthlyAvg,ymax=CI95/MonthlyAvg),alpha=.3,fill="darkblue")+geom_line(color="darkblue")+
  scale_x_continuous(breaks=seq(1,12))+theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::number_format(accuracy = 0.1),limits=c(.75,1.25))
print(plot5)

jpeg(paste("results/Seasonality effect on clinical time",date,".jpeg"), width = 5*4/5, height = 4*4/5, units = 'in', res = 700)
print(plot5)
dev.off()


############################################################################################################################################

#Sensitivity components plot - impact of various contributors to capacity needs

#Set up dataframe
SC <- data.frame(sc_name=c("Baseline 2021","Seasonality","Pop growth","Decreases in U5 illness","Decreases in TB, HIV & malaria","Ongoing decrease fertility"))
SC$TotalTime_mean <- 0 
SC$TotalTime_95 <- 0 
SC$TotalTime_05 <- 0 
SC$Scenario_diff <- 0
SC$Base_value <- 0
SC$Delta_bar <- 0

#Calculate and save baseline values from year 1, before anything has changed.
Baseline_byrun <- ddply(subset(DR,Year==2021 & Scenario_ID==BaselineScenario),.(Trial_num),summarize,TotHrs=sum(Service_time))
Baseline_stats <- ddply(Baseline_byrun,.(),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))

SC$TotalTime_mean[1] = Baseline_stats$MeanHrs[1]
SC$TotalTime_95[1] = Baseline_stats$CI95[1]
SC$TotalTime_05[1] = Baseline_stats$CI05[1]

#Calculate and save baseline values from year 1, factoring in seasonality (select max month).
Baseline_month_byrun <- ddply(subset(DR,Year==2021 & Scenario_ID==BaselineScenario),.(Trial_num,Month),summarize,TotHrs=sum(Service_time))
Baseline_month_max <- ddply(Baseline_month_byrun,.(Trial_num),summarize,MaxMonthHrs=max(TotHrs)) 
Baseline_month_stats <- ddply(Baseline_month_max,.(),summarize,AvgHighestMonth=mean(MaxMonthHrs), CI05= quantile(MaxMonthHrs,probs=c(.05)), CI95= quantile(MaxMonthHrs,probs=c(.95)))

SC$TotalTime_mean[2] = Baseline_month_stats$AvgHighestMonth[1]*12
SC$TotalTime_95[2] = Baseline_month_stats$CI95[1]*12
SC$TotalTime_05[2] = Baseline_month_stats$CI05[1]*12

#Calculate and save baseline values from the last year, factoring in seasonality (select max month).
Baseline_month_byrun <- ddply(subset(DR,Year==2035 & Scenario_ID==BaselineScenario),.(Trial_num,Month),summarize,TotHrs=sum(Service_time))
Baseline_month_max <- ddply(Baseline_month_byrun,.(Trial_num),summarize,MaxMonthHrs=max(TotHrs)) 
Baseline_month_stats <- ddply(Baseline_month_max,.(),summarize,AvgHighestMonth=mean(MaxMonthHrs), CI05= quantile(MaxMonthHrs,probs=c(.05)), CI95= quantile(MaxMonthHrs,probs=c(.95)))

SC$TotalTime_mean[6] = Baseline_month_stats$AvgHighestMonth[1]*12
SC$TotalTime_95[6] = Baseline_month_stats$CI95[1]*12
SC$TotalTime_05[6] = Baseline_month_stats$CI05[1]*12

#Calculate and save values from the no-improvements scenario, basically the worst possible outcome, final year
WorstCase_byrun <- ddply(subset(DR,Year==2035 & Scenario_ID=="p_Demographics_NR"),.(Trial_num),summarize,TotHrs=sum(Service_time))
WorstCase_stats <- ddply(WorstCase_byrun,.(),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))

SC$TotalTime_mean[3] = WorstCase_stats$MeanHrs[1]
SC$TotalTime_95[3] = WorstCase_stats$CI95[1]
SC$TotalTime_05[3] = WorstCase_stats$CI05[1]

#Calculate and save values from the child-health-improvement only scenario, final year
ChHealthImprv_byrun <- ddply(subset(DR,Year==2035 & Scenario_ID=="p_ChildHealthImp_NR"),.(Trial_num),summarize,TotHrs=sum(Service_time))
ChHealthImprv_stats <- ddply(ChHealthImprv_byrun,.(),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))

SC$TotalTime_mean[4] = ChHealthImprv_stats$MeanHrs[1]
SC$TotalTime_95[4] = ChHealthImprv_stats$CI95[1]
SC$TotalTime_05[4] = ChHealthImprv_stats$CI05[1]

#Calculate and save values from the Child Improvement + TB/M/HIV improvement scenario, final year
InfDisImprv_byrun <- ddply(subset(DR,Year==2035 & Scenario_ID=="NoFertilityChange_NR"),.(Trial_num),summarize,TotHrs=sum(Service_time))
InfDisImprv_stats <- ddply(InfDisImprv_byrun,.(),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))

SC$TotalTime_mean[5] = InfDisImprv_stats$MeanHrs[1]
SC$TotalTime_95[5] = InfDisImprv_stats$CI95[1]
SC$TotalTime_05[5] = InfDisImprv_stats$CI05[1]



for (rw in 1:nrow(SC)){
  if(rw==1){
    SC$Scenario_diff[1] = SC$TotalTime_mean[1]
    SC$Delta_bar[1] = SC$TotalTime_mean[1]
  }else{
    SC$Scenario_diff[rw] = SC$TotalTime_mean[rw]-SC$TotalTime_mean[rw-1]
    if(SC$Scenario_diff[rw]>0){
      SC$Base_value[rw] = SC$TotalTime_mean[rw-1]
    }else{
      SC$Base_value[rw] = SC$TotalTime_mean[rw]
    }
    SC$Delta_bar[rw] = abs(SC$Scenario_diff[rw])
  }
}

SC$PosNeg <- "positive"
SC$PosNeg[SC$Scenario_diff < 0] = "negative"
SCM <- melt(SC,id=c("sc_name","PosNeg"))
SCM <- subset(SCM,variable=="Base_value" | variable=="Delta_bar")
SCM$color <- ""
SCM$alpha <- 1
SCM$color[SCM$PosNeg=="negative"] = "darkgreen"
SCM$color[SCM$PosNeg=="positive"] = "tomato4"
SCM$color[SCM$variable=="Base_value" | SCM$sc_name==SC$sc_name[1]] = "grey32"
SCM$alpha[SCM$sc_name!=SC$sc_name[1] & SCM$variable=="Base_value"] = .35
SCM$sc_name = factor(SCM$sc_name,levels=SC$sc_name,ordered=TRUE)

plot6 <- ggplot()+
  geom_bar(data=SCM,aes(x=sc_name,y=value),stat="identity",alpha=SCM$alpha,fill=SCM$color)+
  geom_errorbar(data=SC,aes(x=sc_name,ymin=TotalTime_05, ymax=TotalTime_95), colour="black", width=.2)+
  geom_point(data=SC,aes(x=sc_name,y=TotalTime_mean), colour="black")+
  theme_bw()+xlab("")+ylab("Annual Hours (Total)")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(labels=comma,breaks=seq(0,trunc(max(SC$TotalTime_95))+10,1500))
#  geom_line(data=SC,aes(x=seq(1,nrow(SC)),y=TotalTime_95),size=1)
print(plot6)

jpeg(paste("results/Scenario sensitivity analysis",date,".jpeg",sep=""), width = 4.8, height = 3.2, units = 'in', res = 700)
print(plot6)
dev.off()


