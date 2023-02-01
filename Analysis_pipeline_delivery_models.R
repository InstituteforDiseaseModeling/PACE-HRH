
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
scenarios <- read_xlsx("config/model_inputs.xlsx",sheet="Scenarios")
GeoSelect <- scenarios$Geography_dontedit[1] #User should make sure this matches with geography in names of csv files to be read in

#read in summary statistics csv files generated from post-processing steps in "Run_simulations.R"
Mean_ServiceCat <- read.csv(paste("results/Mean_ServiceCat_",GeoSelect,"_",date,".csv",sep=""))
Stats_TotClin <- read.csv(paste("results/Stats_TotClin_",GeoSelect,"_",date,".csv",sep=""))
Mean_ClinCat <- read.csv(paste("results/Mean_ClinCat_",GeoSelect,"_",date,".csv",sep=""))
Mean_Total <- read.csv(paste("results/Mean_Total_",GeoSelect,"_",date,".csv",sep=""))
Stats_ClinMonth <- read.csv(paste("results/Stats_ClinMonth_",GeoSelect,"_",date,".csv",sep=""))
Mean_Alloc <- read.csv(paste("results/Mean_Alloc_",GeoSelect,"_",date,".csv",sep=""))

############################################################################################################################################

#graphic for slide 4 (hours per week on clinical, development, and total work)
#dashed line is actual hours worked per week, according to time and motion study
#solid line is the 95th percentile, simulated
#bars are the simulated expected value for time required, best case with perfect scheduling

maxyval <- max(Mean_Total$CI95/Mean_Total$WeeksPerYr)*1.05
i=1

for(sc in unique(scenarios$UniqueID)){

  temp <- subset(Mean_ClinCat,Scenario_ID==sc)
  weeksperyear = mean(temp$WeeksPerYr[temp$Scenario_ID==sc])
  hoursperweek = mean(temp$HrsPerWeek[temp$Scenario_ID==sc])
  temp$Category <- paste(temp$ClinicalCat)
  temp$Category[temp$ClinicalOrNon=="Clinical"] = paste("Clinical -",temp$Category[temp$ClinicalOrNon=="Clinical"])
  temp$Category[temp$ClinicalOrNon!="Clinical"] = temp$ClinicalOrNon[temp$ClinicalOrNon!="Clinical"]
  temp$Category <- factor(temp$Category,ordered=TRUE,levels=unique(temp$Category))
  temp$Alpha <- 1
  temp$Alpha[temp$ClinicalOrNon!="Clinical"] = .3
  temp <- subset(temp,Year<2036)

  plottitle <- scenarios$UniqueID[i]

  plot1 <- ggplot()+
    geom_bar(data=temp,aes(x=Year,y=MeanHrs/WeeksPerYr,fill=Category),stat="identity",alpha=.9)+
    geom_line(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,y=MeanHrs/WeeksPerYr),linewidth=1.2)+
    geom_point(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,y=MeanHrs/WeeksPerYr))+
    geom_errorbar(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
    ylim(0,maxyval)+
    theme_bw()+
    scale_x_continuous(breaks = seq(2021,2035))+
    theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
    scale_fill_viridis_d()+
    ylab("Hours per Week per 2,500 Pop") + xlab("") + labs(title = plottitle)

  print(plot1)

  jpeg(paste("results/Weekly workload by Type","_",GeoSelect,"_",date,"_",sc,".jpeg",sep=""), width = 6, height = 4, units = 'in', res = 700)
  print(plot1)
  dev.off()

  i=i+1

}

############################################################################################################################################
#graphic from slide 4, but broken down by cadre instead
colorlist <- viridis(6,1,0,1,1,"D")
namelist <- c("MW_hrs","HO_hrs","RN_hrs","EH_hrs","FH_hrs","HEW_hrs")
renamelist <- c("Midwife","Health Off.","Nurse","Env. Health","Fam. Health","HEW")

for(sc in unique(scenarios$UniqueID)){

  temp <- subset(Mean_Alloc,Scenario_ID==sc)
  weeksperyear = mean(temp$WeeksPerYr[temp$Scenario_ID==sc])
  hoursperweek = mean(temp$HrsPerWeek[temp$Scenario_ID==sc])
  temp$colorselect <- colorlist[match(temp$Cadre,namelist)]
  temp$rename <- renamelist[match(temp$Cadre,namelist)]
  
  plot8 <- ggplot()+
    geom_bar(data=temp,aes(x=Year,y=CI50/WeeksPerYr,group=Cadre),fill=temp$colorselect,color="darkgrey",stat="identity",alpha=.9)+
    geom_line(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,y=MeanHrs/WeeksPerYr),linewidth=1.2)+
    theme_bw()+
    scale_x_continuous(breaks = seq(2021,max(temp$Year)))+
    theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
    ylab("Hours per Week per 2,500 Pop") + xlab("") + labs(title = paste("Time Allocation by Cadre",sc)) +
    scale_fill_discrete(labels=renamelist)

  print(plot8)

  jpeg(paste("results/Weekly workload by Cadre","_",GeoSelect,"_",date,"_",sc,".jpeg",sep=""), width = 6, height = 4, units = 'in', res = 700)
  print(plot8)
  dev.off()

}


############################################################################################################################################

# chart for slide 5 (mix of services in a given year)

for(sc in unique(scenarios$UniqueID)){

  temp <- subset(Mean_ServiceCat,Year==2021 & Scenario_ID==sc)
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

  temp <- subset(temp,ClinicalOrNon=="Clinical")

  plot9 <- ggplot(temp,aes(area=MeanHrs,fill=ServiceCat,label=ServiceCat,subgroup=ServiceCat))+
    geom_treemap()+geom_treemap_text(color="black",place="center",size=14)+
    geom_treemap_subgroup_border(color="black",size=2.5)+
    theme_bw()+theme(legend.position = "none")+
    scale_fill_viridis_d()
  print(plot9)

  jpeg(paste("results/Time Mix 2021","_",GeoSelect,"_",date,"_",sc,".jpeg",sep=""), width = 5, height = 4, units = 'in', res = 700)
  print(plot2)
  print(plot9)
  dev.off()

}



#line chart for slide 5 - service mix change over time

plotsub <- subset(Mean_ServiceCat,ClinicalOrNon=="Clinical" & Year<2036)
tempyr1 <- subset(plotsub,Year==2021)

plotsub$Lookup <- paste(plotsub$Scenario_ID,plotsub$ServiceCat)
tempyr1$Lookup <- paste(tempyr1$Scenario_ID,tempyr1$ServiceCat)
plotsub$RatioTo1 <- plotsub$MeanHrs/tempyr1$MeanHrs[match(plotsub$Lookup,tempyr1$Lookup)]
plotsub$ServiceCat <- as.factor(plotsub$ServiceCat)

ymaxdiff = max(plotsub$RatioTo1) - 1
yplotmax = (ymaxdiff+1)*1.02
yplotmin = (1-ymaxdiff)*1.02

for(sc in unique(scenarios$UniqueID)){
  temp <- subset(plotsub,Scenario_ID==sc)

  #make label positions
  LastYear<-subset(temp,Year==max(temp$Year))
  temp$RatioLastYr <- LastYear$RatioTo1[match(temp$ServiceCat,LastYear$ServiceCat)]
  temp$RatioLastYr[temp$ServiceCat=="Mental health"] = temp$RatioLastYr[temp$ServiceCat=="Mental health"] #separate overlapping labels
  temp$RatioLastYr[temp$ServiceCat=="First Aid"] = temp$RatioLastYr[temp$ServiceCat=="First Aid"] #separate overlapping labels
  temp$RatioLastYr[temp$ServiceCat=="Sick child"] = temp$RatioLastYr[temp$ServiceCat=="Sick child"] #separate overlapping labels
  temp$RatioLastYr[temp$ServiceCat=="Malaria"] = temp$RatioLastYr[temp$ServiceCat=="Malaria"] #separate overlapping labels
  temp$RatioLastYr[temp$ServiceCat=="Pregnancy"] = temp$RatioLastYr[temp$ServiceCat=="Pregnancy"] #separate overlapping labels
  temp$RatioLastYr[temp$ServiceCat=="Nutrition"] = temp$RatioLastYr[temp$ServiceCat=="Nutrition"] #separate overlapping labels
  temp$RatioLastYr[temp$ServiceCat=="Immunization"] = temp$RatioLastYr[temp$ServiceCat=="Immunization"] #separate overlapping labels
  temp$RatioLastYr[temp$ServiceCat=="NTDs (LF)"] = temp$RatioLastYr[temp$ServiceCat=="NTDs (LF)"] #separate overlapping labels
  temp$RatioLastYr[temp$ServiceCat=="HIV"] = temp$RatioLastYr[temp$ServiceCat=="HIV"] #separate overlapping labels

  temp$Label <- ""
  temp$Label[temp$Year==max(temp$Year)] = paste(temp$ServiceCat[temp$Year==max(temp$Year)],", ",round(temp$RatioLastYr[temp$Year==max(temp$Year)],1),sep="")

  plot3 <- ggplot(temp,aes(x=Year,y=RatioTo1,group=ServiceCat))+geom_line(aes(color=ServiceCat),size=1.1)+
    geom_hline(yintercept = 1,color="black",linetype="dashed")+
    theme_bw()+xlab("")+ylab("Ratio to Baseline Year")+theme(legend.position="none",axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
    scale_color_discrete()+
    geom_text(aes(x=max(Year)+.2,y=RatioLastYr,label=Label),color="darkgrey",size=2.8,hjust=0)+
    scale_x_continuous(breaks = seq(2021,2035),limits=c(2021,max(temp$Year)+6)) +
    scale_y_continuous(limits = c(yplotmin,yplotmax))+labs(title = paste(sc))

  print(plot3)

  jpeg(paste("results/Time Mix Change Over Time","_",GeoSelect,"_",date,"_",sc,".jpeg",sep=""), width = 4.1, height = 3.5, units = 'in', res = 700)
  print(plot3)
  dev.off()

}


############################################################################################################################################

#Seasonality impact analysis

Stats_ClinMonth$FTE50 <- round(Stats_ClinMonth$CI50/Stats_ClinMonth$HrsPerWeek,2)
Stats_ClinMonth$FTE95 <- round(Stats_ClinMonth$CI95/Stats_ClinMonth$HrsPerWeek,2)

Monthly_Avg <- ddply(Stats_ClinMonth,.(Scenario_ID,Year),summarize,MonthlyMean=mean(CI50))

Stats_ClinMonth$MonthlyAvg <- 0
Stats_ClinMonth$MonthlyAvg = Monthly_Avg$MonthlyMean[match(
                  paste(Stats_ClinMonth$Year,Stats_ClinMonth$Scenario_ID),paste(Monthly_Avg$Year,Monthly_Avg$Scenario_ID))]

Stats_ClinMonth$CI05 <- Stats_ClinMonth$CI05/(Stats_ClinMonth$WeeksPerYr/12)
Stats_ClinMonth$CI95 <- Stats_ClinMonth$CI95/(Stats_ClinMonth$WeeksPerYr/12)
Stats_ClinMonth$CI50 <- Stats_ClinMonth$CI50/(Stats_ClinMonth$WeeksPerYr/12)
Stats_ClinMonth$MonthlyAvg <- Stats_ClinMonth$MonthlyAvg/(Stats_ClinMonth$WeeksPerYr/12)


ps <- subset(Stats_ClinMonth,Year==2021)
plot6 <- ggplot(ps,aes(x=Month,y=CI50/MonthlyAvg,group=Scenario_ID,color=Scenario_ID,fill=Scenario_ID)) +
  geom_line(aes(x=Month,y=CI50/MonthlyAvg))+
  theme_bw() + ylab("Ratio to Avg.")+ labs(title="Seasonality of Clinical Work (2021)") +
  geom_ribbon(aes(ymin=CI05/MonthlyAvg,ymax=CI95/MonthlyAvg),alpha=.3) +
  geom_hline(yintercept=1,color="black",linetype="dashed")+
  scale_x_continuous(breaks=seq(1,12))+theme(legend.title=element_blank(),legend.position = "bottom")+
  scale_y_continuous(labels=comma,limits=c(.5,1.5))
print(plot6)

jpeg(paste("results/Seasonality effect on clinical time","_",date,"_2021AllScenarios",".jpeg"), width = 5, height = 3.5, units = 'in', res = 700)
print(plot6)
dev.off()

ps <- subset(Stats_ClinMonth,Year==2035)
plot7 <- ggplot(ps,aes(x=Month,y=CI50/MonthlyAvg,group=Scenario_ID,color=Scenario_ID,fill=Scenario_ID)) +
  geom_line(aes(x=Month,y=CI50/MonthlyAvg))+
  theme_bw() + ylab("Ratio to Avg.")+ labs(title="Seasonality of Clinical Work (2035)") +
  geom_ribbon(aes(ymin=CI05/MonthlyAvg,ymax=CI95/MonthlyAvg),alpha=.3) +
  geom_hline(yintercept=1,color="black",linetype="dashed")+
  scale_x_continuous(breaks=seq(1,12))+theme(legend.title=element_blank(),legend.position = "bottom")+
  scale_y_continuous(labels=comma,limits=c(.5,1.5))
print(plot7)

jpeg(paste("results/Seasonality effect on clinical time","_",date,"_2035AllScenarios",".jpeg"), width = 5, height = 3.5, units = 'in', res = 700)
print(plot7)
dev.off()


for(sc in unique(scenarios$UniqueID)){

  temp <- subset(Stats_ClinMonth,Scenario_ID==sc)
  temp <- subset(temp,Year==min(temp$Year) | Year==max(temp$Year))

  #calculate FTE need differences
  # temp$FTE50 <- round(temp$CI50/temp$WeeksPerYr/temp$HrsPerWeek,2)
  # temp$FTE95 <- round(temp$CI95/temp$WeeksPerYr/temp$HrsPerWeek,2)
  #
  mean(subset(temp,Scenario_ID==sc & Year==min(temp$Year))$FTE50) #calculated on monthly average, expected value 2021
  max(subset(temp,Scenario_ID==sc & Year==min(temp$Year))$FTE50) #calculated on monthly maximum, expected value 2021
  max(subset(temp,Scenario_ID==sc & Year==min(temp$Year))$FTE95) #calculated on monthly maximum, 95th CI value 2021

  mean(subset(temp,Scenario_ID==sc & Year==max(temp$Year))$FTE50) #calculated on monthly average, expected value 2035
  max(subset(temp,Scenario_ID==sc & Year==max(temp$Year))$FTE50) #calculated on monthly maximum, expected value 2035
  max(subset(temp,Scenario_ID==sc & Year==max(temp$Year))$FTE95) #calculated on monthly maximum, 95th CI value 2035

  #monthly_avg_2021 <- mean(subset(temp,Year==2021)$CI50)
  #monthly_avg_2035 <- mean(subset(temp,Year==2035)$CI50)

  #temp$MonthlyAvg <- 0
  #temp$MonthlyAvg[temp$Year==2021] = monthly_avg_2021
  #temp$MonthlyAvg[temp$Year==2035] = monthly_avg_2035

  #Normalize everything to hours per week
  #temp$CI05 <- temp$CI05/(temp$WeeksPerYr/12)
  #temp$CI95 <- temp$CI95/(temp$WeeksPerYr/12)
  #temp$CI50 <- temp$CI50/(temp$WeeksPerYr/12)
  #temp$MonthlyAvg <- temp$MonthlyAvg/(temp$WeeksPerYr/12)

  ggplot(temp,aes(x=Month,y=CI50,group=as.factor(Year)))+theme_bw()+ylab("Clinical Hours per Week")+
    geom_ribbon(aes(ymin=CI05,ymax=CI95,fill=as.factor(Year)),alpha=.3)+geom_line(aes(color=as.factor(Year)))+
    scale_x_continuous(breaks=seq(1,12))+theme(legend.title=element_blank())+
    scale_y_continuous(labels=comma) + facet_wrap(~Year)

  temp <- subset(temp,Year==2021)
  plot5 <- ggplot(temp,aes(x=Month,y=CI50/MonthlyAvg))+theme_bw()+ylab("Ratio of Clinical Time: Monthly / Average") +
    geom_hline(yintercept=1,color="black",linetype="dashed")+
    geom_ribbon(aes(ymin=CI05/MonthlyAvg,ymax=CI95/MonthlyAvg),alpha=.3,fill="darkblue")+geom_line(color="darkblue")+
    scale_x_continuous(breaks=seq(1,12))+theme(legend.title=element_blank())+
    scale_y_continuous(labels=scales::number_format(accuracy = 0.1),limits=c(.5,1.5))
  print(plot5)

  jpeg(paste("results/Seasonality effect on clinical time","_",date,"_",sc,".jpeg"), width = 5*4/5, height = 4*4/5, units = 'in', res = 700)
  print(plot5)
  dev.off()

}
