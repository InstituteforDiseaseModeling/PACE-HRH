
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

READ_IN = FALSE #Read in .csv files for summary stats, yes/no
WRITE_OUT = TRUE #Write out .csv files for summary stats, yes/no
date <- "SNNPR0628_2022"
#BaselineScenario <- "ExpectedValue_NtlRatio" #ExpectedValue_RuralRatio
#NationalBaseline <- "ExpectedValue_NtlRatio"
#StableFertility <- "NoFertilityChange_NR" #NoFertilityChange_RuralRatio


scenarios <- read_xlsx("config/R Model Inputs.xlsx",sheet="Scenarios")
params <- read_xlsx("config/R Model Inputs.xlsx",sheet="StochasticParamaters")

GeoSelect <- scenarios$Geography_dontedit[i]

#load simulation data
remove(DR)
for (i in 1:nrow(scenarios)){
  print(paste("Loading scenario",i))
  scenario <- scenarios$UniqueID[i]
  tempupload <- read.csv(paste("results/results_",scenario,"_",GeoSelect,"_",date,".csv",sep=""))
  if(!exists('DR')){
    DR <- tempupload
  }else{
    DR <- rbind(DR,tempupload)
  }
  remove(tempupload)
}
DR <- as.data.frame(DR)

DR <- subset(DR,Trial_num < 101)
DR <- subset(DR,Service_time != 0)
DR <- subset(DR,Task_ID != "Test additional")
DR$Service_time <- DR$Service_time/60 #minutes to hours
DR <- subset(DR,Year<2036 & Year>2020) #Don't use year 2020 because it's an artifact. Subsetting to < 2036 to align with 15-year plan horizon.

#####################################################
# Attach scenario details to DR

#cleaning
scenarios$Scenario_ID <- scenarios$UniqueID
DR <- left_join(DR,scenarios,by=c("Scenario_ID"))
numscen <- nrow(scenarios)

for(sc in 1:numscen){
  print(paste("Attaching data to scenario",sc))
  scntext <- scenarios$sheet_TaskValues[sc]
  taskvalues <- read_xlsx("config/R Model Inputs.xlsx",sheet=scntext)
  taskvalues <- subset(taskvalues,Geography==GeoSelect)

  taskvalues$StartingRateInPop[is.na(taskvalues$StartingRateInPop)] = 0
  taskvalues$NumContactsAnnual[is.na(taskvalues$NumContactsAnnual)] = 0
  taskvalues$NumContactsPerUnit[is.na(taskvalues$NumContactsPerUnit)] = 0
  taskvalues$MinsPerContact[is.na(taskvalues$MinsPerContact)] = 0
  taskvalues$HoursPerWeek[is.na(taskvalues$HoursPerWeek)] = 0
  taskvalues$FTEratio[is.na(taskvalues$FTEratio)] = 0

  #Prep to merge together data sets
  taskvalues$Task_ID <- taskvalues$Indicator

  #Copy over scenario data
  DR$CommonName[DR$Scenario_ID==scenarios$UniqueID[sc]] <- taskvalues$CommonName[match(DR$Task_ID[DR$Scenario_ID==scenarios$UniqueID[sc]],taskvalues$Task_ID)]
  DR$ClinicalOrNon[DR$Scenario_ID==scenarios$UniqueID[sc]] <- taskvalues$ClinicalOrNon[match(DR$Task_ID[DR$Scenario_ID==scenarios$UniqueID[sc]],taskvalues$Task_ID)]
  DR$ClinicalCat[DR$Scenario_ID==scenarios$UniqueID[sc]] <- taskvalues$ClinicalCat[match(DR$Task_ID[DR$Scenario_ID==scenarios$UniqueID[sc]],taskvalues$Task_ID)]
  DR$ServiceCat[DR$Scenario_ID==scenarios$UniqueID[sc]] <- taskvalues$ServiceCat[match(DR$Task_ID[DR$Scenario_ID==scenarios$UniqueID[sc]],taskvalues$Task_ID)]
  DR$NumContactsPer[DR$Scenario_ID==scenarios$UniqueID[sc]] <- taskvalues$NumContactsPerUnit[match(DR$Task_ID[DR$Scenario_ID==scenarios$UniqueID[sc]],taskvalues$Task_ID)] + taskvalues$NumContactsAnnual[match(DR$Task_ID[DR$Scenario_ID==scenarios$UniqueID[sc]],taskvalues$Task_ID)]

}

DR$ServiceCat[DR$ClinicalOrNon!="Clinical"]=DR$CommonName[DR$ClinicalOrNon!="Clinical"]

############################################################################################################################################
############################################################################################################################################
#Remove excess rows due to the repetitive entry of overhead time

DR$RemoveCol <- 0

DR$RemoveCol[DR$DeliveryModel=="Merged" & DR$Task_ID=="Overhead_staff3"] = 1
DR$RemoveCol[DR$DeliveryModel=="Merged" & DR$Task_ID=="Overhead_staff4"] = 1
DR$RemoveCol[DR$DeliveryModel=="Merged" & DR$Task_ID=="Overhead_staff5"] = 1
DR$RemoveCol[DR$DeliveryModel=="Merged" & DR$Task_ID=="Overhead_staff6"] = 1

DR$RemoveCol[DR$DeliveryModel=="Basic" & DR$Task_ID=="Overhead_staff3" & DR$Year < 2025] = 1
DR$RemoveCol[DR$DeliveryModel=="Basic" & DR$Task_ID=="Overhead_staff4" & DR$Year < 2030] = 1
DR$RemoveCol[DR$DeliveryModel=="Basic" & DR$Task_ID=="Overhead_staff5"] = 1
DR$RemoveCol[DR$DeliveryModel=="Basic" & DR$Task_ID=="Overhead_staff6"] = 1

DR$RemoveCol[DR$DeliveryModel=="Comprehensive" & DR$Task_ID=="Overhead_staff3" & DR$Year < 2025] = 1
DR$RemoveCol[DR$DeliveryModel=="Comprehensive" & DR$Task_ID=="Overhead_staff4" & DR$Year < 2025] = 1

DR <- subset(DR,RemoveCol==0)

############################################################################################################################################
############################################################################################################################################

#Incorporate cadre allocation, by year * model

ComprehensiveCadre <- as.data.frame(read_xlsx("config/R Model Inputs.xlsx",sheet="Cadres_Comprehensive"))
BasicCadre <- as.data.frame(read_xlsx("config/R Model Inputs.xlsx",sheet="Cadres_Basic"))
MergedCadre <- as.data.frame(read_xlsx("config/R Model Inputs.xlsx",sheet="Cadres_Merged"))

#Prep melted format
ComprehensiveCadre_melt <- melt(ComprehensiveCadre,id.vars=c("Indicator","CommonName"),variable.name="Category",value.name="allocation")
BasicCadre_melt <- melt(BasicCadre,id.vars=c("Indicator","CommonName"),variable.name="Category",value.name="allocation")
MergedCadre_melt <- melt(MergedCadre,id.vars=c("Indicator","CommonName"),variable.name="Category",value.name="allocation")

ComprehensiveCadre_melt$Category <- as.character(ComprehensiveCadre_melt$Category)
BasicCadre_melt$Category <- as.character(BasicCadre_melt$Category)
MergedCadre_melt$Category <- as.character(MergedCadre_melt$Category)

#Extract year
ComprehensiveCadre_melt$Year <- as.numeric(substr(ComprehensiveCadre_melt$Category,1,2)) + 2000
BasicCadre_melt$Year <- as.numeric(substr(BasicCadre_melt$Category,1,2)) + 2000
MergedCadre_melt$Year <- as.numeric(substr(MergedCadre_melt$Category,1,2)) + 2000

#Extract Cadre
ComprehensiveCadre_melt$Cadre <- as.character(substr(ComprehensiveCadre_melt$Category,4,nchar(ComprehensiveCadre_melt$Category)))
BasicCadre_melt$Cadre <- as.character(substr(BasicCadre_melt$Category,4,nchar(BasicCadre_melt$Category)))
MergedCadre_melt$Cadre <- as.character(substr(MergedCadre_melt$Category,4,nchar(MergedCadre_melt$Category)))

#Remove totals because that's only for validation checks
ComprehensiveCadre_melt <- subset(ComprehensiveCadre_melt,Cadre!="TOTAL")
BasicCadre_melt <- subset(BasicCadre_melt,Cadre!="TOTAL")
MergedCadre_melt <- subset(MergedCadre_melt,Cadre!="TOTAL")
ComprehensiveCadre_melt <- subset(ComprehensiveCadre_melt,Cadre!="Unassigned")
BasicCadre_melt <- subset(BasicCadre_melt,Cadre!="Unassigned")
MergedCadre_melt <- subset(MergedCadre_melt,Cadre!="Unassigned")

#Wherever there was a blank should be a 0
ComprehensiveCadre_melt$allocation[is.na(ComprehensiveCadre_melt$allocation)] = 0
BasicCadre_melt$allocation[is.na(BasicCadre_melt$allocation)] = 0
MergedCadre_melt$allocation[is.na(MergedCadre_melt$allocation)] = 0

#Set up new dataframe for calculations
AllocCalcs <- DR[ , c("Task_ID", "Scenario_ID","DeliveryModel","Trial_num","Year","Service_time","WeeksPerYr")]

#Define match years for which delivery model will be in operation
AllocCalcs$MatchYear <- 2020
AllocCalcs$MatchYear[AllocCalcs$Year>2024] = 2025
AllocCalcs$MatchYear[AllocCalcs$Year>2029] = 2030
AllocCalcs$MatchYear[AllocCalcs$Year>2034] = 2035

#Set up for merge
ComprehensiveCadre_melt$DeliveryModel <- "Comprehensive"
BasicCadre_melt$DeliveryModel <- "Basic"
MergedCadre_melt$DeliveryModel <- "Merged"

CadreAllocations <- rbind(ComprehensiveCadre_melt,BasicCadre_melt)
CadreAllocations <- rbind(CadreAllocations,MergedCadre_melt)
remove(BasicCadre_melt)
remove(BasicCadre)
remove(ComprehensiveCadre_melt)
remove(ComprehensiveCadre)
remove(MergedCadre_melt)
remove(MergedCadre)

#Create lookup values
AllocCalcs$Lookup <- paste(AllocCalcs$Task_ID,AllocCalcs$DeliveryModel,AllocCalcs$MatchYear)
CadreAllocations$Lookup <- paste(CadreAllocations$Indicator,CadreAllocations$DeliveryModel,CadreAllocations$Year)

#Match and merge
MW <- subset(CadreAllocations,Cadre=="Midwife")
AllocCalcs$MW_AllocVal <- MW$allocation[match(AllocCalcs$Lookup,MW$Lookup)]
HO <- subset(CadreAllocations,Cadre=="HealthOfficer")
AllocCalcs$HO_AllocVal <- HO$allocation[match(AllocCalcs$Lookup,HO$Lookup)]
RN <- subset(CadreAllocations,Cadre=="Nurse")
AllocCalcs$RN_AllocVal <- RN$allocation[match(AllocCalcs$Lookup,RN$Lookup)]
EH <- subset(CadreAllocations,Cadre=="EnvironHealth")
AllocCalcs$EH_AllocVal <- EH$allocation[match(AllocCalcs$Lookup,EH$Lookup)]
FH <- subset(CadreAllocations,Cadre=="FamilyHealth")
AllocCalcs$FH_AllocVal <- FH$allocation[match(AllocCalcs$Lookup,FH$Lookup)]

HEW <- subset(CadreAllocations,Cadre=="HEW")
AllocCalcs$HEW_AllocVal <- HEW$allocation[match(AllocCalcs$Lookup,HEW$Lookup)]

UN <- subset(CadreAllocations,Cadre=="Unassigned")
AllocCalcs$UN_AllocVal <- UN$allocation[match(AllocCalcs$Lookup,UN$Lookup)]

#Calculate time by cadre
AllocCalcs$MW_Alloc = AllocCalcs$Service_time * AllocCalcs$MW_AllocVal / 100
AllocCalcs$HO_Alloc = AllocCalcs$Service_time * AllocCalcs$HO_AllocVal / 100
AllocCalcs$RN_Alloc = AllocCalcs$Service_time * AllocCalcs$RN_AllocVal / 100
AllocCalcs$EH_Alloc = AllocCalcs$Service_time * AllocCalcs$EH_AllocVal / 100
AllocCalcs$FH_Alloc = AllocCalcs$Service_time * AllocCalcs$FH_AllocVal / 100
AllocCalcs$UN_Alloc = AllocCalcs$Service_time * AllocCalcs$UN_AllocVal / 100
AllocCalcs$HEW_Alloc = AllocCalcs$Service_time * AllocCalcs$HEW_AllocVal / 100


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
  #sumsub <- subset(DR,Scenario_ID==BaselineScenario | Scenario_ID==StableFertility)
  sumsub <- DR

  #Definitely in use
  print("starting clinical cat")
  ByRun_ClinCat <- ddply(sumsub, .(Scenario_ID,Trial_num,Year,ClinicalCat,ClinicalOrNon,WeeksPerYr),summarize,TotHrs = sum(Service_time))
  Mean_ClinCat <- ddply(ByRun_ClinCat, .(Scenario_ID,Year,ClinicalCat,ClinicalOrNon,WeeksPerYr),summarize,MeanHrs = mean(TotHrs))

  print("starting clinical month")
  ByRun_ClinMonth <- ddply(subset(sumsub,ClinicalOrNon=="Clinical"),.(Scenario_ID,Trial_num,Year,Month,WeeksPerYr,HrsPerWeek),summarize,TotHrs=sum(Service_time))
  Stats_ClinMonth <- ddply(ByRun_ClinMonth,.(Scenario_ID,Year,Month,WeeksPerYr,HrsPerWeek),summarize,CI05 = quantile(TotHrs,probs=c(.05)),CI50 = mean(TotHrs),CI95 = quantile(TotHrs,probs=c(.95)))

  print("starting service cat")
  sumsub$ServiceCat[sumsub$ServiceCat=="Schisto MDA"] = "Campaign"
  sumsub$ServiceCat[sumsub$ServiceCat=="LLINs and IRS"] = "Campaign"
  sumsub$ServiceCat[sumsub$ServiceCat=="Vector control"] = "Campaign"
  ByRun_ServiceCat <- ddply(sumsub,.(Scenario_ID,Trial_num,Year,ServiceCat,ClinicalOrNon),summarize,TotHrs=sum(Service_time))
  Mean_ServiceCat <- ddply(ByRun_ServiceCat,.(Scenario_ID,Year,ServiceCat,ClinicalOrNon),summarize,MeanHrs=mean(TotHrs),CI95 = quantile(TotHrs,probs=c(.95)))

  print("starting totals")
  ByRun_Total <- ddply(sumsub, .(Scenario_ID,Trial_num,Year,WeeksPerYr,HrsPerWeek),summarize,TotHrs = sum(Service_time))
  Mean_Total <- ddply(ByRun_Total, .(Scenario_ID,Year,WeeksPerYr,HrsPerWeek),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))

  sumsub <- subset(sumsub, ClinicalOrNon=="Clinical")

  ByRun_TotClin <- ddply(sumsub,.(Scenario_ID,Trial_num,Year,WeeksPerYr,HrsPerWeek),summarize,AnnualHrs=sum(Service_time))
  Stats_TotClin <- ddply(ByRun_TotClin,.(Scenario_ID,Year,WeeksPerYr,HrsPerWeek),summarize,CI05 = quantile(AnnualHrs,probs=c(.05)),CI50 = mean(AnnualHrs),CI95 = quantile(AnnualHrs,probs=c(.95)))

  #Summarize time by cadre
  print("starting by cadre")
  ByRun_Alloc <- ddply(AllocCalcs, .(Scenario_ID,DeliveryModel,Trial_num,Year,WeeksPerYr),summarize,
                       MW_hrs=sum(MW_Alloc),HO_hrs=sum(HO_Alloc),RN_hrs=sum(RN_Alloc),EH_hrs=sum(EH_Alloc),FH_hrs=sum(FH_Alloc),UN_hrs=sum(UN_Alloc),HEW_hrs=sum(HEW_Alloc))
  ByRun_Alloc_melt <- melt(ByRun_Alloc,id.vars=c("Scenario_ID","DeliveryModel","Trial_num","Year","WeeksPerYr"),
                           variable.name="Cadre",value.name="AnnualHrs")
  ByRun_Alloc_melt <- subset(ByRun_Alloc_melt,!is.na(AnnualHrs))
  Mean_Alloc <- ddply(ByRun_Alloc_melt, .(Scenario_ID,DeliveryModel,Year,WeeksPerYr,Cadre),summarize,CI05=quantile(AnnualHrs,probs=c(.05)),CI50 = mean(AnnualHrs),CI95 = quantile(AnnualHrs,probs=c(.95)))


  }else{
  Mean_ServiceCat <- read.csv(paste("results/Mean_ServiceCat",GeoSelect,"_",date,".csv",sep=""))
  Stats_TotClin <- read.csv(paste("results/Stats_TotClin",GeoSelect,"_",date,".csv",sep=""))
  Mean_ClinCat <- read.csv(paste("results/Mean_ClinCat",GeoSelect,"_",date,".csv",sep=""))
  Mean_Total <- read.csv(paste("results/Mean_Total",GeoSelect,"_",date,".csv",sep=""))
  Stats_ClinMonth <- read.csv(paste("results/Stats_ClinMonth",GeoSelect,"_",date,".csv",sep=""))
  Mean_Alloc <- read.csv(paste("results/Mean_Alloc",GeoSelect,"_",date,".csv",sep=""))
  }

if(WRITE_OUT==TRUE){
  write.csv(Mean_ServiceCat,paste("results/Mean_ServiceCat",GeoSelect,"_",date,".csv",sep=""))
  write.csv(Stats_TotClin,paste("results/Stats_TotClin",GeoSelect,"_",date,".csv",sep=""))
  write.csv(Mean_ClinCat,paste("results/Mean_ClinCat",GeoSelect,"_",date,".csv",sep=""))
  write.csv(Mean_Total,paste("results/Mean_Total",GeoSelect,"_",date,".csv",sep=""))
  write.csv(Stats_ClinMonth,paste("results/Stats_ClinMonth",GeoSelect,"_",date,".csv",sep=""))
  write.csv(Mean_Alloc,paste("results/Mean_Alloc",GeoSelect,"_",date,".csv",sep=""))
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

popratio <- 115000000 / 2500
calculationscenario <- "ComprehensiveModel"

popratio * Stats_TotClin$CI50[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario]
popratio * Stats_TotClin$CI05[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario]
popratio * Stats_TotClin$CI95[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario] / Stats_TotClin$WeeksPerYr[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario] / Stats_TotClin$HrsPerWeek[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID==calculationscenario]

popratio * Mean_Total$MeanHrs[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario] / Mean_Total$WeeksPerYr[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario] / Mean_Total$HrsPerWeek[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario]
popratio * Mean_Total$CI05[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario] / Mean_Total$WeeksPerYr[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario] / Mean_Total$HrsPerWeek[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario]
popratio * Mean_Total$CI95[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario] / Mean_Total$WeeksPerYr[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario] / Mean_Total$HrsPerWeek[Mean_Total$Year==2035 & Mean_Total$Scenario_ID==calculationscenario]


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

maxyval <- max(Mean_Total$CI95/Mean_Total$WeeksPerYr)*1.05
i=1

for(sc in unique(DR$Scenario_ID)){

  temp <- subset(Mean_ClinCat,Scenario_ID==sc)
  weeksperyear = mean(DR$WeeksPerYr[DR$Scenario_ID==sc])
  hoursperweek = mean(DR$HrsPerWeek[DR$Scenario_ID==sc])
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
    geom_line(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,y=MeanHrs/WeeksPerYr),size=1.2)+
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

for(sc in unique(DR$Scenario_ID)){

  temp <- subset(Mean_Alloc,Scenario_ID==sc)
  weeksperyear = mean(DR$WeeksPerYr[DR$Scenario_ID==sc])
  hoursperweek = mean(DR$HrsPerWeek[DR$Scenario_ID==sc])
  temp$colorselect <- colorlist[match(temp$Cadre,namelist)]
  temp$rename <- renamelist[match(temp$Cadre,namelist)]

  plot8 <- ggplot()+
    geom_bar(data=temp,aes(x=Year,y=CI50/WeeksPerYr,group=Cadre),fill=temp$colorselect,color="darkgrey",stat="identity",alpha=.9)+
    geom_line(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,y=MeanHrs/WeeksPerYr),size=1.2)+
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

for(sc in unique(DR$Scenario_ID)){

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

for(sc in unique(DR$Scenario_ID)){
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

#fertility sensitivity analysis

# Stats_TotClin$FTE05 <- round(Stats_TotClin$CI05/Stats_TotClin$WeeksPerYr/Stats_TotClin$HrsPerWeek,2)
# Stats_TotClin$FTE50 <- round(Stats_TotClin$CI50/Stats_TotClin$WeeksPerYr/Stats_TotClin$HrsPerWeek,2)
# Stats_TotClin$FTE95 <- round(Stats_TotClin$CI95/Stats_TotClin$WeeksPerYr/Stats_TotClin$HrsPerWeek,2)
#
# #clinical FTE calcs
# #First/Last Year Total clinical FTE, Baseline/Stable fertility
# subset(Stats_TotClin,(Year==min(Stats_TotClin$Year) | Year==max(Stats_TotClin$Year)) & Scenario_ID==BaselineScenario)
# subset(Stats_TotClin,(Year==min(Stats_TotClin$Year) | Year==max(Stats_TotClin$Year)) & Scenario_ID==StableFertility)
#
# #Annual hours of clinical work, fertility sensitivity plot
# temp <- subset(Mean_ServiceCat,ClinicalOrNon=="Clinical")
# temp$alphavalue <- 0
# temp$alphavalue[temp$Scenario_ID==BaselineScenario] = 1
# temp$alphavalue[temp$Scenario_ID==StableFertility] = .6
# #temp$MeanHrs[temp$Scenario_ID==StableFertility] = temp$MeanHrs[temp$Scenario_ID==StableFertility]*1.2 #just for demo purposes to get a difference between scenarios
#
# #Box outline is stable fertility 95th percentile
# #Light green is stable fertility scenario
# #Dark green is continuing decline in fertility scenario (expected/baseline)
# plot4 <- ggplot(temp,aes(x=ServiceCat,y=MeanHrs,group=Year))+
#   geom_bar(data=subset(temp,Scenario_ID==StableFertility),aes(x=ServiceCat,y=CI95,group=Year),stat="identity",position="dodge",fill="grey",alpha=.65)+
#   geom_bar(alpha=temp$alphavalue,stat="identity",position="dodge",fill="darkslategray")+
# #  geom_line(data=subset(temp,Scenario_ID==StableFertility),aes(x=ServiceCat,y=CI95,group=Year),color="darkgrey")+
#   theme_bw()+
#   xlab("")+ylab("Annual Hours")+
#   theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5))+
#   scale_y_continuous(labels=comma)
# plot(plot4)
#
# jpeg(paste("results/Fertility Effects Over Time",date,".jpeg",sep=""), width = 5*4/5, height = 4*4/5, units = 'in', res = 700)
# print(plot4)
# dev.off()


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


for(sc in unique(DR$Scenario_ID)){

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



############################################################################################################################################

#Sensitivity components plot - impact of various contributors to capacity needs

# #Set up dataframe
# SC <- data.frame(sc_name=c("Baseline 2021","Seasonality","Pop growth","Decreases in U5 illness","Decreases in TB, HIV & malaria","Ongoing decrease fertility"))
# SC$TotalTime_mean <- 0
# SC$TotalTime_95 <- 0
# SC$TotalTime_05 <- 0
# SC$Scenario_diff <- 0
# SC$Base_value <- 0
# SC$Delta_bar <- 0
#
# #Calculate and save baseline values from year 1, before anything has changed.
# Baseline_byrun <- ddply(subset(DR,Year==2021 & Scenario_ID==BaselineScenario),.(Trial_num),summarize,TotHrs=sum(Service_time))
# Baseline_stats <- ddply(Baseline_byrun,.(),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))
#
# SC$TotalTime_mean[1] = Baseline_stats$MeanHrs[1]
# SC$TotalTime_95[1] = Baseline_stats$CI95[1]
# SC$TotalTime_05[1] = Baseline_stats$CI05[1]
#
# #Calculate and save baseline values from year 1, factoring in seasonality (select max month).
# Baseline_month_byrun <- ddply(subset(DR,Year==2021 & Scenario_ID==BaselineScenario),.(Trial_num,Month),summarize,TotHrs=sum(Service_time))
# Baseline_month_max <- ddply(Baseline_month_byrun,.(Trial_num),summarize,MaxMonthHrs=max(TotHrs))
# Baseline_month_stats <- ddply(Baseline_month_max,.(),summarize,AvgHighestMonth=mean(MaxMonthHrs), CI05= quantile(MaxMonthHrs,probs=c(.05)), CI95= quantile(MaxMonthHrs,probs=c(.95)))
#
# SC$TotalTime_mean[2] = Baseline_month_stats$AvgHighestMonth[1]*12
# SC$TotalTime_95[2] = Baseline_month_stats$CI95[1]*12
# SC$TotalTime_05[2] = Baseline_month_stats$CI05[1]*12
#
# #Calculate and save baseline values from the last year, factoring in seasonality (select max month).
# Baseline_month_byrun <- ddply(subset(DR,Year==2035 & Scenario_ID==BaselineScenario),.(Trial_num,Month),summarize,TotHrs=sum(Service_time))
# Baseline_month_max <- ddply(Baseline_month_byrun,.(Trial_num),summarize,MaxMonthHrs=max(TotHrs))
# Baseline_month_stats <- ddply(Baseline_month_max,.(),summarize,AvgHighestMonth=mean(MaxMonthHrs), CI05= quantile(MaxMonthHrs,probs=c(.05)), CI95= quantile(MaxMonthHrs,probs=c(.95)))
#
# SC$TotalTime_mean[6] = Baseline_month_stats$AvgHighestMonth[1]*12
# SC$TotalTime_95[6] = Baseline_month_stats$CI95[1]*12
# SC$TotalTime_05[6] = Baseline_month_stats$CI05[1]*12
#
# #Calculate and save values from the no-improvements scenario, basically the worst possible outcome, final year
# WorstCase_byrun <- ddply(subset(DR,Year==2035 & Scenario_ID=="p_Demographics_NR"),.(Trial_num),summarize,TotHrs=sum(Service_time))
# WorstCase_stats <- ddply(WorstCase_byrun,.(),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))
#
# SC$TotalTime_mean[3] = WorstCase_stats$MeanHrs[1]
# SC$TotalTime_95[3] = WorstCase_stats$CI95[1]
# SC$TotalTime_05[3] = WorstCase_stats$CI05[1]
#
# #Calculate and save values from the child-health-improvement only scenario, final year
# ChHealthImprv_byrun <- ddply(subset(DR,Year==2035 & Scenario_ID=="p_ChildHealthImp_NR"),.(Trial_num),summarize,TotHrs=sum(Service_time))
# ChHealthImprv_stats <- ddply(ChHealthImprv_byrun,.(),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))
#
# SC$TotalTime_mean[4] = ChHealthImprv_stats$MeanHrs[1]
# SC$TotalTime_95[4] = ChHealthImprv_stats$CI95[1]
# SC$TotalTime_05[4] = ChHealthImprv_stats$CI05[1]
#
# #Calculate and save values from the Child Improvement + TB/M/HIV improvement scenario, final year
# InfDisImprv_byrun <- ddply(subset(DR,Year==2035 & Scenario_ID=="NoFertilityChange_NR"),.(Trial_num),summarize,TotHrs=sum(Service_time))
# InfDisImprv_stats <- ddply(InfDisImprv_byrun,.(),summarize,MeanHrs = mean(TotHrs), CI05 = quantile(TotHrs,probs=c(.05)), CI95 = quantile(TotHrs,probs=c(.95)))
#
# SC$TotalTime_mean[5] = InfDisImprv_stats$MeanHrs[1]
# SC$TotalTime_95[5] = InfDisImprv_stats$CI95[1]
# SC$TotalTime_05[5] = InfDisImprv_stats$CI05[1]
#
#
#
# for (rw in 1:nrow(SC)){
#   if(rw==1){
#     SC$Scenario_diff[1] = SC$TotalTime_mean[1]
#     SC$Delta_bar[1] = SC$TotalTime_mean[1]
#   }else{
#     SC$Scenario_diff[rw] = SC$TotalTime_mean[rw]-SC$TotalTime_mean[rw-1]
#     if(SC$Scenario_diff[rw]>0){
#       SC$Base_value[rw] = SC$TotalTime_mean[rw-1]
#     }else{
#       SC$Base_value[rw] = SC$TotalTime_mean[rw]
#     }
#     SC$Delta_bar[rw] = abs(SC$Scenario_diff[rw])
#   }
# }
#
# SC$PosNeg <- "positive"
# SC$PosNeg[SC$Scenario_diff < 0] = "negative"
# SCM <- melt(SC,id=c("sc_name","PosNeg"))
# SCM <- subset(SCM,variable=="Base_value" | variable=="Delta_bar")
# SCM$color <- ""
# SCM$alpha <- 1
# SCM$color[SCM$PosNeg=="negative"] = "darkgreen"
# SCM$color[SCM$PosNeg=="positive"] = "tomato4"
# SCM$color[SCM$variable=="Base_value" | SCM$sc_name==SC$sc_name[1]] = "grey32"
# SCM$alpha[SCM$sc_name!=SC$sc_name[1] & SCM$variable=="Base_value"] = .35
# SCM$sc_name = factor(SCM$sc_name,levels=SC$sc_name,ordered=TRUE)
#
# plot6 <- ggplot()+
#   geom_bar(data=SCM,aes(x=sc_name,y=value),stat="identity",alpha=SCM$alpha,fill=SCM$color)+
#   geom_errorbar(data=SC,aes(x=sc_name,ymin=TotalTime_05, ymax=TotalTime_95), colour="black", width=.2)+
#   geom_point(data=SC,aes(x=sc_name,y=TotalTime_mean), colour="black")+
#   theme_bw()+xlab("")+ylab("Annual Hours (Total)")+
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
#   scale_y_continuous(labels=comma,breaks=seq(0,trunc(max(SC$TotalTime_95))+10,1500))
# #  geom_line(data=SC,aes(x=seq(1,nrow(SC)),y=TotalTime_95),size=1)
# print(plot6)
#
# jpeg(paste("results/Scenario sensitivity analysis",date,".jpeg",sep=""), width = 4.8, height = 3.2, units = 'in', res = 700)
# print(plot6)
# dev.off()

#beep()
