
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

date <- Sys.Date()-5

regions <- c("Tigray","Amhara","Oromia","Somali","SNNPR","Dire Dawa")
#regions <- c("Tigray", "Affar", "Amhara", "Oromia", "Somali", "Benishangul Gumuz", "SNNPR", "Gambela", "Harari", "Addis Ababa", "Dire Dawa")

remove(Mean_Alloc, Mean_ClinCat, Mean_ServiceCat, Mean_Total, Stats_ClinMonth, Stats_TotClin )
for (GeoName in regions){
  print(paste("Loading region",GeoName))
  Mean_ServiceCat_temp <- read.csv(paste("results/Mean_ServiceCat_",GeoName,"_",date,".csv",sep=""))
  Stats_TotClin_temp <- read.csv(paste("results/Stats_TotClin_",GeoName,"_",date,".csv",sep=""))
  Mean_ClinCat_temp <- read.csv(paste("results/Mean_ClinCat_",GeoName,"_",date,".csv",sep=""))
  Mean_Total_temp <- read.csv(paste("results/Mean_Total_",GeoName,"_",date,".csv",sep=""))
  Stats_ClinMonth_temp <- read.csv(paste("results/Stats_ClinMonth_",GeoName,"_",date,".csv",sep=""))
  Mean_Alloc_temp <- read.csv(paste("results/Mean_Alloc_",GeoName,"_",date,".csv",sep=""))

  if(!exists('Mean_ServiceCat')){
    Mean_ServiceCat <- Mean_ServiceCat_temp
  }else{
    Mean_ServiceCat <- rbind(Mean_ServiceCat,Mean_ServiceCat_temp)
  }
  if(!exists('Stats_TotClin')){
    Stats_TotClin <- Stats_TotClin_temp
  }else{
    Stats_TotClin <- rbind(Stats_TotClin,Stats_TotClin_temp)
  }
  if(!exists('Mean_ClinCat')){
    Mean_ClinCat <- Mean_ClinCat_temp
  }else{
    Mean_ClinCat <- rbind(Mean_ClinCat,Mean_ClinCat_temp)
  }
  if(!exists('Mean_Total')){
    Mean_Total <- Mean_Total_temp
  }else{
    Mean_Total <- rbind(Mean_Total,Mean_Total_temp)
  }
  if(!exists('Stats_ClinMonth')){
    Stats_ClinMonth <- Stats_ClinMonth_temp
  }else{
    Stats_ClinMonth <- rbind(Stats_ClinMonth,Stats_ClinMonth_temp)
  }
  if(!exists('Mean_Alloc')){
    Mean_Alloc <-  Mean_Alloc_temp
  }else{
    Mean_Alloc <- rbind( Mean_Alloc, Mean_Alloc_temp)
  }
  remove(Mean_ServiceCat_temp, Stats_TotClin_temp, Mean_ClinCat_temp, Mean_Total_temp, Stats_ClinMonth_temp, Mean_Alloc_temp)
}




#Check overhead staff 1 - 6 in comprehensive model
overheadstaff <- c("Overhead staff 1","Overhead staff 2","Overhead staff 3","Overhead staff 4","Overhead staff 5","Overhead staff 6")
DS <- Mean_ServiceCat %>%
  subset(DeliveryModel=="Comprehensive" & ServiceCat %in% overheadstaff)


#Total clinical hours: o_PopGrowth FALSE + o_Fertility_decr TRUE
ClinHrsTot <- Stats_TotClin %>%
  subset(o_PopGrowth==FALSE & o_Fertility_decr==TRUE)

plot <- ggplot(data=ClinHrsTot, aes(x=Year, y=CI50/WeeksPerYr.x, group=Geography_dontedit, color=Geography_dontedit)) +
  theme_bw()+
  geom_point()+
  facet_grid(rows=vars(ordered(DeliveryModel, levels=c("Comprehensive", "Basic", "Merged"))), scale="free_y")+
  labs(x="Year",y="Clinical Hours per Week per 5,000 Pop", title="PopGrowth FALSE Fertility_decr TRUE")
print(plot)

#Total clinical hours scaled by starting year: o_PopGrowth FALSE + o_Fertility_decr TRUE
ClinHrsGrowth <- Stats_TotClin %>%
  subset(o_PopGrowth==FALSE & o_Fertility_decr==TRUE) %>%
  group_by(Geography_dontedit, DeliveryModel) %>%
  mutate(StartLevel = CI50[Year==2021])

plot <- ggplot(data=ClinHrsGrowth, aes(x=Year, y=CI50/StartLevel, group=Geography_dontedit, color=Geography_dontedit)) +
  theme_bw()+
  geom_point()+
  facet_grid(rows=vars(ordered(DeliveryModel, levels=c("Comprehensive", "Basic", "Merged"))), scale="free_y")+
  labs(x="Year",y="Clinical Hours Growth", title="PopGrowth FALSE Fertility_decr TRUE")
print(plot)

#Total hours: o_PopGrowth FALSE + o_Fertility_decr TRUE
TotalHrs <- Mean_Total %>%
  subset(o_PopGrowth==FALSE & o_Fertility_decr==TRUE) %>%
  mutate(DupOverheadHrs = 216.6*2*(Year<2030)) %>%
  mutate(CI05=CI05-DupOverheadHrs, CI25=CI25-DupOverheadHrs, MeanHrs=MeanHrs-DupOverheadHrs, CI75=CI75-DupOverheadHrs, CI95=CI95-DupOverheadHrs)

plot <- ggplot(data=TotalHrs, aes(x=Year, y=MeanHrs/WeeksPerYr.x, group=Geography_dontedit, color=Geography_dontedit)) +
  theme_bw()+
  geom_point()+
  facet_grid(rows=vars(ordered(DeliveryModel, levels=c("Comprehensive", "Basic", "Merged"))), scale="free_y")+
  labs(x="Year",y="Total Hours per Week per 5,000 Pop", title="PopGrowth FALSE Fertility_decr TRUE")

print(plot)

#Total hours scaled by starting year: o_PopGrowth FALSE + o_Fertility_decr TRUE
TotalHrsGrowth <- Mean_Total %>%
  subset(o_PopGrowth==FALSE & o_Fertility_decr==TRUE) %>%
  mutate(DupOverheadHrs = 216.6*2*(Year<2030)) %>%
  mutate(CI05=CI05-DupOverheadHrs, CI25=CI25-DupOverheadHrs, MeanHrs=MeanHrs-DupOverheadHrs, CI75=CI75-DupOverheadHrs, CI95=CI95-DupOverheadHrs) %>%
  group_by(Geography_dontedit, DeliveryModel) %>%
  mutate(StartLevel = MeanHrs[Year==2021])

plot <- ggplot(data=TotalHrsGrowth, aes(x=Year, y=MeanHrs/StartLevel, group=Geography_dontedit, color=Geography_dontedit)) +
  theme_bw()+
  geom_point()+
  facet_grid(rows=vars(ordered(DeliveryModel, levels=c("Comprehensive", "Basic", "Merged"))), scale="free_y")+
  labs(x="Year",y="Total Hours Growth", title="PopGrowth FALSE Fertility_decr TRUE")
print(plot)

#Total hours scaled by starting year: o_PopGrowth FALSE + o_Fertility_decr FALSE
TotalHrsGrowth <- Mean_Total %>%
  subset(o_PopGrowth==FALSE & o_Fertility_decr==FALSE) %>%
  mutate(DupOverheadHrs = 216.6*2*(Year<2030)) %>%
  mutate(CI05=CI05-DupOverheadHrs, CI25=CI25-DupOverheadHrs, MeanHrs=MeanHrs-DupOverheadHrs, CI75=CI75-DupOverheadHrs, CI95=CI95-DupOverheadHrs) %>%
  group_by(Geography_dontedit, DeliveryModel) %>%
  mutate(StartLevel = MeanHrs[Year==2021])

plot <- ggplot(data=TotalHrsGrowth, aes(x=Year, y=MeanHrs/StartLevel, group=Geography_dontedit, color=Geography_dontedit)) +
  theme_bw()+
  geom_point()+
  facet_grid(rows=vars(ordered(DeliveryModel, levels=c("Comprehensive", "Basic", "Merged"))), scale="free_y")+
  labs(x="Year",y="Total Hours Growth", title="PopGrowth FALSE Fertility_decr FALSE")
print(plot)

#Total hours scaled by starting year: o_PopGrowth TRUE + o_Fertility_decr TRUE
TotalHrsGrowth <- Mean_Total %>%
  subset(o_PopGrowth==TRUE & o_Fertility_decr==TRUE) %>%
  mutate(DupOverheadHrs = 216.6*2*(Year<2030)) %>%
  mutate(CI05=CI05-DupOverheadHrs, CI25=CI25-DupOverheadHrs, MeanHrs=MeanHrs-DupOverheadHrs, CI75=CI75-DupOverheadHrs, CI95=CI95-DupOverheadHrs) %>%
  group_by(Geography_dontedit, DeliveryModel) %>%
  mutate(StartLevel = MeanHrs[Year==2021])

plot <- ggplot(data=TotalHrsGrowth, aes(x=Year, y=MeanHrs/StartLevel, group=Geography_dontedit, color=Geography_dontedit)) +
  theme_bw()+
  geom_point()+
  facet_grid(rows=vars(ordered(DeliveryModel, levels=c("Comprehensive", "Basic", "Merged"))), scale="free_y")+
  labs(x="Year",y="Total Hours Growth", title="PopGrowth TRUE Fertility_decr TRUE")
print(plot)

# Mean_Total$LookUp <- paste(Mean_Total$Scenario_ID,Mean_Total$Geography_dontedit)
# FirstYearSub <- subset(Mean_Total,Year==min(Mean_Total$Year))
# Mean_Total$RatioTo2020 <- Mean_Total$MeanHrs/FirstYearSub$MeanHrs[match(Mean_Total$LookUp,FirstYearSub$LookUp)]
#
# ggplot(Mean_Total,aes(x=Year,y=RatioTo2020,group=Geography_dontedit,color=Geography_dontedit))+geom_point()+theme_bw()+
#   facet_grid(rows=vars(ordered(DeliveryModel, levels=c("Comprehensive", "Basic", "Merged"))), scale="free_y")+
#   facet_wrap(~Scenario_ID,scales="free_y")


#Total clinical hours: baseline; o_PopGrowth FALSE + o_Fertility_decr TRUE; o_PopGrowth FALSE + o_Fertility_decr FALSE
years = c(2021, 2025, 2030, 2035)
#specify path to save PDF to
destination = paste("results/total clinical hours ","_",date,".pdf",sep="")
#open PDF
pdf(file=destination)
for (mdl in c("Comprehensive","Basic", "Merged")){

  DS <- Stats_TotClin %>%
    subset(Year %in% years & DeliveryModel== mdl)

  plot <- ggplot(data=DS, aes(x=Geography_dontedit, y=CI50/WeeksPerYr.x,group=Scenario_ID,fill=Scenario_ID)) +
    geom_bar(stat="identity",position = "dodge") +
    labs(x="Compre",y="Hours per Week per 5,000 Pop",title=mdl) +
    guides(x=guide_axis(n.dodge=2)) +
    facet_grid(rows = vars(Year))

  print(plot)
  #turn off PDF plotting
}
dev.off()


#serivce category with the highest clinical hours
unique(Mean_ServiceCat_Comprehensive$ServiceCat[Mean_ServiceCat_Comprehensive$ClinicalOrNon=="Clinical"])
unique(Mean_ServiceCat_Basic$ServiceCat[Mean_ServiceCat_Basic$ClinicalOrNon=="Clinical"])
unique(Mean_ServiceCat_Merged$ServiceCat[Mean_ServiceCat_Merged$ClinicalOrNon=="Clinical"])

#Calculate differences in clinical hours across regions, among models, over years
Stats_TotClin$CI50[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID=="ComprehensiveModel_B"]/Stats_TotClin$WeeksPerYr.x[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID=="ComprehensiveModel_B"]
Stats_TotClin$Geography_dontedit[Stats_TotClin$Year==2035 & Stats_TotClin$Scenario_ID=="ComprehensiveModel_B"]

Stats_TotClin_byyearandmodel <- Stats_TotClin %>%
  mutate(WeeklyHrs = CI50/WeeksPerYr.x) %>%
  subset(Year==2035) %>%
  subset(DeliveryModel == "Comprehensive")




# age pyramid
ServiceCatHrs <- Mean_ServiceCat_Comprehensive %>%
  subset(ClinicalOrNon=="Clinical" & Year == 2021 & Geography_dontedit=="SNNPR") %>%
  group_by(Scenario_ID, Geography_dontedit, ServiceCat)


# maxyval <- max(Mean_Total$CI95/Mean_Total$WeeksPerYr)*1.05
# i=1
#
# for(sc in unique(Mean_clinCat$Scenario_ID)){
#
#   temp <- subset(Mean_ClinCat,Scenario_ID==sc)
#   weeksperyear = scenarios$WeeksPerYr[scenarios$UniqueID==sc]
#   hoursperweek = scenarios$HrsPerWeek[scenarios$UniqueID==sc]
#   temp$Category <- paste(temp$ClinicalCat)
#   temp$Category[temp$ClinicalOrNon=="Clinical"] = paste("Clinical -",temp$Category[temp$ClinicalOrNon=="Clinical"])
#   temp$Category[temp$ClinicalOrNon!="Clinical"] = temp$ClinicalOrNon[temp$ClinicalOrNon!="Clinical"]
#   temp$Category <- factor(temp$Category,ordered=TRUE,levels=unique(temp$Category))
#   temp$Alpha <- 1
#   temp$Alpha[temp$ClinicalOrNon!="Clinical"] = .3
#   temp <- subset(temp,Year<2036)
#
#   plottitle <- scenarios$UniqueID[i]
#
#   plot1 <- ggplot()+
#     geom_bar(data=temp,aes(x=Year,y=MeanHrs/WeeksPerYr,fill=Category),stat="identity",alpha=.9)+
#     geom_line(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,y=MeanHrs/WeeksPerYr),size=1.2)+
#     geom_point(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,y=MeanHrs/WeeksPerYr))+
#     geom_errorbar(data=subset(Mean_Total,Scenario_ID==sc),aes(x=Year,ymin=CI05/WeeksPerYr, ymax=CI95/WeeksPerYr), colour="black", width=.3)+
#     ylim(0,maxyval)+
#     theme_bw()+
#     scale_x_continuous(breaks = seq(2021,2035))+
#     theme(legend.title=element_blank(),axis.text.x = element_text(angle=-90, vjust = .5, hjust=1))+
#     scale_fill_viridis_d()+
#     ylab("Hours per Week per 5,000 Pop") + xlab("") + labs(title = plottitle)+
#     facet_wrap(~adminName)
#
#   print(plot1)
#
#
#   #jpeg(paste("results/Weekly workload by Type","_",GeoSelect,"_",date,"_",sc,".jpeg",sep=""), width = 6, height = 4, units = 'in', res = 700)
#   #print(plot1)
#   #dev.off()
#
#   i=i+1
#
# }



scenarios <- read_xlsx("config/model_inputs.xlsx",sheet="Scenarios")
weeksperyear = scenarios$WeeksPerYr[1]
hoursperweek = scenarios$HrsPerWeek[1]

Services <- c("Family planning", "Pregnancy")
#Services <- unique(Mean_ServiceCat$ServiceCat[Mean_ServiceCat$ClinicalOrNon=="Clinical"])
deliverymodels <- factor(unique(Mean_ServiceCat$DeliveryModel), ordered=TRUE, levels = c("Comprehensive", "Basic", "Merged"))


#hours per week on clinical services by service category and region
for (yr in c(2021, 2025, 2030, 2035)){
  #specify path to save PDF to
  destination = paste("results/Weekly workload by service cat","_",yr,"_",date,".pdf",sep="")
  #open PDF
  pdf(file=destination)
  for (each in Services) {
    DS <- Mean_ServiceCat %>%
      subset(ServiceCat==each & Year==yr)
    plot <- ggplot(data=DS, aes(x=adminName, y=MeanHrs/weeksperyear)) +
      geom_col() +
      labs(x=yr,y="Hours per Week per 5,000 Pop",title=each) +
      guides(x=guide_axis(n.dodge=2)) +
      facet_grid(rows = vars(Scenario_ID))
    print(plot)
  }
  #turn off PDF plotting
  dev.off()
}


#workload relative to national average by service category and region

for (yr in c(2021, 2035)){
  #specify path to save PDF to
  destination = paste("results/relative workload by service cat","_",yr,"_",date,".pdf",sep="")
  #open PDF
  pdf(file=destination)
  for (each in Services) {
    DS <- Mean_ServiceCat %>%
      subset(ServiceCat==each & Year==yr) %>%
      group_by(Scenario_ID) %>%
      mutate(RelativeWorkload=(MeanHrs/mean(MeanHrs)-1)) %>%
      ungroup()
    NatMean <- DS %>%
      group_by(Scenario_ID) %>%
      summarize(NatMean=mean(MeanHrs))
    plot <- ggplot(data=DS, aes(x=reorder(Geography_dontedit,RelativeWorkload), y=RelativeWorkload)) +
      geom_bar(stat="identity") +
      labs(x=yr,y="Workload per 5,000 Pop compared to national average (% diffenrece)",title=paste(each,"Basic: ",round(NatMean$NatMean[1]/mean(DS$WeeksPerYr),1),", Comp: ",round(NatMean$NatMean[2]/mean(DS$WeeksPerYr),1),", Merged: ",round(NatMean$NatMean[3]/mean(DS$WeeksPerYr),1))) +
      guides(x=guide_axis(n.dodge=2)) +
      facet_grid(rows = vars(ordered(Scenario_ID, levels=c("ComprehensiveModel", "BasicModel", "MergedModel"))))
    print(plot)
  }
  #turn off PDF plotting
  dev.off()
  } #fix reorder, all three scenarios are lumped together; make Scenario_ID an ordered factor (Comprehensive, Basic, Merged)

#workload relative to national average by service category and region; model facet by year

for (yr in c(2021, 2025, 2030, 2035)){
  #specify path to save PDF to
  destination = paste("results/relative workload by service cat","_",yr,"_",date,".pdf",sep="")
  #open PDF
  pdf(file=destination)
  for (each in Services) {
    DS <- Mean_ServiceCat %>%
      subset(ServiceCat==each & Year==yr) %>%
      group_by(Scenario_ID) %>%
      mutate(RelativeWorkload=(MeanHrs/mean(MeanHrs)-1)) %>%
      ungroup()
    NatMean <- DS %>%
      group_by(Scenario_ID) %>%
      summarize(NatMean=mean(MeanHrs))
    plot <- ggplot(data=DS, aes(x=reorder(adminName,RelativeWorkload), y=RelativeWorkload)) +
      geom_bar(stat="identity") +
      labs(x=yr,y="Workload per 5,000 Pop compared to national average (% diffenrece)",title=paste(each,"Basic: ",round(NatMean$NatMean[1]/weeksperyear,1),", Comp: ",round(NatMean$NatMean[2]/weeksperyear,1),", Merged: ",round(NatMean$NatMean[3]/weeksperyear,1))) +
      guides(x=guide_axis(n.dodge=2)) +
      facet_grid(rows = vars(Scenario_ID))
    print(plot)
  }
  #turn off PDF plotting
  dev.off()
} #fix reorder, all three scenarios are lumped together; make Scenario_ID an ordered factor (Comprehensive, Basic, Merged)


# Mean_total plots
urban <- c("Addis Ababa", "Dire Dawa", "Harari")
pastoral <- c("Affar", "Somali", "Gambela","Benishangul Gumuz")
Mean_Total$DevLevel[Mean_Total$adminName %in% urban] = "Urban"
Mean_Total$DevLevel[Mean_Total$adminName %in% pastoral] = "Pastoral"
Mean_Total$DevLevel[is.na(Mean_Total$DevLevel)] = "Agra"

ggplot(Mean_Total,aes(x=Year,y=MeanHrs/WeeksPerYr,group=Scenario_ID,color=Scenario_ID))+geom_line()+theme_bw()+
  facet_wrap(~adminName,scales="free_y")
ggplot(Mean_Total,aes(x=Year,y=MeanHrs/WeeksPerYr,group=adminName,linetype=adminName))+geom_line()+theme_bw()+
  facet_wrap(~Scenario_ID,scales="free_y")






