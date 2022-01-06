
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
library(RColorBrewer)


dirs = "C:/Users/brittanyha/OneDrive - Bill & Melinda Gates Foundation/Projects/HEP capacity projection/Model"
setwd(dirs)

DALYs <- as.data.frame(read_xlsx("R Model Inputs.xlsx",sheet="AggregateDALYs"))
TV <- as.data.frame(read_xlsx("R Model Inputs.xlsx",sheet="TaskValues"))

######################################################################################
######################################################################################
# Create a plot of DALY order depending on how choose to prioritize

#sort for DALYs per event and record data
DALYs <- DALYs[order(-DALYs$DALYs_per),]
DALYs$SortBy <- "DALYs per Episode"
DALYs$SortedOrder <- seq(1,nrow(DALYs))
DALYs$LabelText <- ""
plotdata <- DALYs

#sort for Attributed DALYs per event and record data
DALYs <- DALYs[order(-DALYs$Attributed_per),]
DALYs$SortBy <- "Attributed DALYs per Episode"
DALYs$SortedOrder <- seq(1,nrow(DALYs))
DALYs$LabelText <- ""
plotdata <- rbind(plotdata,DALYs)

#sort for Attributed DALYs per hour of time spent and record data
DALYs <- DALYs[order(-DALYs$DALYs_perhour),]
DALYs$SortBy <- "Attributed DALYs per Hour"
DALYs$SortedOrder <- seq(1,nrow(DALYs))
DALYs$LabelText <- DALYs$Health_area
plotdata <- rbind(plotdata,DALYs)

plotdata$SortBy <- factor(plotdata$SortBy,ordered=TRUE,levels=c("DALYs per Episode","Attributed DALYs per Episode","Attributed DALYs per Hour"))

ggplot(plotdata,aes(x=SortBy,y=SortedOrder,group=Health_area))+geom_point(aes(color=Health_area))+
  geom_line(aes(color=Health_area),size=1.2)+
  theme_bw()+xlab("")+ylab("Sorted rank")+scale_y_continuous(breaks=seq(1:nrow(DALYs)),trans="reverse")+
  geom_text(aes(x=3.1,y=SortedOrder,label=LabelText,color=Health_area),size=4,hjust=0)+theme(legend.position="none")
  



######################################################################################
######################################################################################
# Create a plot to show how many DALYs are left on the table if there is inadequate capacity

#merge DALYs into results data
DR$DALYs_epsd <- TV$DALYs_per[match(DR$ServiceCat,DALYs$Health_area)] 
DR$DALYs_attr <- TV$DALYs_attr[match(DR$ServiceCat,DALYs$Health_area)] 
DR$DALYs_epsd[is.na(DR$DALYs_epsd)] = 0
DR$DALYs_attr[is.na(DR$DALYs_attr)] = 0

#calculate DALYs per service category/run/trial
DR$DALYs_tot_epsd <- DR$DALYs_epsd * DR$Num_services / DR$NumContactsPer
DR$DALYs_tot_attr <- DR$DALYs_attr * DR$Num_services / DR$NumContactsPer
DR$DALYs_tot_epsd[is.infinite(DR$DALYs_tot_epsd)] = 0
DR$DALYs_tot_attr[is.infinite(DR$DALYs_tot_attr)] = 0
DR$DALYs_tot_epsd[is.na(DR$DALYs_tot_epsd)] = 0
DR$DALYs_tot_attr[is.na(DR$DALYs_tot_attr)] = 0

#calculate average DALYs per service category
ByRun_DALYs <- ddply(DR, .(Scenario_ID,Trial_num,Run_num,Year,ServiceCat,WeeksPerYr,HrsPerWeek),summarize,
                     DALYs_epsd = sum(DALYs_tot_epsd),
                     DALYs_attr = sum(DALYs_tot_attr),
                     timespent = sum(Service_time))
Mean_DALYs <- ddply(ByRun_DALYs, .(Scenario_ID,Year,ServiceCat,WeeksPerYr,HrsPerWeek),summarize,
                    MeanDALYs_epsd = mean(DALYs_epsd),
                    MeanDALYs_attr = mean(DALYs_attr),
                    MeanTimeSpent = mean(timespent))

Mean_DALYs$Order_epsd <- plotdata$SortedOrder[match(Mean_DALYs$ServiceCat,plotdata$Health_area[plotdata$SortBy=="DALYs per Episode"])]
Mean_DALYs$Order_attr <- plotdata$SortedOrder[match(Mean_DALYs$ServiceCat,plotdata$Health_area[plotdata$SortBy=="Attributed DALYs per Episode"])]
Mean_DALYs$Order_prhr <- plotdata$SortedOrder[match(Mean_DALYs$ServiceCat,plotdata$Health_area[plotdata$SortBy=="Attributed DALYs per Hour"])]

Mean_DALYs$MeanTimeSpent = Mean_DALYs$MeanTimeSpent / Mean_DALYs$WeeksPerYr

#subset to relevant scenarios
plotsub <- subset(Mean_DALYs,Scenario_ID=="ScenarioA" & !is.na(Order_epsd))       ###FIX THIS LATER

#Get the orders right for plotting
yr1data <- subset(plotsub,Year==min(plotsub$Year))
yr1data <- yr1data[order(-yr1data$Order_epsd),]
serviceslist_epsd <- yr1data$ServiceCat

yr1data <- yr1data[order(-yr1data$Order_attr),]
serviceslist_attr <- yr1data$ServiceCat

yr1data <- yr1data[order(-yr1data$Order_prhr),]
serviceslist_prhr <- yr1data$ServiceCat

#Set up color scheme
color_list_epsd <- colorRampPalette(brewer.pal(9, "Set1"))(15)
color_list_attr <- color_list_epsd[match(serviceslist_attr,serviceslist_epsd)]
color_list_prhr <- color_list_epsd[match(serviceslist_prhr,serviceslist_epsd)]


#Set up dataset to be used for the plot
plotsub$LabelText <- plotsub$ServiceCat
plotsub$LabelText[plotsub$Year < max(plotsub$Year)] = ""

plotsub$ServiceCat_epsd <- factor(plotsub$ServiceCat,ordered=TRUE,levels=serviceslist_epsd)

plotsub$LabelY <- 0
ycalc <- subset(plotsub,Year==max(plotsub$Year))
ycalc <- ycalc[order(ycalc$Order_epsd),]
ycalc$LabelY[1] = ycalc$MeanTimeSpent[1]/2
for(i in 2:nrow(ycalc)){
  ycalc$LabelY[i] = sum(ycalc$MeanTimeSpent[1:i-1])+ycalc$MeanTimeSpent[i]/2
}
plotsub$LabelY = ycalc$LabelY[match(plotsub$ServiceCat_epsd,ycalc$ServiceCat)]

ggplot(plotsub,aes(x=Year,y=MeanTimeSpent,fill=ServiceCat_epsd))+geom_bar(stat="identity",position="stack")+
  theme_bw()+scale_x_continuous(breaks=seq(min(plotsub$Year),max(plotsub$Year)),limits=c(min(plotsub$Year)-.5,max(plotsub$Year)+2))+
  geom_text(aes(x=max(Year+.6),y=LabelY,label=LabelText),color="darkgrey",size=4,hjust=0)+theme(legend.position="none") +
  ylab("Hours per Week") + labs(title="Time Required to Meet Clinical Needs, 10k Rural Pop",subtitle="Sorted Bottom-to-Top by DALYs per Episode")+
  scale_fill_manual(values = color_list_epsd)


plotsub$ServiceCat_attr <- factor(plotsub$ServiceCat,ordered=TRUE,levels=serviceslist_attr)

plotsub$LabelY <- 0
ycalc <- subset(plotsub,Year==max(plotsub$Year))
ycalc <- ycalc[order(ycalc$Order_attr),]
ycalc$LabelY[1] = ycalc$MeanTimeSpent[1]/2
for(i in 2:nrow(ycalc)){
  ycalc$LabelY[i] = sum(ycalc$MeanTimeSpent[1:i-1])+ycalc$MeanTimeSpent[i]/2
}
plotsub$LabelY = ycalc$LabelY[match(plotsub$ServiceCat_attr,ycalc$ServiceCat_attr)]

ggplot(plotsub,aes(x=Year,y=MeanTimeSpent,fill=ServiceCat_attr))+geom_bar(stat="identity",position="stack")+
  theme_bw()+scale_x_continuous(breaks=seq(min(plotsub$Year),max(plotsub$Year)),limits=c(min(plotsub$Year)-.5,max(plotsub$Year)+2))+
  geom_text(aes(x=max(Year+.6),y=LabelY,label=LabelText),color="darkgrey",size=4,hjust=0)+theme(legend.position="none") +
  ylab("Hours per Week") + labs(title="Time Required to Meet Clinical Needs, 10k Rural Pop",subtitle="Sorted Bottom-to-Top by Attributable DALYs per Episode")+
  scale_fill_manual(values = color_list_attr)


plotsub$ServiceCat_prhr <- factor(plotsub$ServiceCat,ordered=TRUE,levels=serviceslist_prhr)

plotsub$LabelY <- 0
ycalc <- subset(plotsub,Year==max(plotsub$Year))
ycalc <- ycalc[order(ycalc$Order_prhr),]
ycalc$LabelY[1] = ycalc$MeanTimeSpent[1]/2
for(i in 2:nrow(ycalc)){
  ycalc$LabelY[i] = sum(ycalc$MeanTimeSpent[1:i-1])+ycalc$MeanTimeSpent[i]/2
}
plotsub$LabelY = ycalc$LabelY[match(plotsub$ServiceCat_prhr,ycalc$ServiceCat_prhr)]

ggplot(plotsub,aes(x=Year,y=MeanTimeSpent,fill=ServiceCat_prhr))+geom_bar(stat="identity",position="stack")+
  theme_bw()+scale_x_continuous(breaks=seq(min(plotsub$Year),max(plotsub$Year)),limits=c(min(plotsub$Year)-.5,max(plotsub$Year)+2))+
  geom_text(aes(x=max(Year+.6),y=LabelY,label=LabelText),color="darkgrey",size=4,hjust=0)+theme(legend.position="none") +
  ylab("Hours per Week") + labs(title="Time Required to Meet Clinical Needs, 10k Rural Pop",subtitle="Sorted Bottom-to-Top by Attributable DALYs per Hour")+
  scale_fill_manual(values = color_list_prhr)









