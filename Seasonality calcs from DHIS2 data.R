
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

dirs = "C:/Users/brittanyha/OneDrive - Bill & Melinda Gates Foundation/Projects/HEP capacity projection/Data"
setwd(dirs)

ds <- read.csv("ForHEP_Capacity.csv")
ds$year <- as.integer(substr(ds$gdate,1,4))
ds$month <- as.integer(substr(ds$gdate,6,7))

pdf("../Outputs/Seasonality DHIS2 plots.pdf",width=8,height=6)

ANCbyzone <- ddply(ds,.(region,zone,year,month),summarize,numservices=sum(anc1),numreporting=n())
for(rgn in unique(ANCbyzone$region)){
  ANCcounts <- subset(ANCbyzone,region==rgn & numservices>0)
  p1 <-  ggplot(ANCcounts,aes(x=month,y=numservices,color=as.factor(year)))+geom_line()+theme_bw()+facet_wrap(~zone)+
    labs(title=paste(rgn,"ANC"))+scale_x_continuous(breaks=seq(1,12))+  theme(legend.title = element_blank())
  plot(p1)
}

SBAbyzone <- ddply(ds,.(region,zone,year,month),summarize,numservices=sum(sba),numreporting=n())
for(rgn in unique(SBAbyzone$region)){
  SBAcounts <- subset(SBAbyzone,region==rgn & numservices>0)
  p2 <- ggplot(SBAcounts,aes(x=month,y=numservices,color=as.factor(year)))+geom_line()+theme_bw()+facet_wrap(~zone)+
    labs(title=paste(rgn,"SBA"))+scale_x_continuous(breaks=seq(1,12))+  theme(legend.title = element_blank())
  plot(p2)
}

Malariabyzone <- ddply(ds,.(region,zone,year,month),summarize,numservices=sum(malaria),numreporting=n())
for(rgn in unique(SBAbyzone$region)){
  Malariacounts <- subset(Malariabyzone,region==rgn & numservices>0)
  p3 <- ggplot(Malariacounts,aes(x=month,y=numservices,color=as.factor(year)))+geom_line()+theme_bw()+facet_wrap(~zone)+
    labs(title=paste(rgn,"All Malaria"))+scale_x_continuous(breaks=seq(1,12))+  theme(legend.title = element_blank())
  plot(p3)
}

#SBAreporting <- ddply(SBAbyzone,.(region,year,month),summarize,avgreport=mean(numreporting),minreport=min(numreporting),
#                      maxreport=max(numreporting),q25=quantile(numreporting,c(.25)),q75=quantile(numreporting,c(.75)))
#SBAreportingmelt <- melt(SBAreporting,id.vars=c("region","year","month"))
#SBAreportingmelt$YrMo <- paste(SBAreportingmelt$year,SBAreportingmelt$month)

#ggplot(SBAreportingmelt,aes(x=YrMo,y=value,group=YrMo))+geom_point(aes(color=variable))+geom_line()+
#  facet_wrap(~region,scales="free")+theme_bw()+ theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
#  scale_y_continuous(breaks=pretty_breaks())

ANCbyregion <- ddply(ds,.(region,year,month),summarize,numservices=sum(anc1))
SBAbyregion <- ddply(ds,.(region,year,month),summarize,numservices=sum(sba))
malariabyregion <- ddply(ds,.(region,year,month),summarize,numservices=sum(malaria))
severemalariabyregion <- ddply(ds,.(region,year,month),summarize,numservices=sum(sever_mal))
TBbyregion <- ddply(subset(ds,!is.na(tb)),.(region,year,month),summarize,numservices=sum(tb))
PneumoU5byregion <- ddply(ds,.(region,year,month),summarize,numservices=sum(U5_Pneumonia_Rx))
COVIDlikebyregion <- ddply(ds,.(region,year,month),summarize,numservices=sum(covidlike))

plot_by_region<-function(servicecount,title){
  ggplot(servicecount,aes(x=month,y=numservices,color=as.factor(year)))+labs(title=title)+xlab("Month")+ylab("Number of services")+
    geom_line()+theme_bw()+facet_wrap(~region)+scale_x_continuous(breaks=seq(1,12))+
    theme(legend.title = element_blank())
}

servicecount <- subset(ANCbyregion,numservices>0)
title <- "ANC"
plot_by_region(servicecount,title)

servicecount <- subset(SBAbyregion,numservices>0)
title <- "SBA"
plot_by_region(servicecount,title)

servicecount <- subset(malariabyregion,numservices>0)
title <- "malaria all"
plot_by_region(servicecount,title)

servicecount <- subset(severemalariabyregion,numservices>0)
title <- "malaria severe"
plot_by_region(servicecount,title)

servicecount <- subset(TBbyregion,numservices>0)
title <- "Tuberculosis"
plot_by_region(servicecount,title)

servicecount <- subset(PneumoU5byregion,numservices>0)
title <- "Pneumonia dx U5"
plot_by_region(servicecount,title)

servicecount <- subset(COVIDlikebyregion,numservices>0)
title <- "COVID-like"
plot_by_region(servicecount,title)

dev.off()


###########################################

urbanrural <- as.data.frame(read_xlsx("Ethiopian population classification.xlsx",sheet="ruralurban"))
ds$urbanrural <- urbanrural$ru_classification[match(ds$zone,urbanrural$zone)]
ds$lookup <- paste(ds$zone,ds$year,sep="")

anc_average <- ddply(ds,.(region,zone,year),summarize,totalservices=sum(anc1),nummonths=n())
sba_average <- ddply(ds,.(region,zone,year),summarize,totalservices=sum(sba),nummonths=n())
mal_average <- ddply(ds,.(region,zone,year),summarize,totalservices=sum(malaria),nummonths=n())
tb_average <- ddply(ds,.(region,zone,year),summarize,totalservices=sum(tb),nummonths=n())
pu5_average <- ddply(ds,.(region,zone,year),summarize,totalservices=sum(U5_Pneumonia_Rx),nummonths=n())

anc_average <- subset(anc_average,nummonths>9)
sba_average <- subset(sba_average,nummonths>9)
mal_average <- subset(mal_average,nummonths>9)
tb_average <- subset(tb_average,nummonths>9)
pu5_average <- subset(pu5_average,nummonths>9)

anc_average$average <- anc_average$totalservices / anc_average$nummonths
sba_average$average <- sba_average$totalservices / sba_average$nummonths
mal_average$average <- mal_average$totalservices / mal_average$nummonths
tb_average$average <- tb_average$totalservices / tb_average$nummonths
pu5_average$average <- pu5_average$totalservices / pu5_average$nummonths

anc_average$lookup <- paste(anc_average$zone,anc_average$year,sep="")
sba_average$lookup <- paste(sba_average$zone,sba_average$year,sep="")
mal_average$lookup <- paste(mal_average$zone,mal_average$year,sep="")
tb_average$lookup <- paste(tb_average$zone,tb_average$year,sep="")
pu5_average$lookup <- paste(pu5_average$zone,pu5_average$year,sep="")

ds$anc_ratio <- ds$anc1 / match(ds$lookup,anc_average$lookup)
ds$sba_ratio <- ds$sba / match(ds$lookup,sba_average$lookup)
ds$mal_ratio <- ds$malaria / match(ds$lookup,mal_average$lookup)
ds$tb_ratio <- ds$tb / match(ds$lookup,tb_average$lookup)
ds$pu5_ratio <- ds$U5_Pneumonia_Rx / match(ds$lookup,pu5_average$lookup)

anc_regression <- lm(anc_ratio~urbanrural+as.factor(month)+region,data=ds) #R2=.26. month not significant.

sba_regression <- lm(sba_ratio~urbanrural+as.factor(month)+region,data=ds) #R2=.31. month not significant.

mal_regression <- lm(mal_ratio~urbanrural+as.factor(month)+region,data=ds) #R2=.13. month is significant.

#Could try a spline? Could try a lagged month included? Could try at the regional level instead of zone?






