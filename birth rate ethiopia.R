# R version 
# R studio 1.3.959

# Package: survey
# Version: 4.1.1
# 
# Package: readstata13
# Version: 0.10.0
#
# Package: foreign
# Version: 0.8.81
#
# Package: ggplot2
# Version: 3.3.2
#
# Package: reshape2
# Version: 1.4.4


library(foreign)
library(readstata13)
library(survey)
library(ggplot2)
library(reshape2)


# DHS directories
country = "Ethiopia"
dirs = "C:/Users/bhagedorn/OneDrive - Institute for Disease Modeling/Projects/HEP capacity projection/Data/ETH DHS"
setwd(dirs)

# The file extraction process is to use the zipped (compressed) household and
# birth recode files provided by DHS, using the .dta file format

files = data.frame(filename = c("ETBR41FL.DTA","ETBR51FL.DTA","ETBR61FL.DTA","ETBR71FL.DTA","ETBR81FL.DTA",
                                "ETHR41FL.DTA","ETHR51FL.DTA","ETHR61FL.DTA","ETHR71FL.DTA","ETHR81FL.DTA"),
                   recode = c(  "birth","birth","birth","birth","birth",
                                "household","household","household","household","household"),
                   year_label = c(2000,2005,2011,2016,2019,
                                  2000,2005,2011,2016,2019),
                   index = c(1,2,3,4,5,
                             1,2,3,4,5))


# revised analysis
# step 1 - months before first interview date

results = vector("list", max(files$index))
for(i in 1:max(files$index)){
  # pick up birth and household data
  tmp.file.birth = file.path(subset(files, index == i & recode == "birth")$filename)
  tmp.file.hh = file.path(subset(files, index == i & recode == "household")$filename)
  
  # read from temporary location
  tmp.svy.birth = read.dta13(tmp.file.birth,convert.factors = TRUE,generate.factors = TRUE)
  #file.remove(tmp.file.birth)
  
  tmp.svy.hh = read.dta13(tmp.file.hh,convert.factors = TRUE,generate.factors = TRUE)
  #file.remove(tmp.file.hh)

  # calculate the birth month relative to the month of the first interview;
  # larger number imply further in the past
  tmp.svy.birth$relative_birth_month = min(tmp.svy.birth$v008) - tmp.svy.birth$b3
  temp_birth_month = table(tmp.svy.birth$relative_birth_month)

  # recode month of birth to missing if not within 3*12 = 36 months from month of first interview
  # b1 is month of birth, 1-12
  # b10 is completeness of information, where b10 == 1 implies no imputation
  
  tmp.svy.birth$b1_recode = as.factor(ifelse(tmp.svy.birth$relative_birth_month %in% 1:36,tmp.svy.birth$b1,NA))

  tmp.svy.birth$b1_recode_no_imputation = as.factor(ifelse(tmp.svy.birth$relative_birth_month %in% 1:36,
                   ifelse(as.integer(tmp.svy.birth$b10) == 1,tmp.svy.birth$b1,NA),NA))
  
  ## approach
  #  1. proportion by month for last three years by stratum (differences are testable)
  #  2. yearly birth rate by month (testable)
  #  3. yearly * proportion (probably testable, but no point)
  
  # summarize births by household and month
  # create dummy coding of births
  for(k in 1:12){
    tmp.svy.birth[,paste("month",k,sep="")] = ifelse(tmp.svy.birth$relative_birth_month %in% 1:36 &
                                                       tmp.svy.birth$b1 == k, 1, 0)
  }
  
  tmp.svy.birth.summary = aggregate(cbind(month1,month2,month3,month4,month5,month6,month7,month8,month9,month10,month11,month12)~
                                      v001+v002, data = tmp.svy.birth, sum)
  
  names(tmp.svy.birth.summary)[1:2] = c("hv001","hv002")
  
  # merge with the hh file
  tmp.svy.hh = merge(tmp.svy.hh, tmp.svy.birth.summary, by = c("hv001","hv002"), all.x = T)
  
  # replace NA with 0
  for(k in 1:12){
    tmp.svy.hh[,paste("month",k,sep="")] = ifelse(
      is.na(tmp.svy.hh[,paste("month",k,sep="")]),0,
      tmp.svy.hh[,paste("month",k,sep="")]
    )
  }
  
  # part 1: proportion per month with standard errors     v024=region. v025=urban/rural

  tmp.svy.design.birth = svydesign(id = ~v001, strata=~v024+v025, weights = tmp.svy.birth$v005/mean(tmp.svy.birth$v005), data=tmp.svy.birth)
  #tmp.svy.design.birth = svydesign(id = ~v001, strata=~v025, weights = tmp.svy.birth$v005/mean(tmp.svy.birth$v005), data=tmp.svy.birth)
  
  birth_month = svyby(~b1_recode, by = ~v024, FUN = svymean, design=tmp.svy.design.birth, na.rm = T)
  #birth_month = svyby(~b1_recode, by = ~v025, FUN = svymean, design=tmp.svy.design.birth, na.rm = T)
  
  birth_month_no_imputation = svyby(~b1_recode_no_imputation, by = ~v024, FUN = svymean, design=tmp.svy.design.birth, na.rm = T)
  #birth_month_no_imputation = svyby(~b1_recode_no_imputation, by = ~v025, FUN = svymean, design=tmp.svy.design.birth, na.rm = T)
  
  names(birth_month) = c("region",paste("month",1:12,sep=""),paste("month",1:12,"_se",sep=""))

  birth_month[,1] = tolower(as.character(birth_month[,1]))
  
  names(birth_month_no_imputation) = c("region",paste("month",1:12,sep=""),paste("month",1:12,"_se",sep=""))
  birth_month_no_imputation[,1] = tolower(as.character(birth_month_no_imputation[,1]))
  
  birth_month_combined = svyby(~b1_recode, by = ~I("Country"), FUN = svymean, design=tmp.svy.design.birth, na.rm = T)
  birth_month_no_imputation_combined = svyby(~b1_recode_no_imputation, by = ~I("Country"), FUN = svymean, design=tmp.svy.design.birth, na.rm = T)
  
  names(birth_month_combined) = c("region",paste("month",1:12,sep=""),paste("month",1:12,"_se",sep=""))
  names(birth_month_no_imputation_combined) = c("region",paste("month",1:12,sep=""),paste("month",1:12,"_se",sep=""))

  birth_month = rbind(birth_month, birth_month_combined)
  birth_month_no_imputation = rbind(birth_month_no_imputation, birth_month_no_imputation_combined)
  
  # part 2: birth rate
  tmp.svy.hh$total_births = with(tmp.svy.hh,month1+month2+month3+month4+month5+month6+month7+month8+month9+month10+month11+month12)
  
  tmp.svy.design.hh = svydesign(id = ~hv001, strata=~hv024+hv025, weights = tmp.svy.hh$hv005/mean(tmp.svy.hh$hv005), data=tmp.svy.hh)
  #tmp.svy.design.hh = svydesign(id = ~hv001, strata=~hv025, weights = tmp.svy.hh$hv005/mean(tmp.svy.hh$hv005), data=tmp.svy.hh)
  
  birth_rate_year = svyby(~I(total_births/3),
                          #by=~hv024, denominator=~I(hv013/1000),
                          by=~hv025, denominator=~I(hv013/1000),
                           design=tmp.svy.design.hh,FUN=svyratio)
  names(birth_rate_year) = c("region","yearly_birthrate_per_1000", "yearly_birthrate_per_1000_se")
  birth_rate_year$region = tolower(as.character(birth_rate_year$region))

  birth_rate_year_combined = svyby(~I(total_births/3),
                                    by=~I("Country"), denominator=~I(hv013/1000),
                                    design=tmp.svy.design.hh,FUN=svyratio)
  names(birth_rate_year_combined) = c("region","yearly_birthrate_per_1000", "yearly_birthrate_per_1000_se")

  birth_rate_year = rbind(birth_rate_year, birth_rate_year_combined)
  
  # part3: birth rate by month
  
  birth_rate_month = svyby(~I(month1/3)+I(month2/3)+I(month3/3)+I(month4/3)+
                                      I(month5/3)+I(month6/3)+I(month7/3)+I(month8/3)+
                                      I(month9/3)+I(month10/3)+I(month11/3)+I(month12/3),
                                      #by=~hv024, denominator=~I(hv013/1000),
                                      by=~hv025, denominator=~I(hv013/1000),
                                    design=tmp.svy.design.hh,FUN=svyratio)
  
  names(birth_rate_month) = c("region",paste("month",1:12,"_birthrate",sep=""),
                                       paste("month",1:12,"_birthrate_se",sep=""))
  
  birth_rate_month$region = tolower(birth_rate_month$region)
  
  birth_rate_month_combined = svyby(~I(month1/3)+I(month2/3)+I(month3/3)+I(month4/3)+
                                      I(month5/3)+I(month6/3)+I(month7/3)+I(month8/3)+
                                      I(month9/3)+I(month10/3)+I(month11/3)+I(month12/3),
                                    by=~I("Country"), denominator=~I(hv013/1000),
                                    design=tmp.svy.design.hh,FUN=svyratio)
  
  names(birth_rate_month_combined) = names(birth_rate_month)
  
  birth_rate_month = rbind(birth_rate_month, birth_rate_month_combined)
  
  # simple adjustment using no imputation: multiple birth rate and age distribution
  
  birth_rate_month_no_imputation = merge(birth_rate_year[,1:2],
                                         birth_month_no_imputation[1:13],by="region")
  
  for(k in 1:12){
    birth_rate_month_no_imputation[,paste("month",k,"_birthrate",sep="")] = 
      birth_rate_month_no_imputation[,paste("month",k,sep="")] * 
      birth_rate_month_no_imputation$yearly_birthrate_per_1000
  }
  
  # just the relevant columns; SE's not computed
  birth_rate_month_no_imputation = birth_rate_month_no_imputation[,names(birth_rate_month)[1:13]]

  # wrap it up
  
  results[[i]] = list(birth_month = birth_month,
                      birth_month_no_imputation = birth_month_no_imputation,
                      birth_rate_year = birth_rate_year,
                      birth_rate_month = birth_rate_month,
                      birth_rate_month_no_imputation = birth_rate_month_no_imputation)
  
  cat(i,"\n")
 
}

year_label = unique(files[,c("index","year_label")])
year_label = year_label$year_label[order(year_label$index)]

# write to csv
birth_month_all = do.call("rbind",lapply(1:length(results),function(m) cbind(year=year_label[m],results[[m]]$birth_month)))
birth_month_no_imputation_all = do.call("rbind",lapply(1:length(results),function(m) cbind(year=year_label[m],results[[m]]$birth_month_no_imputation)))
birth_rate_year_all = do.call("rbind",lapply(1:length(results),function(m) cbind(year=year_label[m],results[[m]]$birth_rate_year)))
birth_rate_month_all = do.call("rbind",lapply(1:length(results),function(m) cbind(year=year_label[m],results[[m]]$birth_rate_month)))
birth_rate_month_no_imputation_all = do.call("rbind",lapply(1:length(results),function(m) cbind(year=year_label[m],results[[m]]$birth_rate_month_no_imputation)))

today = format(Sys.Date(),"%Y%m%d")

write.csv(birth_month_all, file=paste(country,"_birth_month_",today,".csv",sep=""),row.names = F)
write.csv(birth_month_no_imputation_all, file=paste(country,"_birth_month_no_imputation_",today,".csv",sep=""),row.names = F)
write.csv(birth_rate_year_all, file=paste(country,"_birth_rate_year_",today,".csv",sep=""),row.names = F)
write.csv(birth_rate_month_all, file=paste(country,"_birth_rate_month_",today,".csv",sep=""),row.names = F)
write.csv(birth_rate_month_no_imputation_all, file=paste(country,"_birth_rate_month_no_imputation_",today,".csv",sep=""),row.names = F)


# make plots

ggplot(birth_rate_year_all,aes(x=year,y=yearly_birthrate_per_1000,group=region,color=region))+theme_bw()+
  geom_ribbon(aes(ymin=yearly_birthrate_per_1000-yearly_birthrate_per_1000_se*3.92/2,ymax=yearly_birthrate_per_1000+yearly_birthrate_per_1000_se*3.92/2,fill=region),alpha=.3)+
  geom_point()+geom_line()+labs(title="Birth Rate per 1,000 women w/ 95% CI")


brmonth_val <- melt(birth_rate_month_all[,1:14],id.vars=c("year","region"),value.name="birthrate")
brmonth_val$month <- substring(brmonth_val$variable,1,regexpr("_",brmonth_val$variable)-1)
brmonth_val$month <- substring(brmonth_val$month,regexpr("h",brmonth_val$month)+1,nchar(brmonth_val$month))
brmonth_val$variable <- NULL

brmonth_se <- birth_rate_month_all
brmonth_se[,3:14]<-NULL
brmonth_se <- melt(brmonth_se,id.vars=c("year","region"),value.name="stderror")
brmonth_se$month <- substring(brmonth_se$variable,1,regexpr("_",brmonth_se$variable)-1)
brmonth_se$month <- substring(brmonth_se$month,regexpr("h",brmonth_se$month)+1,nchar(brmonth_se$month))
brmonth_se$variable <- NULL

brmonth <- merge(brmonth_val,brmonth_se,by=c("year","region","month"))
brmonth$month <- as.numeric(brmonth$month)

pdf("Seasonality of births visualizations.pdf",width=11,height=8.5)

ggplot(brmonth,aes(x=month,y=birthrate,group=year,color=year))+geom_point()+geom_line()+theme_bw()+facet_wrap(~region)+ylim(0,4.1)+xlim(1,12)
ggplot(brmonth,aes(x=month,y=birthrate,group=region,color=region))+geom_point()+geom_line()+theme_bw()+facet_wrap(~year)+ylim(0,4.1)+xlim(1,12)
ggplot(brmonth,aes(x=year,y=birthrate,group=region,color=region))+geom_point()+geom_line()+theme_bw()+facet_wrap(~month)+ylim(0,4.1)+xlim(2000,2020)

ggplot(brmonth,aes(x=month,y=birthrate,group=region,color=region))+theme_bw()+facet_wrap(~year)+ylim(0,4.5)+xlim(1,12)+
  geom_ribbon(aes(ymin=birthrate-stderror*3.92/2,ymax=birthrate+stderror*3.92/2,fill=region),alpha=.3)+
  geom_point()+geom_line()+labs(title="Birth Rate per 1,000 women w/ 95% CI")

ggplot(subset(brmonth,region=="Country"),aes(x=month,y=birthrate,group=region,color=region))+theme_bw()+facet_wrap(~year)+ylim(0,4.5)+xlim(1,12)+
  geom_ribbon(aes(ymin=birthrate-stderror*3.92/2,ymax=birthrate+stderror*3.92/2,fill=region),alpha=.3)+
  geom_point()+geom_line()+labs(title="Birth Rate per 1,000 women w/ 95% CI")

ggplot(subset(brmonth,region=="rural"),aes(x=month,y=birthrate,group=region,color=region))+theme_bw()+facet_wrap(~year)+ylim(0,4.5)+xlim(1,12)+
  geom_ribbon(aes(ymin=birthrate-stderror*3.92/2,ymax=birthrate+stderror*3.92/2,fill=region),alpha=.3)+
  geom_point()+geom_line()+labs(title="Birth Rate per 1,000 women w/ 95% CI")

ggplot(brmonth,aes(x=year,y=birthrate,group=region,color=region))+theme_bw()+facet_wrap(~month)+ylim(0,4.5)+xlim(2000,2020)+
  geom_ribbon(aes(ymin=birthrate-stderror*3.92/2,ymax=birthrate+stderror*3.92/2,fill=region),alpha=.3)+
  geom_point()+geom_line()+labs(title="Birth Rate per 1,000 women w/ 95% CI")

ggplot(subset(brmonth,region=="Country"),aes(x=year,y=birthrate,group=region,color=region))+theme_bw()+facet_wrap(~month)+ylim(0,4.5)+xlim(2000,2020)+
  geom_ribbon(aes(ymin=birthrate-stderror*3.92/2,ymax=birthrate+stderror*3.92/2,fill=region),alpha=.3)+
  geom_point()+geom_line()+labs(title="Birth Rate per 1,000 women w/ 95% CI")

ggplot(subset(brmonth,region=="rural"),aes(x=year,y=birthrate,group=region,color=region))+theme_bw()+facet_wrap(~month)+ylim(0,4.5)+xlim(2000,2020)+
  geom_ribbon(aes(ymin=birthrate-stderror*3.92/2,ymax=birthrate+stderror*3.92/2,fill=region),alpha=.3)+
  geom_point()+geom_line()+labs(title="Birth Rate per 1,000 women w/ 95% CI")

dev.off()

# run t-tests

# Paired t-test by month - i.e. is January different than February?
numrows <- 11+10+9+8+7+6+5+4+3+2+1
tres <- data.frame(ID=1:numrows)
rownum <- 1

for(i in 1:11){
  for(j in (i+1):12){
    tc <- t.test(subset(brmonth,region=="Country" & month==i)$birthrate,subset(brmonth,region=="Country" & month==j)$birthrate,paired=TRUE,alternative = "two.sided")
    tr <- t.test(subset(brmonth,region=="rural" & month==i)$birthrate,subset(brmonth,region=="rural" & month==j)$birthrate,paired=TRUE,alternative = "two.sided")
    tres$i[rownum] = i
    tres$j[rownum] = j
    tres$c_pval[rownum] = tc$p.value
    tres$r_pval[rownum] = tr$p.value
    tres$c_95L[rownum] = tc$conf.int[1]
    tres$c_95H[rownum] = tc$conf.int[2]
    tres$r_95L[rownum] = tr$conf.int[1]
    tres$r_95H[rownum] = tr$conf.int[2]
    tres$c_estimate[rownum] = tc$estimate
    tres$r_estimate[rownum] = tr$estimate
    tres$c_stderr[rownum] = tc$stderr
    tres$r_stderr[rownum] = tr$stderr
    
    rownum = rownum + 1
  }
}

tres$c_binary <- 0
tres$r_binary <- 0
tres$c_binary[tres$c_pval < .05] = 1
tres$r_binary[tres$r_pval < .05] = 1

write.csv(tres,"T test results by month.csv")

pdf("T tests by month visualization.pdf",width=11,height=8.5)

ggplot(tres,aes(x=i,y=j,fill=c_binary))+geom_tile()+theme_bw()+scale_y_continuous(breaks=seq(1,12,1))+scale_x_continuous(breaks=seq(1,12,1))+labs(title="National level")
ggplot(tres,aes(x=i,y=j,fill=r_binary))+geom_tile()+theme_bw()+scale_y_continuous(breaks=seq(1,12,1))+scale_x_continuous(breaks=seq(1,12,1))+labs(title="Rural only")
ggplot(tres,aes(x=i,y=j,fill=c_pval))+geom_tile()+theme_bw()+scale_y_continuous(breaks=seq(1,12,1))+scale_x_continuous(breaks=seq(1,12,1))+labs(title="National level")
ggplot(tres,aes(x=i,y=j,fill=r_pval))+geom_tile()+theme_bw()+scale_y_continuous(breaks=seq(1,12,1))+scale_x_continuous(breaks=seq(1,12,1))+labs(title="Rural only")

dev.off()