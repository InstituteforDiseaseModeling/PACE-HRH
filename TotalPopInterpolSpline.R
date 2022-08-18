library(tidyverse)
library(tidyr)
library(readxl)

dirs = "C:/Users/ruiha/OneDrive - Bill & Melinda Gates Foundation/Documents/GitHub/Ethiopia-HEP-Capacity-Analysis"
setwd(dirs)

#read in UN estimated Ethiopia national and regional populations
admin0pops <- read_xlsx("ethiopia_national_pop_2020.xlsx",sheet="Sheet1")
admin1pops <- read_xlsx("ethiopia_worldpop_2020_admin1_both.xlsx",sheet="unadj_constrained")

#assign adminCode to region names
NAME_1 <- c("Ethiopia","Tigray","Afar","Amhara","Oromia","Somali","Benshangul-Gumaz","SouthernNations,NationalitiesandPeoples","GambelaPeoples","HarariPeople","AddisAbeba","DireDawa")
adminCode <- c("ET","ET01","ET02","ET03","ET04","ET05","ET06","ET07","ET12","ET13","ET14","ET15")
df_admin <- data.frame(NAME_1,adminCode)

#append national to regional populations
df <- admin1pops %>% 
  merge(y=df_admin,by="NAME_1", all.x=TRUE) %>% 
  select(adminCode,NAME_1,sex,age_group,year,population_estimate) %>% 
  rbind(admin0pops)

#Sum regional pops for under 1 and 1-4 respectively to calculate proportion of under 1 pop in 0-4 age group, apply proportion to national 0-4 pop
under1 <- subset(df, age_group=='<1 year' | age_group=='1-4') %>% 
  group_by(year, sex, age_group) %>% 
  summarize(region_sum = sum(population_estimate)) %>% 
  group_by(sex) %>% 
  mutate(popweight = region_sum/sum(region_sum)) %>% 
  ungroup() %>% 
  rename(subgroup = age_group) %>% 
  mutate(age_group = "0-4") %>% 
  select(-region_sum)

ETunder1 <- under1 %>% 
  merge(y=admin0pops, by=c("year", "sex", "age_group"), all.x=TRUE) %>% 
  mutate(population_estimate = population_estimate*popweight) %>% 
  select(adminCode, NAME_1, sex, subgroup, year, population_estimate) %>% 
  rename(age_group = subgroup)

#apply national weights of age groups in 80+ populations to regional 80+ populations; weights differ by sex
weights80 <- admin0pops %>% 
  separate(col=age_group, into=c("age_l","age_h"), sep='-') %>% 
  subset(as.numeric(age_l)>=80 | age_l=="100+") %>% 
  group_by(adminCode,sex) %>% 
  mutate(sumpop=sum(population_estimate), popweight=population_estimate/sumpop, age_group="80+") %>% 
  ungroup() %>% 
  select(age_group, sex, year, age_h, age_l, popweight)
pops80 <- merge(x=weights80, y=df, by=c("age_group","sex","year"), all.x=TRUE) %>% 
  rename(population_sum = population_estimate) %>% 
  mutate(population_estimate = round(population_sum*popweight)) %>% 
  select(adminCode, NAME_1, sex, age_l, age_h, year, population_estimate)
pops80$age_l[pops80$age_l=='100+'] = '100'
pops80$age_h[is.na(pops80$age_h)] = '100'

#stack all age groups from all regions, calculated values included
temp <- df %>% 
  rbind(ETunder1) %>% 
  subset(age_group != "0-4" & age_group != "80+")
temp$age_group[temp$age_group=="<1 year"] = "0-0"
temp$age_group[temp$age_group=="100+"] = "100-100"
pops <- temp %>% 
  separate(col=age_group, into=c("age_l","age_h"), sep='-') %>% 
  rbind(pops80) %>% 
  mutate(age_l=as.numeric(age_l), age_h=as.numeric(age_h)) %>% 
  arrange(adminCode, sex, age_l)

pops$age_m = round((pops$age_h + pops$age_l)/2)
pops$age_span = (pops$age_h - pops$age_l)+1
#adjut 80+ age groups by mortality rate
mortality_adjustment = tibble(age_l = c(80, 80, 85, 85, 90, 90, 95, 95),
                              age_h = c(84, 84, 89, 89, 94, 94, 99, 99),
                              sex = c('Female', 'Male', 'Female', 'Male', 'Female', 'Male', 'Female', 'Male'),
                              mort_adj = c(0.95, 0.94, 0.92, 0.92, 0.75, 0.71, 0.58, 0.4))
pops <- pops %>% 
  merge(mortality_adjustment, by = c("age_l", "age_h", "sex"), all.x = TRUE)
pops$mort_adj[is.na(pops$mort_adj)] = 1
pops$pop_mean = (pops$population_estimate/pops$age_span)*pops$mort_adj


ages <- seq(0, 100, by = 1)
#Interpolate female population at each age for each region
remove(Pop_female)
for (i in 1:length(df_admin$adminCode)){
  print(paste("Interpolate for ",df_admin$NAME_1[i]))
  GeoSelect <- df_admin$adminCode[i]
  df_geoselect <- subset(pops, pops$adminCode == GeoSelect & pops$sex == "Female")
  plot(df_geoselect$age_m, df_geoselect$pop_mean)
  mod_l = approx(df_geoselect$age_m, df_geoselect$pop_mean, xout = ages)
  mod_s = splines::interpSpline(df_geoselect$age_m, df_geoselect$pop_mean)
  temp <- tibble(Age = mod_l$x, pop_linear = mod_l$y, pop_spline = predict(mod_s, ages)$y) %>% 
    mutate(adminCode = GeoSelect, adminNameAsUsed = df_admin$NAME_1[i])
  plot <- temp %>%
    select(!contains("admin")) %>% 
    pivot_longer(cols = !Age) %>%
    ggplot(aes(x = Age, y = value, col = name)) + ggtitle(GeoSelect) + geom_line() + geom_point()
  print(plot)
  if(!exists('Pop_female')){
    Pop_female <- temp
  }else{
    Pop_female <- rbind(Pop_female,temp)
  }
  remove(temp)
}

Pop_female$Age[Pop_female$Age==0] = "<1"
write.csv(Pop_female,paste("Pop_female_2020",".csv",sep=""))

#Interpolate male population at each age for each region
remove(Pop_male)
for (i in 1:length(df_admin$adminCode)){
  print(paste("Interpolate for ",df_admin$NAME_1[i]))
  GeoSelect <- df_admin$adminCode[i]
  df_geoselect <- subset(pops, pops$adminCode == GeoSelect & pops$sex == "Male")
  plot(df_geoselect$age_m, df_geoselect$pop_mean)
  mod_l = approx(df_geoselect$age_m, df_geoselect$pop_mean, xout = ages)
  mod_s = splines::interpSpline(df_geoselect$age_m, df_geoselect$pop_mean)
  temp <- tibble(Age = mod_l$x, pop_linear = mod_l$y, pop_spline = predict(mod_s, ages)$y) %>% 
    mutate(adminCode = GeoSelect, adminNameAsUsed = df_admin$NAME_1[i])
  plot <- temp %>%
    select(!contains("admin")) %>% 
    pivot_longer(cols = !Age) %>%
    ggplot(aes(x = Age, y = value, col = name)) + ggtitle(GeoSelect) + geom_line() + geom_point()
  print(plot)  
  if(!exists('Pop_male')){
    Pop_male <- temp
  }else{
    Pop_male <- rbind(Pop_male,temp)
  }
  remove(temp)
}

Pop_male$Age[Pop_male$Age==0] = "<1"
write.csv(Pop_male,paste("Pop_male_2020",".csv",sep=""))
