library(readxl)
library(plyr)
library(dplyr)
library(stringr)
library(reshape2)


dirs = "C:/Users/brittanyha/OneDrive - Bill & Melinda Gates Foundation/Projects/HEP capacity projection/Model"
setwd(dirs)

#parameters

numyears <- 15
numruns <- 50
#numtrials <- 1

pop_variance <- .1
birthrate_sd <- .1
mortrate_sd <- .1

#Read in input sheets

startingpop <- read_xlsx("R Model Inputs.xlsx",sheet="TotalPop")
poprates <- read_xlsx("R Model Inputs.xlsx",sheet="PopValues")

# _p = parameter. _s = stochastic. _variance = width of uniform distribution around mean value

#################################################
# Generate stochastic starting population pyramid

# Total population
totpop_p <- sum(startingpop$Male) + sum(startingpop$Female)
totpop_s <- runif(numruns,min=totpop_p*(1-pop_variance),max=totpop_p*(1+pop_variance))

# Proportion of total population, female
proportionf_p <- sum(startingpop$Female)/totpop_p
proportionf_s <- runif(numruns,min=proportionf_p*(1-pop_variance),max=proportionf_p*(1+pop_variance))

# Female pyramid 
pyramidf_p <- data.frame(proportion_p=startingpop$Female/(totpop_p*proportionf_p))
pyramidf_s <- as.data.frame(sapply(pyramidf_p,rep.int,times=numruns)) #Baseline values in first column (starting parameters)
pyramidf_s$multiplier_s <- runif(nrow(pyramidf_s),min=(1-pop_variance),max=(1+pop_variance)) #Stochastic noise to distribution proportion
pyramidf_s$proportion_s <- pyramidf_s$proportion_p*pyramidf_s$multiplier_s #multiply baseline by noise
pyramidf_s$run_num = 0
for(i in 1:numruns){
  tempsum = sum(pyramidf_s$proportion_s[((i-1)*101+1):(i*101)])
  pyramidf_s$proportion_s[((i-1)*101+1):(i*101)] = pyramidf_s$proportion_s[((i-1)*101+1):(i*101)]/tempsum #normalize proportions to 100% of pop.
  pyramidf_s$run_num[((i-1)*101+1):(i*101)] = i #label the run
  pyramidf_s$agegroup[((i-1)*101+1):(i*101)] = seq(0,100) #label the age group
}
pyramidf_s$year1pop_s <- pyramidf_s$proportion_s*totpop_s[pyramidf_s$run_num]*proportionf_s[pyramidf_s$run_num] #calculate number of people in the age group
pyramidf_s$year1pop_s = round(pyramidf_s$year1pop_s,0) #round to whole people

# Male pyramid
pyramidm_p <- data.frame(proportion_p=startingpop$Male/(totpop_p*(1-proportionf_p)))
pyramidm_s <- as.data.frame(sapply(pyramidm_p,rep.int,times=numruns))
pyramidm_s$multiplier_s <- runif(nrow(pyramidm_s),min=(1-pop_variance),max=(1+pop_variance))
pyramidm_s$proportion_s <- pyramidm_s$proportion_p*pyramidm_s$multiplier_s
pyramidm_s$run_num = 0
for(i in 1:numruns){
  tempsum = sum(pyramidm_s$proportion_s[((i-1)*101+1):(i*101)])
  pyramidm_s$proportion_s[((i-1)*101+1):(i*101)] = pyramidm_s$proportion_s[((i-1)*101+1):(i*101)]/tempsum
  pyramidm_s$run_num[((i-1)*101+1):(i*101)] = i
  pyramidm_s$agegroup[((i-1)*101+1):(i*101)] = seq(0,100)
}
pyramidm_s$year1pop_s <- pyramidm_s$proportion_s*totpop_s[pyramidm_s$run_num]*(1-proportionf_s[pyramidm_s$run_num])
pyramidm_s$year1pop_s = round(pyramidm_s$year1pop_s,0)

#################################################
# Generate stochastic birth and death rates per year

# Pull out values
br1519_p <- poprates$Value2020[4]
br2029_p <- poprates$Value2020[5]
br3039_p <- poprates$Value2020[6]
br4049_p <- poprates$Value2020[7]

mortinf_p <- poprates$Value2020[8]
mort14_p <- poprates$Value2020[9]
mort59_p <- poprates$Value2020[10]
mort1014_p <- poprates$Value2020[11]
mort1519_p <- poprates$Value2020[12]
mort2024_p <- poprates$Value2020[13]
mortadultf_p <- poprates$Value2020[14]
mortadultm_p <- poprates$Value2020[15]

br1519_delta_p <- poprates$AnnualChange[4]
br2029_delta_p <- poprates$AnnualChange[5]
br3039_delta_p <- poprates$AnnualChange[6]
br4049_delta_p <- poprates$AnnualChange[7]

mortinf_delta_p <- poprates$AnnualChange[8]
mort14_delta_p <- poprates$AnnualChange[9]
mort59_delta_p <- poprates$AnnualChange[10]
mort1014_delta_p <- poprates$AnnualChange[11]
mort1519_delta_p <- poprates$AnnualChange[12]
mort2024_delta_p <- poprates$AnnualChange[13]
mortadultf_delta_p <- poprates$AnnualChange[14]
mortadultm_delta_p <- poprates$AnnualChange[15]

# create a formatted template for dropping rates into
ratebase <- data.frame(year=seq(1,numyears))
ratebase <- as.data.frame(sapply(ratebase,rep.int,times=numruns))
ratebase$run_num = 0
for(i in 1:numruns){
  ratebase$run_num[((i-1)*numyears+1):(i*numyears)] = i #label the run
}

# create data frames
br1519_s <- ratebase
br2029_s <- ratebase
br3039_s <- ratebase
br4049_s <- ratebase

mrinf_s <- ratebase
mr14_s <- ratebase
mr59_s <- ratebase
mr1014_s <- ratebase
mr1519_s <- ratebase
mr2024_s <- ratebase
mraf_s <- ratebase
mram_s <- ratebase

# set up year 1 rates
br1519_s$rate[br1519_s$year==1] = rnorm(numruns,mean=br1519_p,sd=br1519_p*birthrate_sd)
br2029_s$rate[br2029_s$year==1] = rnorm(numruns,mean=br2029_p,sd=br2029_p*birthrate_sd)
br3039_s$rate[br3039_s$year==1] = rnorm(numruns,mean=br3039_p,sd=br3039_p*birthrate_sd)
br4049_s$rate[br4049_s$year==1] = rnorm(numruns,mean=br4049_p,sd=br4049_p*birthrate_sd)

mrinf_s$rate[mrinf_s$year==1] = rnorm(numruns,mean=mortinf_p,sd=mortinf_p*mortrate_sd)
mr14_s$rate[mr14_s$year==1] = rnorm(numruns,mean=mort14_p,sd=mort14_p*mortrate_sd)
mr59_s$rate[mr59_s$year==1] = rnorm(numruns,mean=mort59_p,sd=mort59_p*mortrate_sd)
mr1014_s$rate[mr1014_s$year==1] = rnorm(numruns,mean=mort1014_p,sd=mort1014_p*mortrate_sd)
mr1519_s$rate[mr1519_s$year==1] = rnorm(numruns,mean=mort1519_p,sd=mort1519_p*mortrate_sd)
mr2024_s$rate[mr2024_s$year==1] = rnorm(numruns,mean=mort2024_p,sd=mort2024_p*mortrate_sd)
mraf_s$rate[mraf_s$year==1] = rnorm(numruns,mean=mortadultf_p,sd=mortadultf_p*mortrate_sd)
mram_s$rate[mram_s$year==1] = rnorm(numruns,mean=mortadultm_p,sd=mortadultm_p*mortrate_sd)

# generate deltas
br1519_s$delta = rnorm(nrow(br1519_s),mean=br1519_delta_p,sd=br1519_delta_p*birthrate_sd)
br2029_s$delta = rnorm(nrow(br2029_s),mean=br2029_delta_p,sd=br2029_delta_p*birthrate_sd)
br3039_s$delta = rnorm(nrow(br3039_s),mean=br3039_delta_p,sd=br3039_delta_p*birthrate_sd)
br4049_s$delta = rnorm(nrow(br4049_s),mean=br4049_delta_p,sd=br4049_delta_p*birthrate_sd)

mrinf_s$delta = rnorm(nrow(mrinf_s),mean=mortinf_delta_p,sd=mortinf_delta_p*mortrate_sd)
mr14_s$delta = rnorm(nrow(mr14_s),mean=mort14_delta_p,sd=mort14_delta_p*mortrate_sd)
mr59_s$delta = rnorm(nrow(mr59_s),mean=mort59_delta_p,sd=mort59_delta_p*mortrate_sd)
mr1014_s$delta = rnorm(nrow(mr1014_s),mean=mort1014_delta_p,sd=mort1014_delta_p*mortrate_sd)
mr1519_s$delta = rnorm(nrow(mr1519_s),mean=mort1519_delta_p,sd=mort1519_delta_p*mortrate_sd)
mr2024_s$delta = rnorm(nrow(mr2024_s),mean=mort2024_delta_p,sd=mort2024_delta_p*mortrate_sd)
mraf_s$delta = rnorm(nrow(mraf_s),mean=mortadultf_delta_p,sd=mortadultf_delta_p*mortrate_sd)
mram_s$delta = rnorm(nrow(mram_s),mean=mortadultm_delta_p,sd=mortadultm_delta_p*mortrate_sd)

# carry deltas forward to future rates
for(yr in 2:numyears){
  br1519_s$rate[br1519_s$year==yr] = br1519_s$rate[br1519_s$year==(yr-1)]*br1519_s$delta[br1519_s$year==(yr-1)]
  br2029_s$rate[br2029_s$year==yr] = br2029_s$rate[br2029_s$year==(yr-1)]*br2029_s$delta[br2029_s$year==(yr-1)]
  br3039_s$rate[br3039_s$year==yr] = br3039_s$rate[br3039_s$year==(yr-1)]*br3039_s$delta[br3039_s$year==(yr-1)]
  br4049_s$rate[br4049_s$year==yr] = br4049_s$rate[br4049_s$year==(yr-1)]*br4049_s$delta[br4049_s$year==(yr-1)]
  
  mrinf_s$rate[mrinf_s$year==yr] = mrinf_s$rate[mrinf_s$year==(yr-1)]*mrinf_s$delta[mrinf_s$year==(yr-1)]
  mr14_s$rate[mr14_s$year==yr] = mr14_s$rate[mr14_s$year==(yr-1)]*mr14_s$delta[mr14_s$year==(yr-1)]
  mr59_s$rate[mr59_s$year==yr] = mr59_s$rate[mr59_s$year==(yr-1)]*mr59_s$delta[mr59_s$year==(yr-1)]
  mr1014_s$rate[mr1014_s$year==yr] = mr1014_s$rate[mr1014_s$year==(yr-1)]*mr1014_s$delta[mr1014_s$year==(yr-1)]
  mr1519_s$rate[mr1519_s$year==yr] = mr1519_s$rate[mr1519_s$year==(yr-1)]*mr1519_s$delta[mr1519_s$year==(yr-1)]
  mr2024_s$rate[mr2024_s$year==yr] = mr2024_s$rate[mr2024_s$year==(yr-1)]*mr2024_s$delta[mr2024_s$year==(yr-1)]
  mraf_s$rate[mraf_s$year==yr] = mraf_s$rate[mraf_s$year==(yr-1)]*mraf_s$delta[mraf_s$year==(yr-1)]
  mram_s$rate[mram_s$year==yr] = mram_s$rate[mram_s$year==(yr-1)]*mram_s$delta[mram_s$year==(yr-1)]
}

# normalize mortality rates to per-year-per-person
mrinf_s$rate = mrinf_s$rate/1000
mr14_s$rate = mr14_s$rate/1000
mr59_s$rate = mr59_s$rate/1000
mr1014_s$rate = mr1014_s$rate/1000
mr1519_s$rate = mr1519_s$rate/1000
mr2024_s$rate = mr2024_s$rate/1000
mraf_s$rate = mraf_s$rate/1000
mram_s$rate = mram_s$rate/1000


#################################################
# Calculate population pyramid into the future. Melt format.

# Create framework for year 1
pop_s <- data.frame(agegroup=seq(0,100),run_num=1)
for(i in 2:numruns){
  poppyramid_next <- data.frame(agegroup=seq(0,100),run_num=i)
  pop_s <- rbind(pop_s,poppyramid_next)
}
remove(poppyramid_next)
pop_s$year <- 1

pop_s$mortrate_s <- 0 #mortality rates for those age 0-24
pop_s$mortrate_af_s <- 0 #mortality rates for women 25+
pop_s$mortrate_am_s <- 0 #mortality rates for men 25+
pop_s$deaths_ly <- 0 #total deaths_ly
pop_s$deaths_ly_af <- 0 #adult female deaths_ly
pop_s$deaths_ly_am <- 0 #adult male deaths_ly
pop_s$deaths_ly_f <- 0 #all female deaths_ly
pop_s$deaths_ly_m <- 0 #all male deaths_ly
pop_s$agepull <- 0 #this is going to the be the offset by 1 reference age group

# Copy in year 1 stochastic population pyramid that was previously generated
pop_s$femalepop <- pyramidf_s$year1pop_s
pop_s$malepop <- pyramidm_s$year1pop_s

for (yr in 2:numyears){
  
  # Create temporary dataframe for calculating this next year's population
  popprev <- subset(pop_s, year==(yr-1)) #last year
  popnew <- popprev #copy last year, then clear populations
  popnew$femalepop <- 0
  popnew$malepop <- 0
  popnew$year <- yr #this is now a new year's dataset, but blank
  
  # Calculate fertile population, previous year
  popsub1519 <- subset(pop_s,agegroup>14 & agegroup<20 & year==(yr-1))
  fertilepop1519_s <- ddply(popsub1519,.(run_num),summarize,numfertilepop = sum(femalepop))
  fertilepop2029_s <- ddply(subset(pop_s,agegroup>19 & agegroup<30 & year==(yr-1)),.(run_num),summarize,numfertilepop = sum(femalepop))
  fertilepop3039_s <- ddply(subset(pop_s,agegroup>29 & agegroup<40 & year==(yr-1)),.(run_num),summarize,numfertilepop = sum(femalepop))
  fertilepop4049_s <- ddply(subset(pop_s,agegroup>39 & agegroup<50 & year==(yr-1)),.(run_num),summarize,numfertilepop = sum(femalepop))
  
  # Calculate number of births during the last year (approximation: all fertility from last year arrives this year)
  infants_s <-  fertilepop1519_s$numfertilepop * br1519_s$rate[br1519_s$year==yr] + 
                fertilepop2029_s$numfertilepop * br2029_s$rate[br2029_s$year==yr] +
                fertilepop3039_s$numfertilepop * br3039_s$rate[br3039_s$year==yr] +
                fertilepop4049_s$numfertilepop * br4049_s$rate[br4049_s$year==yr]
  
  # Drop infants into this year's population pyramid
  popnew$femalepop[popnew$agegroup==0] = infants_s/2
  popnew$malepop[popnew$agegroup==0] = infants_s/2
  
  # Drop in stochastic mortality rates
  popnew$mortrate_s[popnew$agegroup==0] = mrinf_s$rate[mrinf_s$year==yr]
  popnew$mortrate_s[popnew$agegroup>0 & popnew$agegroup<5] = mr14_s$rate[mrinf_s$year==yr]
  popnew$mortrate_s[popnew$agegroup>4 & popnew$agegroup<10] = mr59_s$rate[mrinf_s$year==yr]
  popnew$mortrate_s[popnew$agegroup>9 & popnew$agegroup<15] = mr1014_s$rate[mrinf_s$year==yr]
  popnew$mortrate_s[popnew$agegroup>14 & popnew$agegroup<20] = mr1519_s$rate[mrinf_s$year==yr]
  popnew$mortrate_s[popnew$agegroup>19 & popnew$agegroup<25] = mr2024_s$rate[mrinf_s$year==yr]
  
  popnew$mortrate_af_s[popnew$agegroup>24] = mraf_s$rate[mrinf_s$year==yr]
  popnew$mortrate_am_s[popnew$agegroup>24] = mram_s$rate[mrinf_s$year==yr]
  
  # Calculate deaths_ly during the last year
  popnew$deaths_ly_f = popnew$mortrate_s * popprev$femalepop
  popnew$deaths_ly_m = popnew$mortrate_s * popprev$malepop
  popnew$deaths_ly_af = popnew$mortrate_af_s * popprev$femalepop
  popnew$deaths_ly_am = popnew$mortrate_am_s * popprev$malepop
  popnew$deaths_ly_f = popnew$deaths_ly_f + popnew$deaths_ly_af #sum up components of mortality
  popnew$deaths_ly_m = popnew$deaths_ly_m + popnew$deaths_ly_am #sum up components of mortality
  popnew$deaths_ly = popnew$deaths_ly_f + popnew$deaths_ly_m
  
  # Calculate net new population pyramid = last year - deaths_ly
  popnew$agepull <- popnew$agegroup-1 #lookup offset
  
  popnew$femalepop[popnew$agegroup>0] = 
    popprev$femalepop[match(popnew$agepull[popnew$agegroup>0],popprev$agegroup)] - #last year's population for 1-year-younger pop
    popnew$deaths_ly_f[match(popnew$agepull[popnew$agegroup>0],popnew$agegroup)] #minus the calculated deaths for the 1-year-younger pop
  
  popnew$malepop[popnew$agegroup>0] = 
    popprev$malepop[match(popnew$agepull[popnew$agegroup>0],popprev$agegroup)] - 
    popnew$deaths_ly_m[popnew$agegroup>0]
  
  # Append new year to existing dataset
  pop_s <- rbind(pop_s,popnew)
  
}

# Save out to CSV 
write.csv(pop_s,paste("../Outputs/Annual stochastic demographics",Sys.Date(),".csv"))
