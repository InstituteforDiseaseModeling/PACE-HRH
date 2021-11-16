library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(lme4)

dirs = "C:/Users/bhagedorn/OneDrive - Institute for Disease Modeling/Projects/HEP capacity projection/Data"
setwd(dirs)


ds1 <- read_excel("Malaria seasonality rates.xlsx",sheet="Sheet1")
dsm1 <- melt(ds1)
dsm1$Month <- as.factor(dsm1$Month)

region <- read_excel("Malaria seasonality rates.xlsx",sheet="Sheet3")
dsm1$region <- region$Region[match(dsm1$variable,region$Geography)]

monthtable <- data.frame(month=c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"),
                         monthnum=seq(1,12,1))

lmout <- lm(value ~ Month + variable,data=dsm1)
lmout <- lm(value ~ Month + region,data=dsm1)

ds2 <- read_excel("Malaria seasonality rates.xlsx",sheet="Sheet2")
dsm2 <- melt(ds2)
dsm2$MonthNum <- monthtable$monthnum[match(dsm2$Month,monthtable$month)]
dsm2$Month <- as.factor(dsm2$Month)
dsm2$region <- region$Region[match(dsm2$variable,region$Geography)]

lmout <- lm(value ~ Month + region,data=dsm2) #R2 .52
lmout <- lm(value ~ Month*region,data=dsm2) #R2 .55

#lmerout <- lmer(value ~ (1|region) + (1|Month),data=dsm2) #singular fit

#lmerout <- lmer(value ~ (Month|region),data=dsm2) #singular fit

mspl <- smooth.spline(dsm2$Month,dsm2$value)

## Residual (Tukey Anscombe) plot:
plot(residuals(mspl) ~ fitted(mspl))
abline(h = 0, col = "gray")

## The chosen inner knots in original x-scale :
with(mspl$fit, min + range * knot[-c(1:3, nk+1 +1:3)]) # == unique(cars$speed)
