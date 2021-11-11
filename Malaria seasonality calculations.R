library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(lme4)

ds1 <- read_excel("../Data/Malaria seasonality rates.xlsx",sheet="Sheet1")
dsm1 <- melt(ds1)
dsm1$Month <- as.factor(dsm1$Month)

region <- read_excel("../Data/Malaria seasonality rates.xlsx",sheet="Sheet3")
dsm1$region <- region$Region[match(dsm1$variable,region$Geography)]

lmout <- lm(value ~ Month + variable,data=dsm1)
lmout <- lm(value ~ Month + region,data=dsm1)

ds2 <- read_excel("../Data/Malaria seasonality rates.xlsx",sheet="Sheet2")
dsm2 <- melt(ds2)
dsm2$Month <- as.factor(dsm2$Month)
dsm2$region <- region$Region[match(dsm2$variable,region$Geography)]

lmout <- lm(value ~ Month + region,data=dsm2)


