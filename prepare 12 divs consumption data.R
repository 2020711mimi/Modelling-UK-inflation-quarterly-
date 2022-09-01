library(readxl)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyverse)
library(broom)
library(stargazer)
library(knitr)
library(officer)
library(flextable)
library(magrittr)
library(dplyr)
library(xts)
library(zoo)
library(dyn)
library(dynlm)
library(olsrr)
library(modelsummary)
library(car)
library(rms)
library(rio)
library(dplyr)
library(plyr)
library(TSstudio)

Sys.setlocale("LC_TIME", "English")
rm(list = ls())
options(scipen = 200)
# Data -------------------------------------------------------------------------
source("LDV.R")
#read data



consum <- read_excel("Consumption expenditure data.xlsx")

ts.consum <-
  ts(consum[2:length(consum)], start = c(1997, 1), frequency = 4)

#convert % change


yue.3<- lead(consum[,2:14])

yue.2<- consum[,2:14]*1/3 + lead(2/3*consum[,2:14])
yue.1 <-  lead(consum[,2:14]*1/3)+ 2/3*consum[,2:14]

CONSUMPTION <- data.frame(matrix(ncol = 13,nrow=3*nrow(yue.1)))
CONSUMPTION[is.na(CONSUMPTION)] <- 0

CONSUMPTION[c(seq(1,nrow(CONSUMPTION),3)),]<-yue.1
CONSUMPTION[c(seq(2,nrow(CONSUMPTION),3)),]<-yue.2
CONSUMPTION[c(seq(3,nrow(CONSUMPTION),3)),]<-yue.3


names(CONSUMPTION) <-   c("CPI","01FB",
                   "02AT",
                   "03CF",
                   "04HW",
                   "05FH",
                   "06HL",
                   "07TR",
                   "08CM",
                   "09RC",
                   "10ED" ,
                   "11RH",
                   "12MS")

colnames(consum)[2:14] <- c("CPI","01FB",
                            "02AT",
                            "03CF",
                            "04HW",
                            "05FH",
                            "06HL",
                            "07TR",
                            "08CM",
                            "09RC",
                            "10ED" ,
                            "11RH",
                            "12MS")
colnames(consum)
CONSUMPTION<- rbind(consum[1,2:length(consum)],CONSUMPTION)
ts.CONSUMPTION <- ts(CONSUMPTION,start = c(1997,02),end = c(2021,8),frequency = 12)
date <-  seq.Date(from = as.Date("1997/02/01",format = "%Y/%m/%d"), by = "month", length.out = 298)
growth_rate <- function(x)
  (x / lag(x) - 1) * 100
CONSUMPTION<- growth_rate(CONSUMPTION)
CONSUMPTION <- cbind(date,CONSUMPTION)
CONSUMPTION <- na.omit(CONSUMPTION)
#CONSUMPTION<- CONSUMPTION[2:nrow(CONSUMPTION),]
tail(CONSUMPTION)
head(CONSUMPTION)
lapply(CONSUMPTION,adf.test)


write.csv(CONSUMPTION,"consumption monthly data change %.csv",row.names = FALSE)
sapply(CONSUMPTION,class)
sapply(consum,class)
sapply(yue.2,class)
