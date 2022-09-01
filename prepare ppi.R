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

Sys.setlocale("LC_TIME", "English")
rm(list = ls())
options(scipen = 200)

# -------------------------------------------------------------------------

raw.ppi <- read.csv("cpi ppi consumption unemployment/Producer price inflation time series (PPI).csv")
ppi.971<- raw.ppi[which(raw.ppi$Title=="1997 FEB"):nrow(raw.ppi),]
sapply(ppi.971,class)
ppi.971[,2] <- as.numeric(ppi.971[,2])
ts.ppi <- ts(ppi.971[,2],start = c(1997,2),frequency = 12)
# show in %

ts.ppi.percent <- 100 * (ts.ppi / stats::lag(ts.ppi, -1) - 1)
date <-  seq.Date(from = as.Date("1997/03/01",format = "%Y/%m/%d"), by = "month", length.out = 303)
#ts.ppi.percent <- cbind(date,ts.ppi.percent)
library(TSstudio)
df<- ts_reshape(ts.ppi.percent,type = "long",frequency = 12)
df
write.csv(df,"cpi ppi consumption unemployment/ppi change97.csv",row.names = FALSE)
