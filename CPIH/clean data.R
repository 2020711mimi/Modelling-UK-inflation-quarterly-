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
library(gtsummary)
Sys.setlocale("LC_TIME", "English")
rm(list = ls())
options(scipen = 200)

# Data-------------------------------------------------------------------------

df <- import("CPIH/CPIH-ALL.xlsx")


df.divs<- df[,which(colnames(df) %in% paste0(0:12))]
head(df.divs)

df.divs<- df.divs[-c(1:2),]       

df.divs<- sapply(df.divs,as.numeric)
df.divs <- data.frame(df.divs)
sapply(df.divs,class)

growth <- function(x) 100* (x/lag(x)-1)

df.divs <- growth(df.divs)

date <- seq.Date(from = as.Date("1988/01/01",format="%Y/%m/%d"),by = "month",length.out = 395)
Df.divs<- cbind(date,df.divs)

div.name <-
  c("date",
    "CPI",
    "01FB",
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

colnames(Df.divs) <- c(div.name)

head(Df.divs)

which(Df.divs$date=="1993-01-01")
df.1992<- Df.divs[which(Df.divs$date=="1992-01-01"):which(Df.divs$date=="2019-12-01"),]

export(df.1992,"CPIH/CPI and 12 divs.csv")
