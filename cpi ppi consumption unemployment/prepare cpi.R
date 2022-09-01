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

date <-  seq.Date(from = as.Date("1997/03/01",format = "%Y/%m/%d"), by = "month", length.out = 274)

divs <- read_excel("cpi data/CPI 12divs.xlsx")

#convert ts :start from 1988
ts.divs <-
  ts(divs[2:length(divs)], start = c(1988, 1), frequency = 12)

#convert % change

ts.divs.percent <- 100 * (ts.divs / stats::lag(ts.divs, -1) - 1)


divs97<- window(ts.divs.percent,start=c(1997,03),end=c(2019,12))
df.divs.cpi97 <- data.frame(divs97)
div.cpi98<- cbind(date,df.divs.cpi97)
colnames(div.cpi98) <- c("date","CPI","01FB",
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
#write.csv(div.cpi98,"div.cpi97.csv",row.names = FALSE)