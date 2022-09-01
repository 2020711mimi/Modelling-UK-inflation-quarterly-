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
library(tseries)

Sys.setlocale("LC_TIME", "English")
rm(list = ls())
options(scipen = 200)

# Data-------------------------------------------------------------------------

wage <- import("output, unemployment, PPI and hourly wages/Seasonally Adjusted Total Pay Excluding Arrears.csv")

which(wage$Title=="2000 JAN")

wage.2000 <- wage[which(wage$Title=="2000 JAN"):nrow(wage),]

wage.ts <- ts(wage.2000[,2],start = c(2000,01), frequency = 12)

adf.test(wage.ts)
adf.test(wage.2000$`AWE: Whole Economy Level (£): Seasonally Adjusted Total Pay Excluding Arrears`)
pp.test(wage.ts)

#not stationary


growth_rate <- function(x)  (x / lag(x) - 1) * 100

sapply(wage.2000,class)

wage.2000$`AWE: Whole Economy Level (£): Seasonally Adjusted Total Pay Excluding Arrears` <- as.numeric(wage.2000$`AWE: Whole Economy Level (£): Seasonally Adjusted Total Pay Excluding Arrears`)

wage.2000$change <- growth_rate(wage.2000$`AWE: Whole Economy Level (£): Seasonally Adjusted Total Pay Excluding Arrears`)

adf.test(wage.2000$change)

wage.new <- na.omit(wage.2000)

adf.test(wage.new$change)

export(wage.new[,c(1,3)],"wage %.csv")
