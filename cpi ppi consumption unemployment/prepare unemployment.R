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

# -------------------------------------------------------------------------

unemployment <- rio::import("cpi ppi consumption unemployment/Unemployment rate.csv")
 unemployment97.3_2019.12 <- unemployment[which( unemployment$Title=="1997 FEB"):which(unemployment$Title=="2019 DEC"),]
 
 growth_rate <- function(x)
   (x / lag(x) - 1) * 100
 #as numeric
unemployment97.3_2019.12$`Unemployment rate (aged 16 and over, seasonally adjusted): %` <- as.numeric(unemployment97.3_2019.12$`Unemployment rate (aged 16 and over, seasonally adjusted): %`)

#growh rate
  unemployment97.3_2019.12$`Unemployment rate (aged 16 and over, seasonally adjusted): %` <- growth_rate(unemployment97.3_2019.12$`Unemployment rate (aged 16 and over, seasonally adjusted): %`)
  #remove na
unemployment97.3_2019.12 <- na.omit(unemployment97.3_2019.12)

adf.test(unemployment97.3_2019.12$`Unemployment rate (aged 16 and over, seasonally adjusted): %`)
  
rio::export(unemployment97.3_2019.12, "unemployment 97-19.csv")
