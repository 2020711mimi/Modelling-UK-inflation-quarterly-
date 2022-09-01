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
source("output, unemployment, PPI and hourly wages/stargazer and modelsummary function.R")

# Data-------------------------------------------------------------------------
source("output, unemployment, PPI and hourly wages/stargazer and modelsummary function.R")


df_cpi <- rio::import("cpi ppi consumption unemployment/div.cpi97.csv") 
df_ppi <- rio::import("cpi ppi consumption unemployment/ppi %97.csv")
df_unemploy <- rio::import("cpi ppi consumption unemployment/unemployment 97-19%.csv")
df_consumption <- rio::import("cpi ppi consumption unemployment/consumption monthly data change %.csv")
df_wage <- import("output, unemployment, PPI and hourly wages/wage %.csv")
df_consumption <- df_consumption[1:which(df_consumption$date=="2019-12-01"),]

# wage 12 lag-------------------------------------------------------------------------
nrow( df_wage[which(df_wage$Title=="2001 FEB"):which(df_wage$Title=="2019 DEC"),])
df_wage_19<- ( df_wage[which(df_wage$Title=="2000 FEB"):which(df_wage$Title=="2019 DEC"),])

for (i in 1:12) {
  assign(paste0("wage",i),lag(df_wage_19,i))
  
}

for (i in 1:12) {
  assign(paste0("WAGE_LDV",i),tail(get(paste0("wage",i)),227))
}

# #CPI lag create-------------------------------------------------------------------------

for (i in 1:12) {
  assign(paste0("cpi",i),lag(df_cpi,i))
  
}
for (i in 1:12) {
  assign(paste0("CPI_LDV",i),tail(get(paste0("cpi",i)),227))
}


CPI <- df_cpi[which(df_cpi$date=="2001-02-01"):nrow(df_cpi),]

#create consumption lag

#consum -------------------------------------------------------------------------


for (i in 1:12) {
  assign(paste0("consum",i),lag(df_consumption,i))
  
  
}

for (i in 1:12) {
  assign(paste0("Con_LDV",i),tail(get(paste0("consum",i)),227))
}

Consumption <- df_consumption[which(df_consumption$date=="2001-02-01"):nrow(df_consumption),]

#PPI -------------------------------------------------------------------------
df_ppi$date <- seq.Date(  from = as.Date("1997/03/01", format = "%Y/%m/%d"),
                          by = "month",
                          length.out =303)

PPI19 <- df_ppi[which(df_ppi$date=="2000-02-01"):which(df_ppi$date=="2019-12-01"),]

for (i in 1:12) {
  assign(paste0("PPI",i),lag(PPI19,i))
  
}

for (i in 1:12) {
  assign(paste0("PPI_LDV",i),tail(get(paste0("PPI",i)),227))
}

PPI <- df_ppi[which(df_ppi$date=="2001-02-01"):which(df_ppi$date=="2019-12-01"),]


# Unemployment ------------------------------------------------------------


for (i in 1:12) {
  assign(paste0("U",i),lag(df_unemploy,i))
  
}

for (i in 1:12) {
  assign(paste0("U_LDV",i),tail(get(paste0("U",i)),227))
}

#crea a time index for dummy create  -------------------------------------------------------------------------


time1 = seq.Date(
  from = as.Date("2001/02/01", format = "%Y/%m/%d"),
  by = "month",
  length.out =227
)


#dummies var
VAT1 <- as.numeric(time1 == "2008-12-01")
VAT2 <- as.numeric(time1  == "2010-01-01")
VAT3 <- as.numeric(time1  == "2011-01-01")
Recession <-
  as.numeric(time1  >= "2008-04-01" &
               time1  <= "2009-06-01")
#montyhly dummies
D. = as.factor(months(time1))

#trend

Trend = seq_along(time1)

# res-------------------------------------------------------------------------

model.div <- list()
for (i in 2:ncol(CPI)) {
  form <-
    reformulate(
      c(
        paste0("CPI_LDV", 1:12, "[, 2]") ,
        paste0("CPI_LDV", 1:12, "[, i]") ,

        paste0("PPI_LDV", 1:12, "[, 3]") ,
        "VAT1",
        "VAT2",
        "VAT3",
        "Recession",
        "D.",
        "Trend"
      ),
      "CPI[,i]"
    )
  
  model.div[[i]] <- lm(form)
  
  
}

star(model.div[2:13])

#output -------------------------------------------------------------------------
ugly_name <-  c(
  paste0("CPI_LDV", 1:12, "[, 2]") ,
  paste0("CPI_LDV", 1:12, "[, i]") ,
  paste0("Con_LDV", 1:12, "[, i]") ,
  paste0("PPI_LDV", 1:12, "[, 3]") ,
  paste0("U_LDV", 1:12, "[, 2]") ,
  paste0("WAGE_LDV", 1:12, "[, 2]") 
)
ugly_name

new_name <- c(paste0("CPI_LDV", 1:12),
              paste0("LDV", 1:12),
              paste0("Con_LDV", 1:12) ,
              paste0("PPI_LDV", 1:12) ,
              paste0("U_LDV", 1:12) ,
              paste0("WAGE_LDV", 1:12) 
)
new_name

search_for_these <- ugly_name
replace_with_these <- new_name

cm <- setNames(new_name, ugly_name)
cm

new_name1 <- c(paste0("CPI_LDV", 1:12),paste0("LDV", 1:12),
               paste0("Con_LDV", 1:12),"ppi","unemploy","wage")

found <- list()
for (i in 1:length(model.div)) {
  found[[i]] <-
    match(names(model.div[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(model.div[[i]]$coefficients)[names(model.div[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
vars.order <- new_name1
div.name <-
  c("CPI",
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

names(model.div) <- c(div.name)

modelsummary(
  model.div,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "12 lag unemploy wage ppi/ppi.html"
)

