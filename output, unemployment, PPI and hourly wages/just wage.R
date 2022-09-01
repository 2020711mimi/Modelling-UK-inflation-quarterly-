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
source("output, unemployment, PPI and hourly wages/stargazer and modelsummary function.R")


df_cpi <- rio::import("cpi ppi consumption unemployment/div.cpi97.csv") 
df_ppi <- rio::import("cpi ppi consumption unemployment/ppi %97.csv")
df_unemploy <- rio::import("cpi ppi consumption unemployment/unemployment 97-19%.csv")
df_consumption <- rio::import("cpi ppi consumption unemployment/consumption monthly data change %.csv")
df_wage <- import("output, unemployment, PPI and hourly wages/wage %.csv")

df_consumption <- df_consumption[1:which(df_consumption$date=="2019-12-01"),]
#CPI lag create
for (i in 1:12) {
  assign(paste0("cpi",i),lag(df_cpi,i))
  
}
for (i in 1:12) {
  assign(paste0("CPI_LDV",i),tail(get(paste0("cpi",i)),239))
}


CPI <- df_cpi[which(df_cpi$date=="2000-02-01"):nrow(df_cpi),]
CPI

#create consumption lag

for (i in 1:12) {
  assign(paste0("consum",i),lag(df_consumption,i))
  
}

for (i in 1:12) {
  assign(paste0("Con_LDV",i),tail(get(paste0("consum",i)),239))
}

Consumption <- df_consumption[which(df_consumption$date=="2000-02-01"):nrow(df_consumption),]

# cpi <- ts(df_cpi[,2:length(df_cpi)],start = c(1997,03),end = c(2019,12),frequency = 12)
#consum <- ts(df_consumption[,2:length(df_consumption)],start = c(1997,03),end = c(2019,12),frequency = 12)

ppi <- ts(df_ppi[,3],start = c(1997,03),end = c(2019,12),frequency = 12)
unemploy <- ts(df_unemploy[,2],start = c(1997,03),end = c(2019,12),frequency = 12)
wage <- ts(df_wage[,2],start = c(2000,02),end = c(2019,12),frequency = 12)

# cpi <- window(cpi,start=c(2000,02))
ppi <- window(ppi,start=c(2000,02))
unemploy <- window(unemploy,start=c(2000,02))
# consum <- window(consum,start=c(2000,02))

#crea a time index for dummy create

time1 = seq.Date(
  from = as.Date("2000/02/01", format = "%Y/%m/%d"),
  by = "month",
  length.out =239
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


# -------------------------------------------------------------------------





model.div <- list()
for (i in 3:ncol(CPI)) {
  form <-
    reformulate(
      c(
        paste0("CPI_LDV", 1:12, "[, 2]") ,
        paste0("CPI_LDV", 1:12, "[, i]") ,
        "wage",
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



# Stepwise AIC----------------------------------------------------------------



#step divs
step_divs <- list()

for (i in 3:ncol(CPI)) {
  step_divs[i] <- step(
    model.div[[i]],
    scope = list(
      lower = CPI[, i] ~  VAT1 + VAT2 + VAT3 + Recession + D. + Trend
      ,
      upper = model.div[[i]]
    )
  )
  
}


#save output to y1:y12
for (i in 3:ncol(CPI)) {
  assign(paste0("y", i), step(
    model.div[[i]],
    scope = list(
      lower = CPI[, i] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
      ,
      upper = model.div[[i]]
    )
  ))
  
}

# ALL-------------------------------------------------------------------------
model.div[sapply(model.div, is.null)] <- NULL

ugly_name <-  c(
  paste0("CPI_LDV",1:12, "[, 2]"),
  paste0("CPI_LDV",1:12, "[, i]"),
  paste0("Con_LDV",1:12, "[, i]")
)
ugly_name

new_name <- c(paste0("CPI_LDV", 1:12),
              paste0("LDV", 1:12),
              paste0("Con_LDV", 1:12)
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


star(model.div)
div.name <-
  c("01FB",
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
stargazer(model.div,
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(div.name),
          report = "vc*p",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2,
          single.row = TRUE,
          model.numbers = FALSE,
          summary = FALSE,
          flip = FALSE,
          se = NULL,
          out = "output, unemployment, PPI and hourly wages/just wage P.html",
          type = "text"
)   

names(model.div) <- c(div.name)

modelsummary(
  model.div,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "output, unemployment, PPI and hourly wages/wage.html"
)

# AIC -------------------------------------------------------------------------
y.list1 <- mget(paste0("y",3:14))
found <- list()
for (i in 1:length(y.list1)) {
  found[[i]] <-
    match(names(y.list1[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(y.list1[[i]]$coefficients)[names(y.list1[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}



vars.order <- new_name
y.list1[sapply(y.list1, is.null)] <- NULL


names(y.list1) <- c(div.name)
modelsummary(
  y.list1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = new_name1,
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "output, unemployment, PPI and hourly wages/wage AIC.html"
)


# AIC-------------------------------------------------------------------------




stargazer(y.list1,
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(div.name),
          report = "vc*p",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2,
          single.row = TRUE,
          model.numbers = FALSE,
          summary = FALSE,
          flip = FALSE,
          se = NULL,
          out = "output, unemployment, PPI and hourly wages/wage AIC P.html",
          type = "text"
)   




