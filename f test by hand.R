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
library(routput_03_08)
library(rio)
library(dplyr)

Sys.setlocale("LC_TIME", "English")
rm(list = ls())
options(scipen = 200)
# Data -------------------------------------------------------------------------
#read data
divs <- read_excel("cpi data/CPI 12divs.xlsx")

#convert ts :start from 1988
ts.divs <-
  ts(divs[2:length(divs)], start = c(1988, 1), frequency = 12)

#convert % change

ts.divs.percent <- 100 * (ts.divs / stats::lag(ts.divs,-1) - 1)

#crea a time for dummy create

time1 = seq.Date(
  from = as.Date("1988/02/01", format = "%Y/%m/%d"),
  by = "month",
  length.out = 417
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
# Regression --------------------------------------------------------------
#cpi

reg.cpi <-
  dynlm(
    reformulate(
      c(
        paste0("L(ts.divs.percent[,1],", 1:12, ")") ,
        "VAT1",
        "VAT2",
        "VAT3",
        "Recession",
        "D.",
        "Trend"
      ),
      "ts.divs.percent[,1]"
    ),
    start = c(1993, 1),
    end = c(2019, 12)
  )

#div
model.div <- vector("list", 13)

for (i in 2:13) {
  form <-
    reformulate(
      c(
        paste0("L(ts.divs.percent[,i],", 1:12, ")"),
        paste0("L(ts.divs.percent[,1],", 1:12, ")") ,
        "VAT1",
        "VAT2",
        "VAT3",
        "Recession",
        "D.",
        "Trend"
      ),
      "ts.divs.percent[,i]"
    )
  model.div[[i]] <-
    dynlm(form, start = c(1993, 1), end = c(2019, 12))
}

# output ------------------------------------------------------------------


ugly_name <-  c(
  paste0("L(ts.divs.percent[, i], ", 1:12, ")"),
  paste0("L(ts.divs.percent[, 1], ", 1:12, ")")
)
ugly_name

new_name <- c(paste0("LDV", 1:12),
              paste0("CPI-LDV", 1:12))
new_name
cm <- setNames(new_name, ugly_name)

# Stepwise AIC----------------------------------------------------------------


#step for cpi
step_CPI <- step(
  reg.cpi,
  scope = list(
    lower = ts.divs.percent[, 1] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
    ,
    upper = reg.cpi
  )
)

#step divs
step_divs <- list()

for (i in 2:13) {
  step_divs[i] <- step(
    model.div[[i]],
    scope = list(
      lower = ts.divs.percent[, i] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
      ,
      upper = model.div[[i]]
    )
  )
  
}


#save output to y2:y13
for (i in 2:13) {
  assign(paste0("y", i), step(
    model.div[[i]],
    scope = list(
      lower = ts.divs.percent[, i] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
      ,
      upper = model.div[[i]]
    )
  ))
  
}

# 01 ----------------------------------------------------------------------
y.list <- mget(paste0("y",2:13))
modelsummary(y.list)

source("LDV.r")
search_for_these <- ugly_name
replace_with_these <- new_name
found <- list()
for (i in 1:length(y.list)) {
  found[[i]] <-
    match(names(y.list[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(y.list[[i]]$coefficients)[names(y.list[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
vars.order <- new_name
y.list[sapply(y.list, is.null)] <- NULL

stargazer(
  y.list,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  #out = "stage 1 regression.html",
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se = NULL,
  column.labels = c(div.name),
  type = "text"
)

summary(y2)[[4]][,4]

names(summary(y2)[[4]][,4][summary(y2)[[4]][,4] >0.1] & names(summary(y2)[[4]][,4] %in% ugly_name))
names(summary(y3)[[4]][,4][summary(y3)[[4]][,4] >0.1] & names(summary(y3)[[4]][,4] %in% ugly_name))

summary(y3)[[4]][,4][summary(y3)[[4]][,4]>0.1 & summary(y3)[[4]][,4] %in% ugly_name] 

for (i in ) {
  
}
summary(
  mget(paste0("y",2:13)))[[4]][,4][summary(mget(paste0("y",2:13)))[[4]][,4]>0.1 & summary(mget(paste0("y",2:13)))[[4]][,4] %in% ugly_name] 

