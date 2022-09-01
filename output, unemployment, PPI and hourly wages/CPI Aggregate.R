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


# CPI ---------------------------------------------------------------------


# general ---------------------------------------------------------------------

lapply(2:ncol(CPI),function(x) lm(CPI[,x]~ CPI_LDV1[,x]+D.))

reformulate(
  c(
    paste0("CPI_LDV", 1:12, "[,2]") ,
    paste0("Con_LDV", 1:12, "[,2]") ,
    "ppi","unemploy","wage",
    "VAT1",
    "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"
  ),
  "CPI[,2]"
)


reg.cpi <- 
lm(
    reformulate(
      c(
        paste0("CPI_LDV", 1:12, "[,2]") ,
        paste0("Con_LDV", 1:12, "[,2]") ,
        "ppi","unemploy","wage",
        "VAT1",
        "VAT2",
        "VAT3",
        "Recession",
        "D.",
        "Trend"
      ),
      "CPI[,2]"
    )
    
  )

star(reg.cpi)
# AIC ---------------------------------------------------------------------


#step for cpi
step_CPI <- step(
  reg.cpi,
  scope = list(
    lower = CPI[, 2] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
    ,
    upper = reg.cpi
  )
)


starp(list(step_CPI,reg.cpi))



# stage 1 -----------------------------------------------------------------


all.coef.name <- names(step_CPI$coefficients)

LDV <- c(        paste0("CPI_LDV", 1:12, "[, 2]") ,
                 paste0("Con_LDV", 1:12, "[, 2]") ,
                 "ppi","unemploy","wage")
LDV <- LDV[-1]
LDV


#coef name with p value >0.1

#extract results table
insiginifcant.coef <- names(which(coef(summary(step_CPI))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(step_CPI))[, 'Pr(>|t|)'] > 0.1)) %in% LDV]
 
insiginifcant.coef

# F -----------------------------------------------------------------------

linearHypothesis(step_CPI,insiginifcant.coef)

# Linear hypothesis test
# 
# Hypothesis:
#   Con_LDV1[, 2] = 0
# Con_LDV3[, 2] = 0
# 
# Model 1: restricted model
# Model 2: CPI[, 2] ~ CPI_LDV6[, 2] + CPI_LDV12[, 2] + Con_LDV1[, 2] + Con_LDV2[, 
#                                                                               2] + Con_LDV3[, 2] + Con_LDV4[, 2] + Con_LDV9[, 2] + Con_LDV11[, 
#                                                                                                                                              2] + Con_LDV12[, 2] + ppi + wage + VAT1 + VAT2 + VAT3 + Recession + 
#   D. + Trend
# 
# Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1    213 5.5371                              
# 2    211 5.4166  2   0.12046 2.3461 0.09823 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# -------------------------------------------------------------------------


#re estimate 




coef_without_insigni <- list()
coef_without_insigni <- 
    print(names(step_CPI$coefficients)[names(step_CPI$coefficients) %in% LDV &
                                             names(step_CPI$coefficients) %!in% insiginifcant.coef])

  coef_without_insigni
var <- 
  c(coef_without_insigni, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend")

all_sig_formula <- as.formula(paste(
    "CPI[, 2] ~",
    paste(var, collapse = "+")))
  


all_sig_formula

stage1_cpi <- 
  lm(
    all_sig_formula
    
  )


# stage 2-------------------------------------------------------------------------
insiginifcant.coef2 <- names(which(coef(summary(stage1_cpi))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(stage1_cpi))[, 'Pr(>|t|)'] > 0.1)) %in% LDV]

insiginifcant.coef2


# F -----------------------------------------------------------------------
linearHypothesis(stage1_cpi,insiginifcant.coef2)

# -------------------------------------------------------------------------


coef_without_insigni <- 
  print(names(stage1_cpi$coefficients)[names(stage1_cpi$coefficients) %in% LDV &
                                       names(stage1_cpi$coefficients) %!in% insiginifcant.coef2])
coef_without_insigni

var <- 
  c(coef_without_insigni, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend")

all_sig_formula <- as.formula(paste(
  "CPI[, 2] ~",
  paste(var, collapse = "+")))



all_sig_formula

stage2_cpi <- 
  lm(
    all_sig_formula
    
  )
# -------------------------------------------------------------------------

general.cpi <- list(reg.cpi,step_CPI,stage1_cpi,stage2_cpi)

ugly_name <-  c(
  paste0("CPI_LDV",1:12, "[, 2]"),
  paste0("Con_LDV",1:12, "[, 2]")
)
ugly_name

new_name <- c(paste0("CPI_LDV", 1:12),
              paste0("Con_LDV", 1:12)
)
new_name

search_for_these <- ugly_name
replace_with_these <- new_name

cm <- setNames(new_name, ugly_name)
cm

new_name1 <- c(paste0("CPI_LDV", 1:12),
               paste0("Con_LDV", 1:12),"ppi","unemploy")


found <- list()
for (i in 1:length(general.cpi)) {
  found[[i]] <-
    match(names(general.cpi[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(general.cpi[[i]]$coefficients)[names(general.cpi[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
vars.order <- new_name1


# F -----------------------------------------------------------------------

linearHypothesis()
# -------------------------------------------------------------------------


star(general.cpi)

stargazer(general.cpi,
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c("CPI","CPI(AIC)","CPI(Stepwise)","CPI(Final)"),
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
          out = "output, unemployment, PPI and hourly wages/general AIC stepwsie P.html",
          type = "text"
)   

names(general.cpi) <- c("CPI","CPI(AIC)","CPI(Stepwise)","CPI(Final)")

modelsummary(
  general.cpi,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  output = "output, unemployment, PPI and hourly wages/general AIC stepwsie .html",
  
  coef_omit = "Intercept"
)
