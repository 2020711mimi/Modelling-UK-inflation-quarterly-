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

# Data-------------------------------------------------------------------------
cpi_data <- rio::import("cpi ppi consumption unemployment/div.cpi97.csv")
ppi_data <- rio::import("cpi ppi consumption unemployment/ppi %97.csv")
unemploy <- rio::import("cpi ppi consumption unemployment/unemployment 97-19%.csv")
consumption <- rio::import("cpi ppi consumption unemployment/consumption monthly data change %.csv")

cpi <- ts(cpi_data[,2:length(cpi_data)],start = c(1997,03),end = c(2019,12),frequency = 12)
ppi <- ts(ppi_data[,3],start = c(1997,03),end = c(2019,12),frequency = 12)
unemploy <- ts(unemploy[,2],start = c(1997,03),end = c(2019,12),frequency = 12)
consum <- ts(consumption[,2:length(consumption)],start = c(1997,03),end = c(2019,12),frequency = 12)

#crea a time index for dummy create

time1 = seq.Date(
  from = as.Date("1997/03/01", format = "%Y/%m/%d"),
  by = "month",
  length.out = 274
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

# ADF-------------------------------------------------------------------------
# lapply(consum, adf.test)
# lapply(cpi,adf.test)
# lapply(list(ppi,unemploy), adf.test)
# Regression-------------------------------------------------------------------------

#cpi

reg.cpi <- 
  dynlm(
    reformulate(
      c(
        paste0("L(cpi[,1],", 1:12, ")") ,
        paste0("L(consum[,1],", 1:12, ")"),
        "ppi","unemploy",
        "VAT1",
        "VAT2",
        "VAT3",
        "Recession",
        "D.",
        "Trend"
      ),
      "cpi[,1]"
    )
    ,
    start = c(1998, 03),
    end = c(2019, 12)
  )

#12 DIVS

 div<- cpi[,2:13]
 demand <- consum[,2:13] 

model.div <- vector("list", 12)

for (i in 1:12) {
  form <- reformulate(
    c(
      paste0("L(div[,i],", 1:12, ")") ,
      paste0("L(cpi[,1],", 1:12, ")") ,
      paste0("L(demand[,i],", 1:12, ")"),
      "ppi","unemploy",
      "VAT1",
      "VAT2",
      "VAT3",
      "Recession",
      "D.",
      "Trend"
    ),
    "div[,i]"
  )
  
  model.div[[i]] <- dynlm(form,start=c(1998,03),end = c(2019,12))
    
  
}
#
# Stepwise AIC----------------------------------------------------------------

#step for cpi
step_CPI <- step(
  reg.cpi,
  scope = list(
    lower = cpi[, 1] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
    ,
    upper = reg.cpi
  )
)

#step divs
step_divs <- list()

for (i in 1:12) {
  step_divs[i] <- step(
    model.div[[i]],
    scope = list(
      lower = div[, i] ~  VAT1 + VAT2 + VAT3 + Recession + D. + Trend
      ,
      upper = model.div[[i]]
    )
  )
  
}

#save output to y1:y12
for (i in 3:nrow(CPI)) {
  assign(paste0("y", i), step(
    model.div[[i]],
    scope = list(
      lower = CPI[, i] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
      ,
      upper = model.div[[i]]
    )
  ))
  
}





# still insignificant -------------------------------------------------------------------------
source("cpi ppi consumption unemployment/new ldv.R")
all.coef.name <- names(model.div[[2]]$coefficients)

'%!in%' <- function(x, y)
  ! ('%in%'(x, y))

LDV.NAME <- all.coef.name[(which(all.coef.name %!in% LDV))]
LDV.NAME <- LDV.NAME[-1]
LDV.NAME

ynew_list <- list(mget(paste0("y", 1:12)))

#coef name with p value >0.1

#extract results table
insiginifcant.coef <-
  lapply(ynew_list[[1]], function(x)
    names(which(coef(summary(
      x
    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV.NAME])

insiginifcant.coef
new.insignificant <- insiginifcant.coef


#all significant divs: 01,04,07，12
all_sig_divs <- which(new.insignificant %in% NA)
all_sig_divs
insig_divs <- c(which(new.insignificant %!in% NA))
insig_divs

#re estimate divs" 1:12 

y_sig_divs_list <- ynew_list[[1]][c(insig_divs)]

coef_not_significant <- insiginifcant.coef[c(insig_divs)]


coef_without_insigni <- list()

for (i in 1:length(y_sig_divs_list)) {
  coef_without_insigni[[i]] <-
    print(names(y_sig_divs_list[[i]][[1]])[names(y_sig_divs_list[[i]][[1]]) %in% LDV.NAME &
                                             names(y_sig_divs_list[[i]][[1]]) %!in% coef_not_significant[[i]]])
}

var <- lapply(coef_without_insigni, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))

all_sig_formula <- lapply(var, function(x)
  as.formula(paste(
    "div[, i] ~",
    paste(x, collapse = "+")))
  
  )

all__formula <- list()
all__formula[c(1:12)] <- all_sig_formula
all__formula
all_sig_output <- list()

for (i in 1:12) {
  all_sig_output[[i]] <-
    dynlm(all__formula[[i]],
          start = c(1998, 03),
          end = c(2019, 12))
  
}
