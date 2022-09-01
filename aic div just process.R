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
#  # continued stepwise based on P value until all significant ---------------------------------------------------
#insignificant & F test

#p value greater than 0.1
y.list <- list(mget(paste0("y", 2:13)))
#extract formual after AIC step
fff <- lapply(y.list[[1]], function(x)
  x[["call"]])
fff[2:13] <- lapply(fff, function(x)
  as.formula(x))

#step2 : re-estimate, variables is remaining after first step
mylist <- list()
for (i in 2:13) {
  mylist[[i]] <-
    dynlm(fff[[i]], start = c(1993, 1), end = c(2019, 12))
  
}
mylist <- mylist[-1]

source("stepwise by P value function.R")
keep.dummies <-
  c("VAT1" , "VAT2" , "VAT3" , "Recession" , " D." , "Trend")
output_03_08 <- lapply(mylist[], function(x)
  model.select(
    x,
    keep = keep.dummies,
    sig = 0.10,
    verbose = F
  ))


all.coef.name <- names(model.div[[2]]$coefficients)
'%!in%' <- function(x, y)
  ! ('%in%'(x, y))

source("LDV.R")
LDV.NAME <- all.coef.name[(which(all.coef.name %!in% LDV))]
LDV.NAME <- LDV.NAME[-1]
LDV.NAME

ynew_list <- list(mget(paste0("y", 2:13)))
lapply(ynew_list[[1]], function(x)
  summary(x)[4])

#coef name with p value >0.1

#extract coef p value>0,1 from 12 divs
insiginifcant.coef <-
  lapply(ynew_list[[1]], function(x)
    names(which(coef(summary(
      x
    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV.NAME])

#relace characte(0) with na
new.insignificant <-
  lapply(insiginifcant.coef, function(x)
    if (identical(x, character(0)))
      NA_character_
    else
      x)

#all significant divs: 01,04,07，12
all_sig_divs <- which(new.insignificant %in% NA)
insig_divs <- c(which(new.insignificant %!in% NA))
insig_divs
# #F-TEST -----------------------------------------------------------------
f.pvalue <- list()
for (i in insig_divs) {
  f.pvalue[[i]] <-
    (linearHypothesis(ynew_list[[1]][[i]], new.insignificant[[i]]) [6])
}



hypothesis <- list()

for (i in insig_divs) {
  hypothesis[i] <-
    (attributes(linearHypothesis(ynew_list[[1]][[i]], new.insignificant[[i]]))["heading"])
}

formula_insig_divs <- fff[c(2, 3, 5, 6, 8, 9, 10, 11)]
vars_insig_divs <- new.insignificant[c(2, 3, 5, 6, 8, 9, 10, 11)]
sapply(list(formula_insig_divs, vars_insig_divs), length)






var <-
  names(y2$coefficients)[names(y2$coefficients) %in% ugly_name &
                           names(y2$coefficients) %!in% insiginifcant.coef[2]]
y_sig_divs_list <- ynew_list[[1]][c(insig_divs)]
coef_not_significant <- insiginifcant.coef[c(insig_divs)]
coef_not_significant

length(y_sig_divs_list) == length(coef_not_significant)
names(y_sig_divs_list[[1]][[1]])

coef_without_insigni <- list()

for (i in 1:length(y_sig_divs_list)) {
  coef_without_insigni[[i]] <-
    print(names(y_sig_divs_list[[i]][[1]])[names(y_sig_divs_list[[i]][[1]]) %in% ugly_name &
                                             names(y_sig_divs_list[[i]][[1]]) %!in% coef_not_significant[[i]]])
}

var <- lapply(coef_without_insigni, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))

var

all_sig_formula <- lapply(var, function(x)
  as.formula(paste(
    "ts.divs.percent[, i] ~",
    paste(x, collapse = "+")
    
  )))


#formula has i, i must match(insig_divs+1) 3  4  6  7  9 10 11 12
all_sig_output <- list()

insig_inde <- insig_divs + 1
all__formula <- list()
all__formula[c(insig_inde)] <- all_sig_formula
all__formula
for (i in insig_inde) {
  all_sig_output[[i]] <-
    dynlm(all__formula[[i]],
          start = c(1993, 1),
          end = c(2019, 12))
}

# 03 08 3rd round ---------------------------------------------------------
# 
all_sig_output




source("f-test stepwsie.R")

all_sig_output[sapply(all_sig_output, is.null)] <- NULL
#
length(all_sig_output)

round_2_insig_var<- lapply(all_sig_output, function(x)
  names(which(coef(summary(
    x
  ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV.NAME])
insig_divs
#2  3  5  6  8  9 10 11] 03div is the 2rd, 08 is 5th
#10% criteria
#03:"L(ts.divs.percent[, 1], 1)" 03CF for CPI-LDV 1
#08:"L(ts.divs.percent[, 1], 5)" CPI LDV 5

(linearHypothesis(all_sig_output[[2]], round_2_insig_var[[2]]))#03 CPI-LDV1=0, p=0.1218
(linearHypothesis(all_sig_output[[5]], round_2_insig_var[[5]]))#08 CPI-LDV5=0, p=0.1029

  div03.formu <-
    reformulate(
      c(
        paste0("L(ts.divs.percent[,4],", 1:12, ")"),
        paste0("L(ts.divs.percent[,1],", 1:12, ")") ,
        "VAT1",
        "VAT2",
        "VAT3",
        "Recession",
        "D.",
        "Trend"
      ),
      "ts.divs.percent[,4]"
    )
  div03 <-  dynlm(div03.formu, start = c(1993, 1), end = c(2019, 12))
  div03 |> summary()
modelsummary(div03)

div03.1.formu <-
  reformulate(
    c(
      paste0("L(ts.divs.percent[,4],", c(1:6,9,12), ")"),
      paste0("L(ts.divs.percent[,1],", 1:12, ")") ,
      "VAT1",
      "VAT2",
      "VAT3",
      "Recession",
      "D.",
      "Trend"
    ),
    "ts.divs.percent[,4]"
  )
