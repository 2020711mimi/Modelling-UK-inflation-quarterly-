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

#
div.name <-
  c("FB",
    "AT",
    "CF",
    "HW",
    "FH",
    "HL",
    "TR",
    "CM",
    "RC",
    "ED" ,
    "RH",
    "MS")

#write out
stargazer(
  model.div[2:13],
  title = "Regression comparision",
  type = "text",
  star.char =  c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes.append = FALSE,
  notes = " * p<0.05; ** p<0.01 ",
  dep.var.labels.include = FALSE,
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(div.name),
  summary = FALSE,
  se = NULL
)

stargazer(
  reg.cpi,
  title = "Regression comparision",
  type = "text",
  star.char = c("*"),
  star.cutoffs = c(0.01),
  notes = c(" * p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(div.name),
  summary = FALSE,
  se = NULL
)



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

#write out
stargazer(
  reg.cpi,
  step_CPI,
  title = "Regression comparision",
  type = "text",
  star.char =  c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes.append = FALSE,
  notes = " * p<0.05; ** p<0.01 ",
  dep.var.labels.include = FALSE,
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(div.name),
  summary = FALSE,
  se = NULL
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



#re order and name coef
var.order = c(names(model.div[[2]]$coefficients))
LDV <- c(
  "Intercept",
  "LDV1",
  "LDV2",
  "LDV3",
  "LDV4",
  "LDV5",
  "LDV6",
  "LDV7",
  "LDV8",
  "LDV9",
  "LDV10",
  "LDV11",
  "LDV12",
  "CPI-LDV1",
  " CPI-LDV2",
  " CPI-LDV3",
  " CPI-LDV4",
  " CPI-LDV5",
  " CPI-LDV6",
  " CPI-LDV7",
  "CPI-LDV8",
  " CPI-LDV9",
  "CPI-LDV10",
  "CPI-LDV11",
  "CPI-LDV12",
  "VAT1",
  "VAT2",
  "VAT3",
  "Recession"   ,
  "D.August"         ,
  "D.December"    ,
  "D.February",
  "D.January",
  "D.July" ,
  "D.June",
  "D.March"  ,
  "D.May"    ,
  "D.November"         ,
  "D.October"   ,
  "D.September"      ,
  "Trend"
)









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

# stepwise divs
#

stargazer(
  mget(paste0('y', 2:13)),
  title = "Regression comparision",
  type = "text",
  star.char =  c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes.append = FALSE,
  notes = " * p<0.05; ** p<0.01 ",
  dep.var.labels.include = FALSE,
  report = ('vc*p'),
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(div.name),
  summary = FALSE,
  se = NULL
)

#  # continued stepwise based on P value until all significant ---------------------------------------------------
#insignificant & F test

#p value greater than 0.1
y.list <- list(mget(paste0("y", 2:13)))
fff <- lapply(y.list[[1]], function(x)
  x[["call"]])
fff[2:13] <- lapply(fff, function(x)
  as.formula(x))

mylist <- list()
for (i in 2:13) {
  mylist[[i]] <-
    dynlm(fff[[i]], start = c(1993, 1), end = c(2019, 12))
  
}
mylist <- mylist[-1]

source("stepwise by P value function.R")
keep.dummies <-
  c("VAT1" , "VAT2" , "VAT3" , "Recession" , " D." , "Trend")
ms <- lapply(mylist[], function(x)
  model.select(
    x,
    keep = keep.dummies,
    sig = 0.10,
    verbose = F
  ))
# joint f test

all.coef.name <- names(model.div[[2]]$coefficients)
'%!in%' <- function(x, y)
  ! ('%in%'(x, y))
LDV.NAME <- all.coef.name[(which(all.coef.name %!in% LDV))]
LDV.NAME <- LDV.NAME[-1]
LDV.NAME

ynew_list <- list(mget(paste0("y", 2:13)))
lapply(ynew_list[[1]], function(x)
  summary(x)[4])

#coef name with p value >0.1

#extract results table
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

new.insignificant
#all significant divs: 01,04,07，12
all_sig_divs <- which(new.insignificant %in% NA)
insig_divs <- c(which(new.insignificant %!in% NA))
insig_divs

#re estimate divs" 2,3,5,6,8,9,10,11

# #F-TEST -----------------------------------------------------------------


div02 <-
  linearHypothesis(ynew_list[[1]][[2]], new.insignificant[[2]])

linearHypothesis(ynew_list[[1]][[3]], new.insignificant[[3]])

f.pvalue <- list()
for (i in insig_divs) {
  f.pvalue[[i]] <-
    (linearHypothesis(ynew_list[[1]][[i]], new.insignificant[[i]]) [6])
}
f.pvalue



hypothesis <- list()

for (i in insig_divs) {
  hypothesis[i] <-
    (attributes(linearHypothesis(ynew_list[[1]][[i]], new.insignificant[[i]]))["heading"])
}

hypothesis

#[,i]== LDV, [,1]== CPI-LDV


without_insig_coef_02at <-
  dynlm(
    ts.divs.percent[, 3] ~ L(ts.divs.percent[, 3], 1) + L(ts.divs.percent[,
                                                                          3], 2) + L(ts.divs.percent[, 3], 10) + L(ts.divs.percent[,
                                                                                                                                   3], 12) + L(ts.divs.percent[, 1], 3)  + L(ts.divs.percent[, 1], 6) + L(ts.divs.percent[,
                                                                                                                                                                                                                          1], 7) + VAT1 + VAT2 + VAT3 + Recession + D. + Trend,
    start = c(1993, 1),
    end = c(2019, 12)
  )



formula_insig_divs <- fff[c(2, 3, 5, 6, 8, 9, 10, 11)]
vars_insig_divs <- new.insignificant[c(2, 3, 5, 6, 8, 9, 10, 11)]
sapply(list(formula_insig_divs, vars_insig_divs), length)






y_sig_divs_list <- ynew_list[[1]][c(insig_divs)]
y_sig_divs_list
coef_not_significant <- insiginifcant.coef[c(insig_divs)]
coef_not_significant


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

insig_inde <- insig_divs + 1
all__formula <- list()
all__formula[c(insig_inde)] <- all_sig_formula
all__formula
all_sig_output <- list()

for (i in insig_inde) {
  all_sig_output[[i]] <-
    dynlm(all__formula[[i]],
          start = c(1993, 1),
          end = c(2019, 12))
  
}

all_sig_output_nochange_name <- all_sig_output
search_for_these <- ugly_name
replace_with_these <- new_name


found <- list()
for (i in 1:length(all_sig_output)) {
  found[[i]] <-
    match(names(all_sig_output[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output[[i]]$coefficients)[names(all_sig_output[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
vars.order <- new_name
all_sig_output[sapply(all_sig_output, is.null)] <- NULL

stargazer(
  all_sig_output,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  
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
  type = "text"
)
#write out html WITH P
stargazer(
  all_sig_output,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  #out = "re run without insig coef in divs.html",
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
  type = "text"
)
# no P value
stargazer(
  all_sig_output,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  #out = "re run without insig coef no p in divs.html",
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  flip = FALSE,
  se = NULL,
  keep.stat = c("aic", "rsq", "n"),
  omit.table.layout = "n",
  type = "text"
)
  cm1<-
  c(
    'L(ts.divs.percent[, i], 1)' = "LDV1",
    'L(ts.divs.percent[, i], 2)' = "LDV2",
    'L(ts.divs.percent[, i], 3)' = "LDV3",
    'L(ts.divs.percent[, i], 4)' = "LDV4",
    'L(ts.divs.percent[, i], 5)' = "LDV5",
    'L(ts.divs.percent[, i], 6)' = "LDV6",
    'L(ts.divs.percent[, i], 7)' = "LDV7",
    'L(ts.divs.percent[, i], 8)' = "LDV8",
    'L(ts.divs.percent[, i], 9)' = "LDV9",
    'L(ts.divs.percent[, i], 10)' = "LDV10",
    'L(ts.divs.percent[, i], 11)' = "LDV11",
    'L(ts.divs.percent[, i], 12)' = "LDV12",
    'L(ts.divs.percent[, 1], 1)' = 'CPI-LDV1',
    'L(ts.divs.percent[, 1], 2)' = 'CPI-LDV2',
    'L(ts.divs.percent[, 1], 3)' = 'CPI-LDV3',
    'L(ts.divs.percent[, 1], 4)' = 'CPI-LDV4',
    'L(ts.divs.percent[, 1], 5)' = 'CPI-LDV5',
    'L(ts.divs.percent[, 1], 6)' = 'CPI-LDV6',
    'L(ts.divs.percent[, 1], 7)' = 'CPI-LDV7',
    'L(ts.divs.percent[, 1], 8)' = 'CPI-LDV8',
    'L(ts.divs.percent[, 1], 9)' = 'CPI-LDV9',
    'L(ts.divs.percent[, 1], 10)' = 'CPI-LDV10',
    'L(ts.divs.percent[, 1], 11)' = 'CPI-LDV11',
    'L(ts.divs.percent[, 1], 12)' = 'CPI-LDV12',
    "VAT1" = "VAT1" ,
    "VAT2" = "VAT2" ,
    "VAT3" = "VAT3" ,
    "Recession" = "Recession",
    "D.August" =  "D.August",
    "D.December" = "D.December",
    "D.February" = "D.February",
    "D.January" = "D.January",
    "D.July" = "D.July",
    "D.June" = "D.June",
    "D.March" = "D.March",
    "D.May" = "D.May",
    "D.November" = "D.November",
    "D.October" = "D.October",
    "D.September" = "D.September",
    '(Intercept)' = 'Constant'
    
    
  )
  all_sig_output_nochange_name[sapply(all_sig_output_nochange_name, is.null)] <- NULL
  
modelsummary(
  all_sig_output_nochange_name,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = cm1,
  fmt = "%.2f",
  #output = "AIC RE RUN WITHOUT insignificant DIVS.html"
)


# stepwise one by one output ----------------------------------------------
ms1 <- ms
#aic
modelsummary(
  ms1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = cm1,
  fmt = "%.2f",
  #output = "AIC RE RUN by onebyone DIVS.html"
)


found <- list()
for (i in 1:length(ms)) {
  found[[i]] <-
    match(names(ms[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(ms[[i]]$coefficients)[names(ms[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
ms[sapply(ms, is.null)] <- NULL

stargazer(ms,   order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          #out = "re run  one by one no p in divs.html",
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2,
          single.row = TRUE,
          model.numbers = FALSE,
          flip = FALSE,
          se = NULL,
          type = "text"
)



# modelsummary-------------------------------------------------------------------------

#a named list of length(models) vectors with names equal to the names of your coefficient estimates. See 'Examples' section below. Warning: since this list of vectors can include arbitrary strings or numbers, modelsummary cannot automatically calculate p values. The stars argument may thus use incorrect significance thresholds when vcov is a list of vectors.

# modelsummary function could deal with order of coef and rename in the output, but the p value calculations would be wrong
#order and rename coef


#compare cpi with stepwise
cpi.comare <- list(reg.cpi, step_CPI)

#cpi compare
modelsummary(
  cpi.comare,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = cm,
  fmt = "%.2f",
  #output = "modelsummary inflation persistence CPI compare.html"
)

#CPI compare p values
stargazer(
  cpi.comare,
  title = "Regression comparision",
  type = "text",
  dep.var.labels.include = FALSE,
  report = ('vc*p'),
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(div.name),
  summary = FALSE,
  #out = "modelsummary inflation persistence CPI compare with P value.html",
  se = NULL
)


#div general

modelsummary(
  model.div[2:13],
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = cm,
  fmt = "%.2f",
  #output = "modelsummary inflation persistence divs .html"
)

#div stepwise


modelsummary(
  mget(paste0("y", 2:13)),
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = cm,
  fmt = "%.2f",
  output = "modelsummary inflation persistence divs stepwise.html"
)

#div general p value

stargazer(
  model.div[2:13],
  title = "Regression comparision",
  type = "text",
  dep.var.labels.include = FALSE,
  report = ('vc*p'),
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(div.name),
  summary = FALSE,
  #out = "modelsummary inflation persistence DIV GENERAL with P value.html",
  se = NULL
)

#div stepwise p value

stargazer(
  mget(paste0("y", 2:13)),
  title = "Regression comparision",
  type = "text",
  dep.var.labels.include = FALSE,
  report = ('vc*p'),
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(div.name),
  summary = FALSE,
  #out = "modelsummary inflation persistence DIV stepwise with P value.html",
  se = NULL
)


# 06 remove 12 month lagged CPI  ------------------------------------------



test.06 <- dynlm(
  reformulate(
    c(
      paste0("L(ts.divs.percent[,7],", c(1:7, 11:12), ")"),
      paste0("L(ts.divs.percent[,1],", c(7:8), ")") ,
      "VAT1",
      "VAT2",
      "VAT3",
      "Recession",
      "D.",
      "Trend"
    ),
    "ts.divs.percent[,7]"
  ),
  start = c(1993, 1),
  end = c(2019, 12)
)
#06 only

modelsummary(
  list(test.06, y7),
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f"
  
)


# modelsummary(list(test.06,y7),
#              statistic = NULL,
#              stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
#              fmt = "%.2f"
#              ,             output = "test 06hl leave cpi lag12.html"
#             )
#             

#first exclude the 3 CPI variables and do an F test on that. If that passes, then do the same for the 12 month lag (assuming it has not become significant).
insiginifcant.coef[6]
y7
linearHypothesis(y7,insiginifcant.coef[[6]][3:5])
linearHypothesis(y7,insiginifcant.coef[[6]][3:4])
linearHypothesis(y7,insiginifcant.coef[[6]][c(3,5)])
linearHypothesis(y7,insiginifcant.coef[[6]][c(4,5)])
linearHypothesis(y7,insiginifcant.coef[[6]][c(3)])
linearHypothesis(y7,insiginifcant.coef[[6]][c(4)])
linearHypothesis(y7,insiginifcant.coef[[6]][c(5)])


for (i in insig_divs) {
  print  (linearHypothesis(ynew_list[[1]][[i]], new.insignificant[[i]]) )
}
coef_not_significant
