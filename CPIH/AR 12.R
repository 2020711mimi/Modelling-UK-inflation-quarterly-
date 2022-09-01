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

df.all <- import("CPIH/CPI and 12 divs.csv")

for (i in 1:12) {
  assign(paste0("X",i),lag(df.all,i))
  
}

for (i in 1:12) {
  assign(paste0("LDV",i),tail(get(paste0("X",i)),324))
  
}

CPI <- tail(df.all,324)


time1 = seq.Date(
  from = as.Date("1993/01/01", format = "%Y/%m/%d"),
  by = "month",
  length.out =324
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
for (i in 2:ncol(CPI)) {
  form <-
    reformulate(
      c(
        paste0("LDV", 1:12, "[, 2]") ,
        paste0("LDV", 1:12, "[, i]") ,
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

source("output, unemployment, PPI and hourly wages/stargazer and modelsummary function.R")



# -------------------------------------------------------------------------



#step divs
step_divs <- list()

for (i in 2:ncol(CPI)) {
  step_divs[i] <- step(
    model.div[[i]],
    scope = list(
      lower = CPI[, i] ~  VAT1 + VAT2 + VAT3 + Recession + D. + Trend
      ,
      upper = model.div[[i]]
    )
  )
  
}


for (i in 2:ncol(CPI)) {
  assign(paste0("y", i), step(
    model.div[[i]],
    scope = list(
      lower = CPI[, i] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
      ,
      upper = model.div[[i]]
    )
  ))
  
}
# -------------------------------------------------------------------------
y.list1 <- mget(paste0("y",2:14))
# stage 1 -----------------------------------------------------------------


all.coef.name <- names(model.div[[3]]$coefficients)

LDV <- c(        paste0("LDV", 1:12, "[, 2]") ,
                 paste0("LDV", 1:12, "[, i]") )
LDV


# -------------------------------------------------------------------------


insiginifcant.coef_stage_2 <-
  lapply(y.list1, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV])

insiginifcant.coef_stage_2

insig_divs_stage2 <- which(lapply(insiginifcant.coef_stage_2, FUN = length ) != 0   )

insig_divs_stage2





for (i in insig_divs_stage2) {
  print(  linearHypothesis(y.list1[[i]],insiginifcant.coef_stage_2[[i]]) )
}



coef_without_insigni_stage2 <- list()
for (i in 1:length(y.list1)) {
  coef_without_insigni_stage2[[i]] <-
    print(names(y.list1[[i]][[1]])[names(y.list1[[i]][[1]]) %in% LDV &
                                            names(y.list1[[i]][[1]]) %!in% insiginifcant.coef_stage_2[[i]]])
}
coef_without_insigni_stage2



var_stage2 <- lapply(coef_without_insigni_stage2, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage2


all_sig_formula_stage2 <- lapply(var_stage2, function(x)
  as.formula(paste(
    "CPI[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage2 <- list()
all__formula_stage2[c(2:14)] <- all_sig_formula_stage2


y.list1_stage2 <- list()

for (i in 2:14) {
  y.list1_stage2[[i]] <-lm(all__formula_stage2[[i]])
  
}

# stage2 -------------------------------------------------------------------------
star(y.list1_stage2[2:14])
y.list1_stage2[sapply(y.list1_stage2,is.null)] <-NULL 

insiginifcant.coef_stage_3 <-
  lapply(y.list1_stage2, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV])

insiginifcant.coef_stage_3

insig_divs_stage3 <- which(lapply(insiginifcant.coef_stage_3, FUN = length ) != 0   )

insig_divs_stage3





for (i in insig_divs_stage3) {
  print(  linearHypothesis(y.list1_stage2[[i]],insiginifcant.coef_stage_3[[i]]) )
}



coef_without_insigni_stage3 <- list()
for (i in 1:length(y.list1_stage2)) {
  coef_without_insigni_stage3[[i]] <-
    print(names(y.list1_stage2[[i]][[1]])[names(y.list1_stage2[[i]][[1]]) %in% LDV &
                                     names(y.list1_stage2[[i]][[1]]) %!in% insiginifcant.coef_stage_3[[i]]])
}
coef_without_insigni_stage3



var_stage3 <- lapply(coef_without_insigni_stage3, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage3


all_sig_formula_stage3 <- lapply(var_stage3, function(x)
  as.formula(paste(
    "CPI[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage3 <- list()
all__formula_stage3[c(2:14)] <- all_sig_formula_stage3


y.list1_stage3 <- list()

for (i in 2:14) {
  y.list1_stage3[[i]] <-lm(all__formula_stage3[[i]])
  
}


# stage 3-------------------------------------------------------------------------
y.list1_stage3[sapply(y.list1_stage3,is.null)] <-NULL 

insiginifcant.coef_stage_4 <-
  lapply(y.list1_stage3, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV])

insiginifcant.coef_stage_4

insig_divs_stage3 <- which(lapply(insiginifcant.coef_stage_3, FUN = length ) != 0   )

insig_divs_stage3







#star -------------------------------------------------------------------------


# -------------------------------------------------------------------------

#y.list1_stage3
#y.list1_stage2
#y.list1
#model.div

ugly_name <-  c(
  paste0("LDV",1:12, "[, 2]"),
  paste0("LDV",1:12, "[, i]")
)
ugly_name

new_name <- c(paste0("CPIH_LDV", 1:12),
              paste0("LDV", 1:12)
)
new_name

search_for_these <- ugly_name
replace_with_these <- new_name

cm <- setNames(new_name, ugly_name)
cm
tl.list <- list(y.list1, y.list1_stage2,y.list1_stage3,model.div[2:14])
found <- list()
for (i in 1:4) {
  for(j in 1:13 ){
  found[[i]] <-
    match(names(tl.list[[i]][[j]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(tl.list[[i]][[j]]$coefficients)[names(tl.list[[i]][[j]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
}
vars.order <- new_name

# final-------------------------------------------------------------------------


stargazer(tl.list[3],
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(colnames(df.all)[2:14]),
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
          out = "CPIH/CPIH 13 simply P.html",
          type = "text"
)

names(tl.list[[3]]) <- c(colnames(df.all)[2:14])

modelsummary(tl.list[[3]],  
             coef_map = new_name,
             statistic = NULL,
             stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
             fmt = "%.2f",
             coef_omit = "Intercept",
             output = "CPIH/CPIH 13 simply .html"
)

# START -------------------------------------------------------------------

stargazer(tl.list[4],
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(colnames(df.all)[2:14]),
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
          out = "CPIH/CPIH 13 START P.html",
          type = "text"
)

names(tl.list[[4]]) <- c(colnames(df.all)[2:14])

modelsummary(tl.list[[4]],  
             coef_map = new_name,
             statistic = NULL,
             stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
             fmt = "%.2f",
             coef_omit = "Intercept",
             output = "CPIH/CPIH 13 start .html"
)


# cpi ---------------------------------------------------------------------
cpi.list<- list(tl.list[[4]]$CPI,tl.list[[3]]$CPI)

stargazer(cpi.list,
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(colnames(df.all)[2:14]),
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
          out = "CPIH/CPIH CPI P.html",
          type = "text"
)


modelsummary(cpi.list,  
             coef_map = new_name,
             statistic = NULL,
             stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
             fmt = "%.2f",
             coef_omit = "Intercept",
             output = "CPIH/CPIH CPI  .html"
)


