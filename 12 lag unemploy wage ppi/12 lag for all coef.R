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
        paste0("Con_LDV", 1:12, "[, i]") ,
        paste0("PPI_LDV", 1:12, "[, 3]") ,
        paste0("U_LDV", 1:12, "[, 2]") ,
        paste0("WAGE_LDV", 1:12, "[, 2]") ,
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


# AIC ---------------------------------------------------------------------

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


#save output to y1:y12
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

star(mget(paste0("y",2:14)))
y.list1 <- mget(paste0("y",2:14))
# stage 1 -----------------------------------------------------------------


all.coef.name <- names(model.div[[3]]$coefficients)

all.coef.name
LDV <-       c(
  paste0("CPI_LDV", 1:12, "[, 2]") ,
  paste0("CPI_LDV", 1:12, "[, i]") ,
  paste0("Con_LDV", 1:12, "[, i]") ,
  paste0("PPI_LDV", 1:12, "[, 3]") ,
  paste0("U_LDV", 1:12, "[, 2]") ,
  paste0("WAGE_LDV", 1:12, "[, 2]") )
LDV



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




# stage3 -------------------------------------------------------------------------
y.list1_stage3[sapply(y.list1_stage3,is.null)] <-NULL 

insiginifcant.coef_stage_4 <-
  lapply(y.list1_stage3, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV])

insiginifcant.coef_stage_4

insig_divs_stage4 <- which(lapply(insiginifcant.coef_stage_4, FUN = length ) != 0   )

insig_divs_stage4





for (i in insig_divs_stage4) {
  print(  linearHypothesis(y.list1_stage3[[i]],insiginifcant.coef_stage_4[[i]]) )
}



coef_without_insigni_stage4 <- list()
for (i in 1:length(y.list1_stage3)) {
  coef_without_insigni_stage4[[i]] <-
    print(names(y.list1_stage3[[i]][[1]])[names(y.list1_stage3[[i]][[1]]) %in% LDV &
                                            names(y.list1_stage3[[i]][[1]]) %!in% insiginifcant.coef_stage_4[[i]]])
}
coef_without_insigni_stage4



var_stage4 <- lapply(coef_without_insigni_stage4, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage4


all_sig_formula_stage4 <- lapply(var_stage4, function(x)
  as.formula(paste(
    "CPI[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage4 <- list()
all__formula_stage4[c(2:14)] <- all_sig_formula_stage4


y.list1_stage4 <- list()

for (i in 2:14) {
  y.list1_stage4[[i]] <-lm(all__formula_stage4[[i]])
  
}

# stage4 -------------------------------------------------------------------------
y.list1_stage4[sapply(y.list1_stage4,is.null)] <-NULL 

insiginifcant.coef_stage_5 <-
  lapply(y.list1_stage4, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV])

insiginifcant.coef_stage_5

insig_divs_stage5 <- which(lapply(insiginifcant.coef_stage_5, FUN = length ) != 0   )

insig_divs_stage5





for (i in insig_divs_stage5) {
  print(  linearHypothesis(y.list1_stage4[[i]],insiginifcant.coef_stage_5[[i]]) )
}



coef_without_insigni_stage5 <- list()
for (i in 1:length(y.list1_stage4)) {
  coef_without_insigni_stage5[[i]] <-
    print(names(y.list1_stage4[[i]][[1]])[names(y.list1_stage4[[i]][[1]]) %in% LDV &
                                            names(y.list1_stage4[[i]][[1]]) %!in% insiginifcant.coef_stage_5[[i]]])
}
coef_without_insigni_stage5



var_stage5 <- lapply(coef_without_insigni_stage5, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage5


all_sig_formula_stage5 <- lapply(var_stage5, function(x)
  as.formula(paste(
    "CPI[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage5 <- list()
all__formula_stage5[c(2:14)] <- all_sig_formula_stage5


y.list1_stage5 <- list()

for (i in 2:14) {
  y.list1_stage5[[i]] <-lm(all__formula_stage5[[i]])
  
}

# stage5 -------------------------------------------------------------------------
y.list1_stage5[sapply(y.list1_stage5,is.null)] <-NULL 

insiginifcant.coef_stage_6 <-
  lapply(y.list1_stage5, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV])

insiginifcant.coef_stage_6

insig_divs_stage6 <- which(lapply(insiginifcant.coef_stage_6, FUN = length ) != 0   )

insig_divs_stage6





 for (i in insig_divs_stage6) {
  print(  linearHypothesis(y.list1_stage5[[i]],insiginifcant.coef_stage_6[[i]]) )
}



coef_without_insigni_stage6 <- list()
for (i in 1:length(y.list1_stage5)) {
  coef_without_insigni_stage6[[i]] <-
    print(names(y.list1_stage5[[i]][[1]])[names(y.list1_stage5[[i]][[1]]) %in% LDV &
                                            names(y.list1_stage5[[i]][[1]]) %!in% insiginifcant.coef_stage_6[[i]]])
}
coef_without_insigni_stage6



var_stage6 <- lapply(coef_without_insigni_stage6, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage6


all_sig_formula_stage6 <- lapply(var_stage6, function(x)
  as.formula(paste(
    "CPI[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage6 <- list()
all__formula_stage6[c(2:14)] <- all_sig_formula_stage6


y.list1_stage6 <- list()

for (i in 2:14) {
  y.list1_stage6[[i]] <-lm(all__formula_stage6[[i]])
  
}

# stage6 -------------------------------------------------------------------------
y.list1_stage6[sapply(y.list1_stage6,is.null)] <-NULL 

insiginifcant.coef_stage_7 <-
  lapply(y.list1_stage6, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV])

insiginifcant.coef_stage_7

insig_divs_stage7 <- which(lapply(insiginifcant.coef_stage_7, FUN = length ) != 0   )

insig_divs_stage7





for (i in insig_divs_stage7) {
  print(  linearHypothesis(y.list1_stage6[[i]],insiginifcant.coef_stage_7[[i]]) )
}



coef_without_insigni_stage7 <- list()
for (i in 1:length(y.list1_stage6)) {
  coef_without_insigni_stage7[[i]] <-
    print(names(y.list1_stage6[[i]][[1]])[names(y.list1_stage6[[i]][[1]]) %in% LDV &
                                            names(y.list1_stage6[[i]][[1]]) %!in% insiginifcant.coef_stage_7[[i]]])
}
coef_without_insigni_stage7



var_stage7 <- lapply(coef_without_insigni_stage7, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage7


all_sig_formula_stage7 <- lapply(var_stage7, function(x)
  as.formula(paste(
    "CPI[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage7 <- list()
all__formula_stage7[c(2:14)] <- all_sig_formula_stage7


y.list1_stage7 <- list()

for (i in 2:14) {
  y.list1_stage7[[i]] <-lm(all__formula_stage7[[i]])
  
}

# stage7 -------------------------------------------------------------------------
y.list1_stage7[sapply(y.list1_stage7,is.null)] <-NULL 

insiginifcant.coef_stage_8 <-
  lapply(y.list1_stage7, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV])

insiginifcant.coef_stage_8

insig_divs_stage8 <- which(lapply(insiginifcant.coef_stage_8, FUN = length ) != 0   )

insig_divs_stage8





for (i in insig_divs_stage8) {
  print(  linearHypothesis(y.list1_stage7[[i]],insiginifcant.coef_stage_8[[i]]) )
}



coef_without_insigni_stage8 <- list()
for (i in 1:length(y.list1_stage7)) {
  coef_without_insigni_stage8[[i]] <-
    print(names(y.list1_stage7[[i]][[1]])[names(y.list1_stage7[[i]][[1]]) %in% LDV &
                                            names(y.list1_stage7[[i]][[1]]) %!in% insiginifcant.coef_stage_8[[i]]])
}
coef_without_insigni_stage8



var_stage8 <- lapply(coef_without_insigni_stage8, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage8


all_sig_formula_stage8 <- lapply(var_stage8, function(x)
  as.formula(paste(
    "CPI[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage8 <- list()
all__formula_stage8[c(2:14)] <- all_sig_formula_stage8


y.list1_stage8 <- list()

for (i in 2:14) {
  y.list1_stage8[[i]] <-lm(all__formula_stage8[[i]])
  
}


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

# -------------------------------------------------------------------------


tl.list <- list(model.div[2:14],y.list1, y.list1_stage2,y.list1_stage3,y.list1_stage4,y.list1_stage5,y.list1_stage6,y.list1_stage7)

found <- list()
for (i in 1:8) {
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

# cpi ---------------------------------------------------------------------
cpi.list<- c(tl.list[[1]][1],tl.list[[2]][1],tl.list[[3]][1],tl.list[[4]][1])

omit.coef<- all.coef.name[ all.coef.name %!in% LDV]
stargazer(cpi.list,
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c("CPI","CPI(AIC)","CPI(Stage1)","CPI(Final)"),
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
          out = "12 lag unemploy wage ppi/cpi.html",
          type = "text"
)

stargazer(cpi.list,
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c("CPI","CPI(AIC)","CPI(Stage1)","CPI(Final)"),
          omit = omit.coef,
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
          out = "12 lag unemploy wage ppi/cpi s.html",
          type = "text"
)


modelsummary(cpi.list,  
             coef_map = new_name,
             statistic = NULL,
             stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
             fmt = "%.2f",
             coef_omit = "Intercept",
             output = "12 lag unemploy wage ppi/test.html"
)

# -------------------------------------------------------------------------
df.all <- import("CPIH/CPI and 12 divs.csv")

stargazer(tl.list[8],
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
          out = "12 lag unemploy wage ppi/div .html",
          type = "text"
)

names(tl.list[[8]]) <- c(colnames(df.all)[2:14])

modelsummary(tl.list[[8]],  
             coef_map = new_name,
             statistic = NULL,
             stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
             fmt = "%.2f",
             coef_omit = "Intercept",
              output = "12 lag unemploy wage ppi/div s.html"
)
