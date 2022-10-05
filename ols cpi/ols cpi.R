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

Sys.setlocale("LC_TIME", "English")
rm(list = ls())
options(scipen = 200)
# Data -------------------------------------------------------------------------

#read data
divs <- read_excel("cpi data/CPI 12divs.xlsx")
#rename cols


ts.divs <-
  ts(divs[2:length(divs)], start = c(1988, 1), frequency = 12)

#convert % change

ts.divs.percent <- 100 * (ts.divs / stats::lag(ts.divs, -1) - 1)

#

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



# Stepwise ----------------------------------------------------------------

step(reg.cpi, direction = "backward")

step_CPI <- step(
  reg.cpi,
  scope = list(
    lower = ts.divs.percent[, 1] ~ VAT1 + VAT2 + VAT3 + Recession + D. + Trend
    ,
    upper = reg.cpi
  )
)

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
y <- paste0("y", 2:13, sep = "", collapse = ",")
noquote(y)


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
  report=('vc*p'),  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(div.name),
  summary = FALSE,
  se = NULL
)


# -------------------------------------------------------------------------

ugly_name <-  c(
  paste0("L(ts.divs.percent[, 1], ", 1:12, ")") 
)
ugly_name

new_name <- c(paste0("CPI_LDV", 1:12)
)
new_name

search_for_these <- ugly_name
replace_with_these <- new_name

cm <- setNames(new_name, ugly_name)
cm




cpi.list <- list(reg.cpi,step_CPI)
names(cpi.list) <- c("CPI","CPI(AICstepwise)")

found <- list()
for (i in 1:2) {
  found[[i]] <-
    match(names(cpi.list[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(cpi.list[[i]]$coefficients)[names(cpi.list[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
vars.order <- new_name

modelsummary(cpi.list,  
             coef_map = new_name,
             statistic = NULL,
             stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
             fmt = "%.2f",
             coef_omit = "Intercept",
             
)

stargazer(
  cpi.list,
  title = "Regression comparision",
  type = "text",
  star.char =  c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes.append = FALSE,
  notes = " * p<0.05; ** p<0.01 ",
  dep.var.labels.include = FALSE,
  report=('vcs*'),  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  column.labels = c("CPI","CPI(AICstepwise)"),
  se=lapply(cpi.list, function(x) summary(x)$coef[,4])
  #out = "table/p value same line cpi.html"
)

stargazer(cpi.list,
  title = "Regression comparision",
  type = "text",
  star.char =  c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes.append = FALSE,
  notes = " * p<0.05; ** p<0.01 ",
  dep.var.labels.include = FALSE,
  report=('vc*p'),  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  se = NULL,
  #out = "table/p value cpi.html"

)
library(texreg)
texreg::screenreg(cpi.list, single.row=TRUE, 
                  #reorder.coef=c(2:3, 1),
                  #custom.model.names=c(div.name),
                  override.se=lapply(cpi.list, function(x) summary(x)$coef[,4]),
                  override.pvalues=lapply(cpi.list, function(x) summary(x)$coef[,4]),
                  digits=3
)
