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


# data --------------------------------------------------------------------

df <- read.csv("cpi data/mom.csv")
which(df[,1] =="2005-01-01")#204
which(df[,1] =="2019-12-01")#383

df.12<- df[204:383,c(3:14)]


# description -------------------------------------------------------------
colnames(df.12) <-   c("01FB",
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
numSummary(df.12, 
           statistics = c("mean", "sd", "se(mean)", "skewness", 
                          "kurtosis")
)
#var
sapply(df.12, var)
library(RcmdrMisc)
fe<- numSummary(df.12[,c(1,4,7)], 
           statistics = c("mean", "sd", "se(mean)", "skewness", 
                          "kurtosis")
           )
fe.table<- data.frame(fe$table)
fe.table
fe.table$var <- unlist(sapply(df.12[,c(1,4,7)],var))

options(digits = 2)
write.csv(fe.table,"cpi data/fe.csv")

rest <- numSummary(df.12[,c(2:3,5:6,8:12)], 
                   statistics = c("mean", "sd", "se(mean)", "skewness", 
                                  "kurtosis")
)
rest.table <- data.frame(rest$table)
rest.table$var <- sapply(df.12[,c(2:3,5:6,8:12)],var)
write.csv(rest.table,"cpi data/rest.csv")


# normal test -------------------------------------------------------------
sapply(df.12, shapiro.test)

#From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
