rm(list = ls())
library(readxl)
cpi <- read.csv("CPI/cpi.csv")
library(psych)
describe(cpi$CPI.ALL.ITEMS)

library(DescTools)
Desc(cpi$CPI.ALL.ITEMS, plotit = TRUE)
