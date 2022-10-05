rm(list = ls())
library(readxl)
cpih <- read.csv("CPIH/CPI and 12 divs.csv")
cpih<- cpih[-c(1:12),]
monthly <- ts(cpih, start = c(1992, 1), frequency = 12)
quarterly <- aggregate(monthly, nfrequency = 4)
quarterly <- data.frame(quarterly)

library(psych)
describe(quarterly$CPIH)

library(EnvStats)
kurtosis(quarterly$CPIH,excess = TRUE)

growth_rate <- function(x)  (x / lag(x) - 1) * 100
test <- growth_rate(quarterly$CPIH)




quarterly/3

df.change<- data.frame( ts(sapply(quarterly,function(x)(x-x[1])/x[1]*100),frequency = 4))
describe(df.change$CPIH)

h <- read.csv("CPIH/series-090922.csv")
which(h$Title=="1993 Q1")
which(h$Title=="2019 Q4")
df <- h[57:164,]
df$CPIH.ANNUAL.RATE.00..ALL.ITEMS.2015.100 <- as.numeric(df$CPIH.ANNUAL.RATE.00..ALL.ITEMS.2015.100)
describe(df$CPIH.ANNUAL.RATE.00..ALL.ITEMS.2015.100)
