library(eFRED)
library(dplyr)
library(oops)

rm(list = ls())

dat <- fred("CPIAUCSL") 

cpi_data <- dat %>%  transmute(
  date,
  pi    = log(CPIAUCSL) - log(lag(CPIAUCSL, 12)),
  pi.1  = lag(pi, 1),
  pi.2  = lag(pi, 2),
  pi.3  = lag(pi, 3),
  pi.6  = lag(pi, 6),
  pi.12 = lag(pi, 12)
) %>%
  na.omit

train <- cpi_data[which(cpi_data$date <= as.Date("2019-12-01")),]
test  <- cpi_data[which(cpi_data$date > as.Date("2019-12-01") & cpi_data$date < as.Date("2021-01-01")),]

y <- as.matrix(train$pi, ncol=1) 
X <- cbind(1, as.matrix(train[,3:7]))

NNetwork <- oClass("NNetwork")
nn <- NNetwork(y, X)

