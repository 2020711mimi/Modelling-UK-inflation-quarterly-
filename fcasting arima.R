rm(list = ls())
Sys.setlocale("LC_TIME", "English")

# -------------------------------------------------------------------------


#Packages
library(readxl)
library(data.table)
library(lubridate)#extract cpi time to newdata(furniture48)
library(tidyverse)
library(broom)
library(stargazer)
library(knitr)
library(officer)
library(flextable)
library(magrittr)
library(dplyr)
library(moments)
library(Inflation)
library(ggplot2)
library(zoo)
library(xts)
library(hydroTSM)
library(openair)
library(tibble)
library(miscTools)
library(priceR)
library(Metrics)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(kableExtra)
library(gridExtra)
library(egg)
library(grid)
library(cowplot)
library(spatstat)#shift有重复
library(SciViews)
library(sos)
library(aTSA)
library(tseries)
library(vars)
library(forecast)
library(TSstudio)
library(strucchange)
# -------------------------------------------------------------------------



#这份数据是从读取的
cpi_all_item_index <- read.csv("series-260522.csv")

growth_rate <- function(x)
  (x / lag(x) - 1) * 100

cpi_all_item_index[, 2] <- as.numeric(cpi_all_item_index[, 2])

cpi_all_item_index$inflation <-
  growth_rate(cpi_all_item_index$CPI.INDEX.00..ALL.ITEMS.2015.100)

MAR93_CPI_monthly <- which(cpi_all_item_index$Title == "1993 JAN")
NOV2019_CPI_monthly <- which(cpi_all_item_index$Title == "2019 NOV")

monthly_cpi_93_19 <-
  cpi_all_item_index[c(MAR93_CPI_monthly:NOV2019_CPI_monthly), ]
######################

table57 <- read.csv("table57.csv")



colnames(table57) <- c(table57[6,])

table57 <- table57[-c(1:6), ]

table57[,-c(1:2)] <- sapply(table57[,-c(1:2)], as.numeric)

table57[,-c(1:2)] <- growth_rate(table57[,-c(1:2)])

CPI_monthly_199301 <- which(table57$`index date   ` == "199301")
CPI_monthly_201911 <- which(table57$`index date   ` == "201911")

monthly_cpi_93_19_table57 <-
  table57[c(CPI_monthly_199301:CPI_monthly_201911), ]

monthly_cpi_93_19$table57 <-
  monthly_cpi_93_19_table57$`CPI ALL ITEMS`

#write.csv(monthly_cpi_93_19,"table57 and ons.csv")
##########################################




for (i in 3:5) {
  print(adf.test(monthly_cpi_93_19[, i]))
}

# AR
iinflaltion <- monthly_cpi_93_19

VARselect(iinflaltion$table57)

VARselect(iinflaltion$inflation)

ar.ols(iinflaltion$table57,
       demean = F,
       intercept = T)
# Arima -------------------------------------------------------------------

cpi57 = ts(iinflaltion$table57,
           frequency = 12,
           start = c(1993, 1))



autoplot(cpi57)

fit_ARIMA <- auto.arima(cpi57)
fit_ARIMA

print(summary(fit_ARIMA))

d7bt = ts(iinflaltion$inflation,
          frequency = 12,
          start = c(1993, 1))

d7bt <- auto.arima(d7bt, seasonal = FALSE)
d7bt
#ARIMA(p,d,q)x(P,D,Q) model, where P=number of seasonal autoregressive (SAR)
#terms, D=number of seasonal differences, Q=number of seasonal moving average
#(SMA) terms

forecast1 <- forecast(fit_ARIMA, h = 12)
#
forecast1
#
plot(forecast1)
#
plot(forecast1$residuals)
#
qqnorm(forecast1$residuals)
#
acf(forecast1$residuals)
#
pacf(forecast1$residuals)
#
summary(fit_ARIMA)
#
accuracy(fit_ARIMA)

#In sample forecasting


#Split the sample into traing and test sets

split_inf <- ts_split(cpi57, sample.out = 12)

traing <- split_inf$train
testing <- split_inf$test

length(traing)
length(testing)

#Use an Arima diagnostic plot on the traing set

arima_diag(traing)

# Series: cpi57 : ARIMA(2,0,1) with non-zero mean
arima201 <- arima(traing, order = c(2, 0, 1))
arima201
autoplot(arima201)
check_res(arima201)

sarima <-
  arima(traing,
        order  = c(2, 0, 1),
        seasonal = list(order = c(1, 0, 0)))

auto <- auto.arima(traing, seasonal = TRUE)
auto
autoplot(auto)
check_res(auto)

auto_se <- auto.arima(traing, seasonal = FALSE)

#forecast values and diagnostic

fcast1 <- forecast(arima201, h = 12)
test_forecast(actual = cpi57,
              forecast.obj = fcast1,
              test = testing)
accuracy(fcast1, testing)

fcast2 <- forecast(sarima, h = 12)
test_forecast(actual = cpi57,
              forecast.obj = fcast2,
              test = testing)
accuracy(fcast2, testing)


fcasta <- forecast(auto, h = 12)
test_forecast(actual = cpi57,
              forecast.obj = fcasta,
              test = testing)
accuracy(fcasta, testing)

fcasta_ns <- forecast(auto_se, h = 12)
test_forecast(actual = cpi57,
              forecast.obj = fcasta_ns,
              test = testing)
accuracy(fcasta_ns, testing)

#结论是Series: traing
#ARIMA(1,0,3)(1,1,2)[12]
#最好

#auto$x 就是traing原数据，auto$fitted 是回归的数据

traing_fit <- cbind(traing, auto$fitted, auto_se$fitted)
autoplot(traing_fit)


# Out-of sample forecast --------------------------------------------------


#Generate the optimal fit

finalfit <- auto.arima(cpi57, seasonal = TRUE)
autoplot(finalfit)
check_res(finalfit)

#Generate out of sample forecast

fcastf <- forecast(cpi57, model = finalfit, h = 12)
plot_forecast(fcastf)
summary(fcastf)


# structure change --------------------------------------------------------


plot.ts(cpi57)
model1 = Fstats(cpi57 ~ 1, from = 0.01)
sctest(model1)
# H0: no structure change in a series
strucchange::breakpoints(cpi57 ~ 1)


# QLR test ----------------------------------------------------------------

data("PhillipsCurve")
model <- dp ~ dp1 + u1
qlr <- Fstats(model, data = PhillipsCurve)
plot(qlr, alpha = 0.05)
#The black line in the plot is the set of F-statistics. The maximum F-stat is
# the QLR stat. The Red line is the critical value based on Andrews (1993) and
#  Hansen (1997). In this case, we would fail to reject the null that there is
#  no structural change
library(desk)
qlr.test(
  model,
  data = PhillipsCurve ,
  from = 1,
  to = 130,
  sig.level = 0.05,
  details = FALSE
)

# seasonal test -----------------------------------------------------------

library(fma)
plot(pigs)
fit1 <- ets(pigs)

# annual cpi test ---------------------------------------------------------


inf_year <- read.csv("CPI_1989_1-2021_10.csv")


inf_yf = ts(
  inf_year$CPI.ANNUAL.RATE.00..ALL.ITEMS.2015.100,
  frequency = 12,
  start = c(1989, 1)
)

#seasonal test
library(seasonal)
seainf <- seas(inf_yf)
plot(seainf)

isSeasonal(inf_yf,  freq = 12)

mod1 <- auto.arima(inf_yf, seasonal = isSeasonal(inf_yf))
summary(mod1)
mod2 <- auto.arima(inf_yf,seasonal = TRUE)
summary(mod2)

#Split the sample into traing and test sets 
split_inf <- ts_split(inf_yf, sample.out = 12) 
traing <- split_inf$train 
testing <- split_inf$test

#
fcasta <- forecast(mod1, h = 12)
test_forecast(actual = inf_yf,
              forecast.obj = fcasta,
              test = testing)
accuracy(fcasta, testing)

fcasta_ns <- forecast(mod2, h = 12)
test_forecast(actual = inf_yf,
              forecast.obj = fcasta_ns,
              test = testing)
accuracy(fcasta_ns, testing)

# -------------------------------------------------------------------------



library(TTR)
birthstimeseriescomponents <- decompose(inf_yf)

plot(birthstimeseriescomponents)

cpi57dec <- decompose(cpi57)
plot(cpi57dec)

autoplot(inf_yf)

#auto arima
fit_inf_arima <- auto.arima(inf_yf)
fit_inf_arima

#cbind together for plot

inf_fit_ye <- cbind(inf_yf, fit_inf_arima$fitted)
autoplot(inf_fit_ye)

check_res(fit_inf_arima)

#Generate out of sample forecast

fcastf <- forecast(inf_yf, model = fit_inf_arima, h = 12)
plot_forecast(fcastf)
summary(fcastf)

#in-sample forecast

#Split the sample into traing and test sets

split_inf <- ts_split(inf_yf, sample.out = 12)

traing <- split_inf$train
testing <- split_inf$test

length(traing)
length(testing)

#Use an Arima diagnostic plot on the traing set

arima_diag(traing)



# Pseudo out-of-sample forecasts ------------------------------------------

d <- data.frame(x = rnorm(15), y = rnorm(15))
ols <- function(d)
  lm(y ~ x, data = d)
## Basic Usage:
##
recursive_forecasts(ols, d, 4, "recursive")

## Illustrate different estimation windows by comparing forecasts for
## observation 11 (note that the forecast for observation 11 will be the
## 7th element that apply.oos returns in this example)
newd <- d[11, ]


all.equal(predict(lm(y ~ x, data = d[7:10, ]), d[11, ]),
          recursive_forecasts(ols, d, 4, "rolling")[7])



all.equal(predict(lm(y ~ x, data = d[1:10, ]), d[11, ]),
          recursive_forecasts(ols, d, 4, "recursive")[7])



all.equal(predict(lm(y ~ x, data = d[1:4, ]), d[11, ]),
          recursive_forecasts(ols, d, 4, "fixed")[7])



# clarkwest: Clark and West's (2006, 2007) Out-of-Sample Test -------------

x <- rnorm(100)
d <- data.frame(y = x + rnorm(100), x = x)
R <- 70

model1 <- function(d)
  lm(y ~ 1, data = d)
model2 <- function(d)
  lm(y ~ x, data = d)

clarkwest(model1, model2, d, R, window = "rolling")
