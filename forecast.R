Sys.setlocale("LC_TIME", "English")
rm(list = ls())

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
# annual cpi test ---------------------------------------------------------


inf_year <- read.csv("CPI_1989_1-2021_10.csv")


inf_yf = ts(
  inf_year$CPI.ANNUAL.RATE.00..ALL.ITEMS.2015.100,
  frequency = 12,
  start = c(1989, 01),
  end = c(2019, 12)
)

plot(inf_yf)
#ADF test
pp.test(inf_yf)

#seasonal test
library(seasonal)
seainf <- seas(inf_yf)
plot(seainf)

library(seastests)
isSeasonal(inf_yf,  freq = 12)

# all sample ---------------------------------------------------------------

#in-sample 不用分割数据
#

auto_insample <- auto.arima(inf_yf, seasonal = TRUE)
check_res(auto_insample)

# in-sample plot actual vs fitted
actual_fit <- data.frame(cbind(inf_yf, auto_insample$fitted))

autoplot(actual_fit)



# 2017.1-2019.12 ----------------------------------------------------------

#start-2016.12 作为回归样本 与去测2017-2019


#Split the sample into traing and test sets 3*12=36
split_inf <- ts_split(inf_yf, sample.out = 36)
traing <- split_inf$train
testing <- split_inf$test

length(traing)
length(testing)

auto_05 <- auto.arima(traing, seasonal = TRUE)
summary(auto_05)


fcasta_auto <- forecast(auto_05, h = 36)
autoplot(fcasta_auto$residuals)
check_res(fcasta_auto)

sarima <- test_forecast(actual = inf_yf,
                        forecast.obj = fcasta_auto,
                        test = testing)
#plot attr
sarima$x$layoutAttrs$`32c0c0a51a5`$title <-
  c("In-sample forecasting using ARIMA(1,1,1)(0,0,2)[12] ")
sarima$x$layoutAttrs$`32c0c0a51a5`$yaxis <- NULL

sarima

accuracy(fcasta_auto, testing)
autoplot(testing, fcasta_auto$mean)

fcast1719 <- (cbind(fcasta_auto$mean, testing))

fcast1719 <- data.frame(cbind(fcasta_auto$mean, testing))

fcast1719$time <-
  seq.Date(
    from = as.Date("2017/01/01", format = "%Y/%m/%d"),
    by = "month",
    length.out = 36
  )
colnames(fcast1719) <- c("aaa", "ccc", "date")

library(ggh4x)
nt <-
  ts(
    fcast1719$date ,
    start = c(2017, 01),
    end = c(2019, 12),
    frequency = 12
  )
economics_m <- economics[1:24,]

lbls <-
  paste0(month.abb[month(fcast1719$date)], " ", lubridate::year(fcast1719$date))
brks <- fcast1719$date


ggplot(fcast1719, aes(x = date)) +
  geom_line(aes(y = aaa, color = "Forecast")) +
  geom_line(aes(y = ccc, color = "CPI")) +
  labs(title = "In-sample forecasting using ARIMA",
       y = " %") +
  scale_x_date(labels = lbls, breaks = brks)  + # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid



fcasta_auto$mean > testing
#2019 jan 持平
#2019 aug 预测值开始大于实际值
#
check_res(sarima)



# Out of Sample Forecasting-------------------------------------------------------------------------

#gengeral the optimal fit

finalyfit <- auto.arima(inf_yf, seasonal = TRUE)
autoplot(finalyfit)
check_res(finalyfit)

#general out of sample forecast

fcast_out <- forecast(inf_yf, model = finalyfit, h = 12)
plot_forecast(fcast_out)
summary(fcast_out)

#到20年6月都在下降，之后上升。
##page 54 -63






