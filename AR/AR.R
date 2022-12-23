#Packages
library(readxl)
library(data.table)
library(lubridate)#extract cpi time to newdata(furniture48)
library(scales)
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
library(parallel)
library(benchmarkme)
library(predict3d)
library(interactions)
library(aTSA)
library(tseries)
library(forecast)
library(officer)
library(tidyverse)
library(rvg)
library(equatiomatic)
options(scipen = 200)

rm(list = ls())
'%!in%' <- function(x, y)
  ! ('%in%'(x, slack))

# -------------------------------------------------------------------------
#aunnual inflation
CPI <- fread("CPI_1989_1-2021_10.csv")

CPI <-
  CPI[which(CPI$date_all == "2005-01-01"):which(CPI$date_all == "2019-12-01")
      , c(2:3)]

adf.test(CPI$`CPI ANNUAL RATE 00: ALL ITEMS 2015=100`)

# -------------------------------------------------------------------------
#log完了再diff， 结果也是ar(3)


d.log <- (diff(log(CPI$`CPI ANNUAL RATE 00: ALL ITEMS 2015=100`)))
d.log[!is.finite(d.log)] <- 0
adf.test(d.log)
qxts <- ts(d.log, start = c(2005, 02))
ar.ols(qxts,
       demean = F,
       intercept = T)

# -------------------------------------------------------------------------



CPI$log <- (log(CPI$`CPI ANNUAL RATE 00: ALL ITEMS 2015=100`))
CPI[!is.finite(CPI$log)] <- 0

adf.test(CPI$log)

#没用 log了还是不显著， 干脆不log了
qxts <- ts(CPI[, 2], start = c(2005, 01), frequency = 12)

ar <- ar.ols(qxts,
             demean = F,
             intercept = T)
ar
arima(qxts, order = c(1, 0, 0))
summary(ar)
class(ar)

ts.plot(qxts)
AR_fit <- qxts -   ar$resid
plot(AR_fit)
points(AR_fit,
       type = "l",
       col = 2,
       lty = 2)

qxts
ar$resid

png()
plot(qxts)
lines(seq(qxts), qxts - ar$resid, col = "red")
dev.off()


# -------------------------------------------------------------------------


ar.auto <-
  auto.arima(
    qxts,
    ,
    max.q = 0,
    max.Q = 0,
    max.D = 0,
    max.P = 0,
    max.d = 0
  )
ar.auto
str(ar.auto)
ar.auto$fitted

df.compare <- data.frame(cbind(ar.auto$fitted, qxts))
colnames(df.compare) <- c("AR", "Headline")

nt <- ts(
  start = c(2005, 1),
  end = c(2019, 12) ,
  frequency = 12
)

FE <- ggplot(data = df.compare, aes(x = time(nt))) +
  geom_line(aes(y = AR, color = "AR")) +
  geom_line(aes(y = Headline, color = "Headline")) +
  labs(x = "Time", y = "Inflation")
plot2 <- FE + theme_bw() + theme(
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black")
)

pptx <- read_pptx()

pptx %>%
  add_slide() %>%
  # This first line puts it in as a static png image for comparison
  ph_with(plot2, location = ph_location_type(type = "body")) %>%
  add_slide() %>%
  # This line puts in a shape object, which can be ungrouped and edited
  ph_with(
    rvg::dml(ggobj = plot2),
    width = 8,
    height = 4,
    location = ph_location_type(type = "body")
  )

#> pptx document with 2 slide(s)

#print(pptx, "AR/test_graph.pptx")

# equ ---------------------------------------------------------------------


cc <- ar.auto$coef
cc
eqn <-
    paste("Y =", paste(
      round(cc[1], 2),
      paste(round(cc[-1], 2), names(cc[-1]), sep = " * ", collapse = " + "),
      sep = " + "
    ), "+ e")

(eqn <- gsub('\\+ -', '- ', gsub(' \\* ', '*', eqn)))

extract_eq(ar.auto)
ar.auto


# -------------------------------------------------------------------------


