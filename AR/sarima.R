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

inf_yf <- ts(CPI$`CPI ANNUAL RATE 00: ALL ITEMS 2015=100`,start = c(2015,01),frequency = 12)

library(seastests)
isSeasonal(inf_yf,  freq = 12)

auto_insample <- auto.arima(inf_yf, seasonal = TRUE)

auto_insample$fitted

df.compare <- data.frame(cbind(auto_insample$fitted, inf_yf))
colnames(df.compare) <- c("SARIMA", "Headline")

nt <- ts(
        start = c(2005, 1),
        end = c(2019, 12) ,
        frequency = 12
)

FE <- ggplot(data = df.compare, aes(x = time(nt))) +
        geom_line(aes(y = SARIMA, color = "SARIMA")) +
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

print(pptx, "AR/Sarima.pptx")

# -------------------------------------------------------------------------


AR <- fread("AR/AR fitted.csv")

AR$SARIMA <- auto_insample$fitted

FE <- ggplot(data = AR, aes(x = time(nt))) +
        geom_line(aes(y = SARIMA, color = "SARIMA")) +
        geom_line(aes(y = Headline, color = "Headline")) +
        geom_line(aes(y = AR, color = "AR")) +
        labs(x = "Time", y = "Inflation")
plot2 <- FE + theme_bw() + theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
)
plot2
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
pptx
#> pptx document with 2 slide(s)

print(pptx, "AR/Sarima AND AR.pptx")

