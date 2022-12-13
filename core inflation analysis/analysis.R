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
library(parallel)
library(benchmarkme)
# -------------------------------------------------------------------------
options(scipen = 200)

rm(list = ls())
'%!in%' <- function(x, y)
  ! ('%in%'(x, y))

df <- fread("core inflation analysis/data.csv")

df <- tail(df, -13)

df.flex <- fread("sticky price/ flexible.csv")

df$flex <- df.flex$x



nt<-ts(df,start = c(2006,2),frequency = 12 )
w<-ggplot(data=df, aes(x=time(nt)))+
  geom_line(aes(y = CPI, color="CPI Inflation")) +
  geom_line(aes(y = Sticky, color="Sticky CPI")) +
  geom_line(aes(y =flex , color="Flexible CPI"))+ 
  labs(x="Time",y="Inflation")
w + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#如何解释2013年之后sticky price CPI 为什么变得稳定了呢？
#
#
sapply(d, var)
var(df$Sticky)
var(df$flex)
var(df$CPI)

var(df$CPI) / var(df$Sticky)

sapply(df, var)
#Natural Rate of Unemployment 
#https://www.economicshelp.org/macroeconomics/unemployment/natural_rate/
#LFS: ILO Unemployment rate: UK: All: Aged 16-64: %: SA
#is a measure of the percentage of individuals in the labor force who are without work but are actively seeking employment. 