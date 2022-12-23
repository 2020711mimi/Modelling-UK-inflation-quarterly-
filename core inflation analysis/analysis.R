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
options(scipen = 200)

rm(list = ls())
'%!in%' <- function(x, y)
  ! ('%in%'(x, slack))
#devtools::install_github("cardiomoon/ggiraphExtra")

# -------------------------------------------------------------------------


df <- fread("core inflation analysis/data.csv")

df <- tail(df,-13)

df.flex <- fread("sticky price/ flexible.csv")

df$flex <- df.flex$x


nt <- ts(df, start = c(2006, 2), frequency = 12)
w <- ggplot(data = df, aes(x = time(nt))) +
  geom_line(aes(y = CPI, color = "CPI Inflation")) +
  geom_line(aes(y = Sticky, color = "Sticky CPI")) +
  geom_line(aes(y = flex , color = "Flexible CPI")) +
  labs(x = "Time", y = "Inflation")
plot2<- w + theme_bw() + theme(
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
  ph_with(rvg::dml(ggobj = plot2),
          width = 8,
          height = 4, 
          location = ph_location_type(type = "body"))

#> pptx document with 2 slide(s)

#print(pptx, "sticky price/compare_graph.pptx")

#如何解释2013年之后sticky price CPI 为什么变得稳定了呢？
#
#
var(df$Sticky)
var(df$flex)
var(df$CPI)
which(df$date_all=="2013-01-01")
var(df$flex) / var(df$Sticky)

df.2013 <- df[which(df$date_all=="2013-01-01"):nrow(df),]

sapply(df.2013, var)
#Natural Rate of Unemployment
#https://www.economicshelp.org/macroeconomics/unemployment/natural_rate/
#LFS: ILO Unemployment rate: UK: All: Aged 16-64: %: SA
#is a measure of the percentage of individuals in the labor force who are without work but are actively seeking employment.
#
#
# #linear relationship between the
# amount of slack in the economy (in this case using the difference between the rate of unemployment and the natural
#                                 rate of unemployment as calculated by the Congressional
#                                 Budget Offi ce)
#                                 and 12-month changes in the headline CPI,
#   the fl exible price CPI, and the sticky-price CPI

slack <- fread("sticky price/slack.csv")

har <- fread("sticky price/Harmonized Unemployment Rate.csv")

slack <-
  slack[which(slack$Title == "2006 FEB"):which(slack$Title == "2019 DEC"),]

u <- fread("sticky price/unemployment.csv")
u <- u[which(u$Title == "2006 FEB"):which(u$Title == "2019 DEC"),]
u[,2] <- as.numeric(unlist(u[,2]))

df.slack <- df[, c("CPI", "Sticky", "flex")]
df.slack$slack <-
  slack$`LFS: ILO Unemployment rate: UK: All: Aged 16-64: %: SA`

sapply(df.slack, class)

var(df.slack$flex)/var(df.slack$Sticky)

df.slack$slack <- as.numeric(df.slack$slack)
#df.slack$slack <- u[,2]-df.slack$slack
#
lm(slack~CPI,df.slack)
lm(slack~Sticky,df.slack)
lm(slack~flex,df.slack)

plot2<- ggplot(df.slack) +
  geom_jitter(aes(CPI, slack, color = "CPI")) + geom_smooth(aes(CPI, slack, color =
                                                                  "CPI"), method =
                                                              lm, se = FALSE) +
  geom_jitter(aes(Sticky, slack, color = "Sticky CPI", )) + geom_smooth(aes(Sticky, slack, color = "Sticky CPI", ),
                                                                    method =
                                                                      lm,
                                                                    
                                                                    se = FALSE) +
  geom_jitter(aes(flex, slack, , color = "Flexible CPI", )) + geom_smooth(aes(flex, slack, color =
                                                                        "Flexible CPI"),
                                                                  method =
                                                                    lm,
                                                                  se = FALSE) +
  labs(x = "Unemployment", y = "12-month percent change") + theme_bw() + theme(
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
  ph_with(rvg::dml(ggobj = plot2),
          width = 8,
          height = 4, 
          location = ph_location_type(type = "body"))

#> pptx document with 2 slide(s)

#print(pptx, "sticky price/test_graph.pptx")



