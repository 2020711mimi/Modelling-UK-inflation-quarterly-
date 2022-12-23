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

df <- fread("all core inflation measures/OVERALL.csv")
df <- df[,-1]
# 计算每一列的平均值
mean_values <- sapply(df, mean)

# 计算每一列的中位数
median_values <- sapply(df, median)

# 计算每一列的标准差
sd_values <- sapply(df, sd)

# 计算每一列的偏度
skewness_values <- sapply(df, skewness)

# 计算每一列的峰度
kurtosis_values <- sapply(df, kurtosis)

# 将结果绑定在一起，生成新的数据框
new_df <- cbind(mean_values, median_values, sd_values, skewness_values, kurtosis_values)

# 保留两位小数
new_df <-data.frame( round(new_df, 2))

# 使用kable函数生成表格
library(knitr)
library(officer)
# 将行名添加到数据的第一行中
new_df <- cbind(rownames(new_df), new_df)
# 创建一个新的Word文档
doc <- read_docx()

# 将表格插入到文档中
# 将表格插入到文档中
doc <- doc %>%
  body_add_table(new_df)

# 保存文档
doc <- doc %>%
  print(target = "all core inflation measures/data description.docx")

