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
library(tis)
# -------------------------------------------------------------------------


rm(list = ls())
'%!in%' <- function(x, y)
  ! ('%in%'(x, y))

Sys.setlocale("LC_TIME", "English")

Dixon_tian <- fread("sticky price/dixon data.csv")

colnames(Dixon_tian)[7] <- c("accumative")
sapply(Dixon_tian, class)

#duration
Dixon_tian <- head(Dixon_tian, -2)
Dixon_tian$duration <- 100 / Dixon_tian$frequency

# median frequency: Median frequency by CPI weight
# You do not measure frequency by months. You measure it as proportion of changes per month. I would suggest frequencies of over >18% (mean duration 5.56 months.  Use the mean duration of 1/freq not the log one.


Dixon_tian[which.min(abs(500 - Dixon_tian$accumative)), ]

#there are 244 sticky items.
sticky_id <-
  as.numeric(unlist(Dixon_tian[Dixon_tian$duration > 5.56, 1]))
flex_item <- Dixon_tian[Dixon_tian$duration < 5.56, 1]
244 + 325

# item index --------------------------------------------------------------


item_cpi <-
  fread("sticky price/price quots.csv")#195 time(feb,05-apr,21) and 1144 items
##重复32 个id 210211
duo_id <-
  as.numeric(colnames(item_cpi)[duplicated(colnames(item_cpi))])

#duplicated sticky item
# 在sticky 里重复了7个
duplicated_stickyID <- ((sticky_id[((sticky_id)) %in% duo_id]))

#item id not avaiable:410103 - Private Rented Furnished property (see note)
#                    410101- Private Rented Unfurnished property (see note)
sticky_id[((sticky_id)) %!in% as.numeric(colnames(item_cpi)[-1])]


#ts. annual growth rate. 
## in % percent
test <-
  ts(item_cpi[,-1] , start = c(2005, 2), frequency = 12)
cnm<- growth.rate(test, lag = 12)
casdaasd <- data.table(cnm)
# -------------------------------------------------------------------------


newdf <- fread("item ID number name start 2016.csv")

#总数据里，含有na的列
df.na <- as.data.frame(cbind(lapply(lapply(newdf, is.na)
                                    , sum)))
#699个数据都有na
length(which(df.na != 0))
##missing sticky: 1  410103 #2  410101
sticky_id[((sticky_id)) %!in% as.numeric(colnames(newdf)[-1])]

#重复的32个id 在inflation 数据中 名称后缀.1
#在sticky里重复的7个的位置
#176  230  237  542  588    843 1017
dup.loc <-  which(colnames(newdf) %in% duplicated_stickyID) + 1
colnames(newdf)[(dup.loc)]

#sticky location

sti.loc <-  which(colnames(newdf) %in% sticky_id)

all.sti.loc <- c(dup.loc , sti.loc)

sti_index <- newdf[, ..all.sti.loc]

sti_index[is.na(sti_index)] <- 0

# -------------------------------------------------------------------------


#sticky 数据里，含有na的列
df.na <- as.data.frame(cbind(lapply(lapply(sti_index, is.na)
                                    , sum)))
#有106个item 含有过na
length(which(df.na != 0))

# weight ------------------------------------------------------------------
#weight data from feb 05
sticky_weight <- fread("sticky price/weight.csv")
#减去前12个月(05 feb - jan 06)
sticky_weight <- tail(sticky_weight,-12)

sti_weight <- sticky_weight[, ..all.sti.loc]

sti_weight[is.na(sti_weight)] <- 0

#重新权重weight
#weight 总和不是1000，
#每行的每个数加上（1000-每行的和）/290
new_w<-data.frame(matrix(nrow = 183,ncol = 249))
for (i in 1:nrow(sti_weight)) {
  new_w[i,]<-sti_weight[i,]+(1000- rowSums(sti_weight[i,]))/ncol(sti_weight)
}
#
rowSums(new_w[,])
sti_weight<-data.table( new_w/1000)
rowSums(sti_weight)


# sticky CPI-------------------------------------------------------------------------


colnames(sti_weight) == colnames(sti_index)

sticky_core_inflation <- rowSums(sti_index * sti_weight)

sticky_cpi<- ts(sticky_core_inflation, start = c(2006,2),frequency = 12)

#write.csv(sticky_cpi,"sticky price/ sticky test.csv")


