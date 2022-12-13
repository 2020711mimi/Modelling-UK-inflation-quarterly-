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

rm(list = ls())
'%!in%' <- function(x,y)!('%in%'(x,y))

Sys.setlocale("LC_TIME","English")

Dixon_tian<-read_excel("sticky price/Dixon_and_Tian_570_item_Freq yiyi.xlsx","Data")

Dixon_tian$frequency <- as.numeric(Dixon_tian$frequency)

sapply(Dixon_tian, class)

Dixon_tian$mo <- -1/ln(1- Dixon_tian$frequency/100)

#which is na
#Dixon_tian[ which(is.na(Dixon_tian$mo)),2]

Dixon_tian <- Dixon_tian[-c(which(is.na(Dixon_tian$mo))),]
median(Dixon_tian$mo)#4.48142=4.48

#持续时间短，变化快，flexable
sticky_id <-as.numeric(unlist( Dixon_tian[which(Dixon_tian$mo > median(Dixon_tian$mo)),1]))#284
flexable_id <- Dixon_tian[which(Dixon_tian$mo < median(Dixon_tian$mo)),1]#282


#item_cpi<-read_excel("sticky price/item_indices_complete.xlsx","Sheet2") 

item_cpi<- fread("sticky price/price quots.csv")#195 time(feb,05-apr,21) and 1144 items

##重复32 个id 210211
duo_id<- as.numeric( colnames(item_cpi)[ duplicated(colnames(item_cpi))] )

#duplicated sticky item  
# 在sticky 里重复了8个
duplicated_stickyID<- ((sticky_id[( (sticky_id)) %in% duo_id]))

#item id not avaiable:410103 - Private Rented Furnished property (see note)
#                    410101- Private Rented Unfurnished property (see note)
sticky_id[((sticky_id)) %!in% as.numeric(colnames(item_cpi)[-1])]


newdf<-fread("item ID number name start 2016.csv")

#总数据里，含有na的列
df.na <- as.data.frame(
cbind(
  lapply(
    lapply(newdf, is.na)
    , sum)
)
)
#699个数据都有na
length(which(df.na!=0))
##missing sticky: 1  410103 #2  410101
sticky_id[as.numeric(unlist(sticky_id)) %!in% as.numeric(colnames(newdf)[-1]),]

#重复的32个id 在inflation 数据中 名称后缀.1
#在sticky里重复的8个的位置
#176  230  237  542  588  717  843 1017
dup.loc<- which(colnames(newdf) %in% duplicated_stickyID)+1
colnames(newdf)[(dup.loc)]

#sticky location

sti.loc<- which(colnames(newdf) %in% sticky_id)

all.sti.loc <- c(dup.loc , sti.loc)


# #sticky index -----------------------------------------------------------


sti_index<- newdf[,..all.sti.loc]

#sticky 数据里，含有na的列
df.na <- as.data.frame(
  cbind(
    lapply(
      lapply(sti_index, is.na)
      , sum)
  )
)
#有125个item 含有过na
length(which(df.na!=0))


# weight ------------------------------------------------------------------

sticky_weight<-fread("sticky price/weight.csv")

sticky_weight[sticky_weight %in% item_cpi]

as.numeric(colnames(sticky_weight)[-1])[as.numeric(colnames(sticky_weight)[-1]) %in% colnames( item_cpi)]
setdiff(colnames(sticky_weight),colnames(item_cpi))
setdiff(sticky_weight,item_cpi)
colnames(sticky_weight)[colnames(sticky_weight)!=colnames(item_cpi)]

colnames(sticky_weight)[ncol(sticky_weight)]
colnames(item_cpi)[ncol(item_cpi)]

which(colnames(sticky_weight) == colnames(item_cpi))
