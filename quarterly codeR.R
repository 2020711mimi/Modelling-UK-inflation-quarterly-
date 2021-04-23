library(tidyverse)
rm(list = ls())
library(stargazer)
# DATA INPUT --------------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
getwd()
library(readxl)
cpi <- read_excel("correct quarter cpi.xlsx")#108
library(data.table)

setDF(cpi)#convert to 'data.frame'

cols.num <- c(colnames(cpi))
cpi[cols.num] <- sapply(cpi[cols.num],as.numeric)

cpi <- as.data.table(cpi)
colnames(cpi) = gsub("&", "and", colnames(cpi))
COPY<-copy(cpi)
tracemem(COPY)==tracemem(cpi)
Data_A = as.data.table (cpi)

# -------------------------------------------------------------------------
Varnames <- copy(names(Data_A))
Data_A[, (names(Data_A)) := lapply(.SD, as.numeric)][,
                                                     rn := .I]
melted <- melt(Data_A, id.vars="rn")[,
                                     (paste0("lag_", 0:4)) := shift(value, 0:4, type="lag"),
                                     by=variable][, 
                                                  value:=NULL][]
res <- dcast.data.table(melt(melted, id.vars=c("rn", "variable"), variable.name="lag"),
                        rn ~ variable + lag, sum)
res[, ncol(res), with=FALSE]
# lagged variable generation ----------------------------------------------


cpi <- cpi[, unlist(lapply(.SD, shift, n = 0:4), recursive = FALSE)]#lags generation
# quarterly  Dummies ---------------------------------------------------------


cpi$time <- seq.Date(from = as.Date("1992/01/01",format = "%Y/%m/%d"), by = "quarter", length.out = 112)
cpi$time <- format(cpi$time, format = "%b")

cpi <- fastDummies::dummy_cols(cpi, select_columns = "time", remove_most_frequent_dummy = TRUE,
)

cpi$time1 <- seq.Date(from = as.Date("1992/01/01",format = "%Y/%m/%d"), by = "quarter", length.out = 112)

# Trend,vat, recession ----------------------------------------------------
newdata<-copy(cpi)

newdata<-newdata[newdata$'time1' >= as.Date("1993-01-01")]
Trend <- seq_along(newdata$time1)

VAT1 <- as.numeric(newdata$`time1` == "2008-10-01")
VAT2 <- as.numeric(newdata$`time1` == "2010-01-01")
VAT3 <- as.numeric(newdata$`time1` == "2011-01-01")
Recession <- as.numeric(newdata$`time1` >= "2008-04-01" & newdata$`time1`<= "2009-06-01")#??5??dummy

# Lag ---------------------------------------------------------------------
library(dplyr) 
lag1 <- newdata %>% 
  select(matches("(2)$"))
lag2 <- newdata %>% 
  select(matches("(3)$"))
lag3 <- newdata %>% 
  select(matches("(4)$"))
lag4 <- newdata %>% 
  select(matches("(5)$"))

# MONTYHLY DUMMIES --------------------------------------------------------


quarter1 <- as.numeric(newdata$time_Jan)
quarter3 <- as.numeric(newdata$time_Jul)
quarter4 <- as.numeric(newdata$time_Oct)

COPY <- as.data.frame(COPY)
lag1 <- as.data.frame(lag1)
lag2 <- as.data.frame(lag2)
lag3 <- as.data.frame(lag3)
lag4 <- as.data.frame(lag4)

# CPI-LAGS ----------------------------------------------------------------


CPI_lag1 <- (newdata$"CPI ALL ITEMS2")
CPI_lag2 <- (newdata$"CPI ALL ITEMS3")
CPI_lag3 <- (newdata$"CPI ALL ITEMS4")
CPI_lag4 <- (newdata$"CPI ALL ITEMS5")

# Copy????1992????ɾ?? ?ָ? 108obs --------------------------------------------------

COPY<-COPY[-1:-4,]
# regression --------------------------------------------------------------


reg.model <- lapply(1:122, function(x) lm(COPY[,x] ~ lag1[,x]+lag2[,x]+lag3[,x]+lag4[,x]+
                                            quarter1+quarter3+quarter4+VAT1+VAT2+VAT3+Recession+Trend)
)
for (i in 1:122) {
  names(reg.model[[i]]$coefficients)<-c('Constant','lag1','lag2',"lag3","lag4","quarter1","quarter3","quarter4","VAT1","VAT2","VAT3","Recession","Trend")
}

regression <- lapply(1:122, function(x) lm(COPY[,x] ~ lag1[,x]+lag2[,x]+lag3[,x]+lag4[,x]+CPI_lag1+CPI_lag2+CPI_lag3+CPI_lag4+ quarter1+quarter3+quarter4+VAT1+VAT2+VAT3+Recession+Trend)
)
for (i in 1:122) {
  names(regression[[i]]$coefficients)<-c('Constant','lag1','lag2',"lag3","lag4",
                                         "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4",
                                         "quarter1","quarter3","quarter4",
                                         "VAT1","VAT2","VAT3","Recession","Trend")
}

lm(`REPAIR OF HOUSEHOLD APPLIANCES1` ~ `CPI ALL ITEMS1`,data = furniture48)
lm(formula = `REPAIR OF HOUSEHOLD APPLIANCES1`~ `REPAIR OF HOUSEHOLD APPLIANCES2`, data = furniture48)
lm(`OTHER MEDICAL & THERAPEUTIC EQUIPMENT1`~`OTHER MEDICAL & THERAPEUTIC EQUIPMENT2`, data=furniture48)
lm(``~``, data=furniture48)
lm(`OUT-PATIENT SERVICES1`~`OUT-PATIENT SERVICES2`, data=furniture48)
lm(`REPAIR OF HOUSEHOLD APPLIANCES1` ~`OUT-PATIENT SERVICES2`, data = newdata )
# rename regression list --------------------------------------------------

names(regression) <- c(colnames(COPY))
# Extract divisions  ------------------------------------------------------


CPI_ <- reg.model[[1]]
FB01 <- regression[[2]]
AT02 <- regression[[16]]
CF03 <- regression[[22]]
HW04 <- regression[[28]]
FH05 <- regression[[41]]
HL06 <- regression[[54]]
TR07 <- regression[[62]]
CM08 <- regression[[77]]
RC09 <- regression[[80]]
ED10 <- regression[[102]]
RH11 <- regression[[103]]
MS12 <- regression[[108]]
# replace & with AND---------------------------------------------------------------

colnames(COPY) = gsub("&", "and", colnames(COPY))
# how to find na value in col position ------------------------------------
misscolname <- colnames(COPY)[colSums(is.na(COPY)) > 0]

for (i in 1:19) {
  print(grep(misscolname[i],colnames(COPY))) 
}
# 01FOOD and their lower level------------------------------------------------------------------


stargazer(regression[2:15],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[2:15])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# ?????ж??ٸ???��??0.01??0.05????(??һ?ű?) ----------------------------------------------
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
percent(1)
a=list()


length(a)

for (i in 1:4) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  print(percent(length(which(a[[i]] < 0.05))/121))
  #print(a[[i]])
}
for (i in 1:4) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  print(percent(length(which(a[[i]] < 0.01))/121))
}

for (i in 1:4) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  print(percent(length(which(a[[i]] < 0.05))/121))
  #print(a[[i]])
}
for (i in 1:4) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  print(percent(length(which(a[[i]] < 0.01))/121))
}
# Weight ------------------------------------------------------------------
weight<- list(0.109,	0.042,	0.063,	0.115,	0.067,	0.022,	0.152,	0.023,	0.152,	0.019,	0.137,	0.099
              
)
Reduce("+",weight)

# ??һ?ű??ļ??????? ---------------------------------------------------------------

 
#FB01 --------------------------------------------------------------------
a=list()
FB01.0.05.cpi<-list()
FB01.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[2:15], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])#pֵ

  #print(length(which(a[[i]] < 0.01)))#cpi-lag???ж??ٸ?????��??????????
  FB01.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[1]]#oֵС??0.05??*Ȩ??
  #print(length(which(a[[i]] < 0.01))*weight[[1]])
  FB01.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[1]]
  #print(percent(length(which(a[[i]] < 0.05))/14))
  #print(percent(length(which(a[[i]] < 0.01))/14))
}
FB01.0.05.lag<-list()
FB01.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[2:15], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  #print(length(which(a[[i]] < 0.05)))
  FB01.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[1]]
  #print(length(which(a[[i]] < 0.01)))#lag С??0.01??
  FB01.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[1]]
}
# 02AT --------------------------------------------------------------------
AT02.0.05.cpi<-list()
AT02.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[16:21], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  

  AT02.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[2]]

  AT02.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[2]]
  
}
AT02.0.05.lag<-list()
AT02.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[16:21], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  AT02.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[2]]
  
  AT02.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[2]]
}
# 03CF --------------------------------------------------------------------

CF03.0.05.cpi<-list()
CF03.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[22:27], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  

  CF03.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[3]]
  CF03.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[3]]
  
}
CF03.0.05.lag<-list()
CF03.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[22:27], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  CF03.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[3]]
  
  CF03.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[3]]
}
# 04hw --------------------------------------------------------------------
HW04.0.05.cpi<-list()
HW04.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[28:40], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  HW04.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[4]]
  HW04.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[4]]
  
}
HW04.0.05.lag<-list()
HW04.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[28:40], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  HW04.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[4]]
  
  HW04.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[4]]
}
# 05FH --------------------------------------------------------------------

FH05.0.05.cpi<-list()
FH05.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[41:53], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  FH05.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[5]]
  FH05.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[5]]
  
}
FH05.0.05.lag<-list()
FH05.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[41:53], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  FH05.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[5]]
  
  FH05.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[5]]
}
# 06HL --------------------------------------------------------------------
HL06.0.05.cpi<-list()
HL06.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[54:61], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  HL06.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[6]]
  HL06.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[6]]
  
}
HL06.0.05.lag<-list()
HL06.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[54:61], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  HL06.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[6]]
  
  HL06.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[6]]
}
# 07tr --------------------------------------------------------------------
TR07.0.05.cpi<-list()
TR07.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[62:76], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  TR07.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[7]]
  TR07.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[7]]
  
}
TR07.0.05.lag<-list()
TR07.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[62:76], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  TR07.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[7]]
  
  TR07.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[7]]
}
# 08cm --------------------------------------------------------------------
CM08.0.05.cpi<-list()
CM08.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[77:79], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  CM08.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[8]]
  CM08.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[8]]
  
}
CM08.0.05.lag<-list()
CM08.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[77:79], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  CM08.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[8]]
  
  CM08.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[8]]
}
# 09rc --------------------------------------------------------------------
RC09.0.05.cpi<-list()
RC09.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[80:101], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  RC09.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[9]]
  RC09.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[9]]
  
}
RC09.0.05.lag<-list()
RC09.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[80:101], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  RC09.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[9]]
  
  RC09.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[9]]
}
# 10ed --------------------------------------------------------------------
ED10.0.05.cpi<-list()
ED10.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[102], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  ED10.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[10]]
  ED10.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[10]]
  
}
ED10.0.05.lag<-list()
ED10.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[102], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  ED10.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[10]]
  
  ED10.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[10]]
}

# 11rh --------------------------------------------------------------------
RH11.0.05.cpi<-list()
RH11.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[103:107], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  RH11.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[11]]
  RH11.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[11]]
  
}
RH11.0.05.lag<-list()
RH11.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[103:107], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  RH11.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[11]]
  
  RH11.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[11]]
}
# 12ms --------------------------------------------------------------------
MS12.0.05.cpi<-list()
MS12.0.01.cpi<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[108:122], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  MS12.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[12]]
  MS12.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[12]]
  
}
MS12.0.05.lag<-list()
MS12.0.01.lag<-list()
for (i in 1:4) {
  a[[i]] <- lapply(regression[108:122], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  MS12.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[12]]
  
  MS12.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[12]]
}
# add them up and divide by 121 -------------------------------------------

# table 3?? ??4??(0.05) ǰ?벿??(cpi) -----------------------------------------------------


y<-percent(
  mapply(function(x1, y1,a,b1,c,d,e,f,h,i,j,k) x1[[1]]+y1[[1]]+a[[1]]+b1[[1]]+c[[1]]+d[[1]]+e[[1]]+f[[1]]+
           h[[1]]+i[[1]]+j[[1]]+k[[1]],
         FB01.0.05.cpi, AT02.0.05.cpi, CF03.0.05.cpi, HW04.0.05.cpi, FH05.0.05.cpi, HL06.0.05.cpi, TR07.0.05.cpi, CM08.0.05.cpi,
         RC09.0.05.cpi,ED10.0.05.cpi,RH11.0.05.cpi, MS12.0.05.cpi)/121)
print(y)

# table 3?? ??????(0.01) ǰ?벿??(cpi) -------------------------------------------------------


y<-
  percent(
    mapply(function(x1, y1,a,b1,c,d,e,f,h,i,j,k) x1[[1]]+y1[[1]]+a[[1]]+b1[[1]]+c[[1]]+d[[1]]+e[[1]]+f[[1]]+
             h[[1]]+i[[1]]+j[[1]]+k[[1]],
           FB01.0.01.cpi, AT02.0.01.cpi, CF03.0.01.cpi, HW04.0.01.cpi, FH05.0.01.cpi, HL06.0.01.cpi, TR07.0.01.cpi, CM08.0.01.cpi,
           RC09.0.01.cpi,ED10.0.01.cpi,RH11.0.01.cpi, MS12.0.01.cpi)/121)
print(y)

# table 3???? ??????(0.05) ???벿??(lag) ------------------------------------------------------
y<-
  percent(
    mapply(function(x1, y1,a,b1,c,d,e,f,h,i,j,k) x1[[1]]+y1[[1]]+a[[1]]+b1[[1]]+c[[1]]+d[[1]]+e[[1]]+f[[1]]+
             h[[1]]+i[[1]]+j[[1]]+k[[1]],
           FB01.0.05.lag, AT02.0.05.lag, CF03.0.05.lag, HW04.0.05.lag, FH05.0.05.lag, HL06.0.05.lag, TR07.0.05.lag, CM08.0.05.lag,
           RC09.0.05.lag,ED10.0.05.lag,RH11.0.05.lag, MS12.0.05.lag)/121)

print(y)

# table 3???? ??3??(0.01) ???벿??(lag) ------------------------------------------------------
y<-
  percent(
    mapply(function(x1, y1,a,b1,c,d,e,f,h,i,j,k) x1[[1]]+y1[[1]]+a[[1]]+b1[[1]]+c[[1]]+d[[1]]+e[[1]]+f[[1]]+
             h[[1]]+i[[1]]+j[[1]]+k[[1]],
           FB01.0.01.lag, AT02.0.01.lag, CF03.0.01.lag, HW04.0.01.lag, FH05.0.01.lag, HL06.0.01.lag, TR07.0.01.lag, CM08.0.01.lag,
           RC09.0.01.lag,ED10.0.01.lag,RH11.0.01.lag, MS12.0.01.lag)/121)
print(y)

# # stepwise linear Regression --------------------------------------------




# function ----------------------------------------------------------------


# Automated model selection
# Author      : Joris Meys
# version     : 0.2
# date        : 12/01/09

#CHANGE LOG
# 0.2   : check for empty scopevar vector

# #CHANGE LOG -------------------------------------------------------------



# Function has.interaction checks whether x is part of a term in terms
# terms is a vector with names of terms from a model
has.interaction <- function(x,terms){
  out <- sapply(terms,function(i){
    sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
  })
  return(sum(out)>0)
}

# Function Model.select
# model is the lm object of the full model
# keep is a list of model terms to keep in the model at all times
# sig gives the significance for removal of a variable. Can be 0.1 too (see SPSS)
# verbose=T gives the F-tests, dropped var and resulting model after 
model.select <- function(model,keep,sig=0.05,verbose=F){
  counter=1
  # check input
  if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
  # calculate scope for drop1 function
  terms <- attr(model$terms,"term.labels")
  if(missing(keep)){ # set scopevars to all terms
    scopevars <- terms
  } else{            # select the scopevars if keep is used
    index <- match(keep,terms)
    # check if all is specified correctly
    if(sum(is.na(index))>0){
      novar <- keep[is.na(index)]
      warning(paste(
        c(novar,"cannot be found in the model",
          "\nThese terms are ignored in the model selection."),
        collapse=" "))
      index <- as.vector(na.omit(index))
    }
    scopevars <- terms[-index]
  }
  
  # Backward model selection : 
  
  while(T){
    # extract the test statistics from drop.
    test <- drop1(model, scope=scopevars,test="F")
    
    if(verbose){
      cat("-------------STEP ",counter,"-------------\n",
          "The drop statistics : \n")
      print(test)
    }
    
    pval <- test[,dim(test)[2]]
    
    names(pval) <- rownames(test)
    pval <- sort(pval,decreasing=T)
    
    if(sum(is.na(pval))>0) stop(paste("Model",
                                      deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))
    
    # check if all significant
    if(pval[1]<sig) break # stops the loop if all remaining vars are sign.
    
    # select var to drop
    i=1
    while(T){
      dropvar <- names(pval)[i]
      check.terms <- terms[-match(dropvar,terms)]
      x <- has.interaction(dropvar,check.terms)
      if(x){i=i+1;next} else {break}              
    } # end while(T) drop var
    
    if(pval[i]<sig) break # stops the loop if var to remove is significant
    
    if(verbose){
      cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
    }
    
    #update terms, scopevars and model
    scopevars <- scopevars[-match(dropvar,scopevars)]
    terms <- terms[-match(dropvar,terms)]
    
    formul <- as.formula(paste(".~.-",dropvar))
    model <- update(model,formul)
    
    if(length(scopevars)==0) {
      warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
      return()
    }
    counter=counter+1
  } # end while(T) main loop
  return(model)
}

# keep dummies ------------------------------------------------------------

keep.dummies <- c("quarter1","quarter3","quarter4",
                  "VAT1","VAT2","VAT3","Recession","Trend")
# stepwise cpi and divisions ----------------------------------------------


slr.cpi <- model.select(CPI_,keep = keep.dummies,sig = 0.05,verbose = F)

# -------------------------------------------------------------------------
slr.total <- list()
for (i in 2:length(regression)) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}
# ?ǵ?ɾ 

# 0.01 --------------------------------------------------------------------

slr.total <- list()
for (i in 2:length(regression)) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}
#which(lengths(slr.total)==0)显示哪个是缺失的
length( which(lengths(slr.total)==0))#统计slr.total缺失了多少个.


#
search_for_these <- c("lag1[, x]", "lag2[, x]", "lag3[, x]", "lag4[, x]")
replace_with_these <- c("lag1", "lag2", "lag3", "lag4")
found <- list()
#
for (i in 2:122) {
  found[[i]] <- match(names(regression[[i]]$coefficients), search_for_these, nomatch = 0)
  names(regression[[i]]$coefficients)[names(regression[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
}
for (i in 1:length(slr.total)) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
}

# -------------------------------------------------------------------------


# 01FOOD and their lower level------------------------------------------------------------------
slr.1<-list()
for (i in 2:15) {
  slr.1[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}
for (i in 2:length(slr.1)) {
  found[[i]] <- match(names(slr.1[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.1[[i]]$coefficients)[names(slr.1[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
}


stargazer(slr.1[2:7],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("quarter1","quarter3","quarter4",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[2:7])),
          summary = FALSE,
          out = "C:/Users/PC/Desktop/output/4.15 quarterly data/1-1.html",
          flip = FALSE,
          se = NULL, type = "html"
)

stargazer(slr.1[8:13],#6个数字
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("quarter1","quarter3","quarter4",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[9]),colnames(COPY[11:15])),#6个数
          summary = FALSE,
          out = "C:/Users/PC/Desktop/output/4.15 quarterly data/1-2.html",
          flip = FALSE,
          se = NULL, type = "html"
)

#library(texreg)
screenreg(slr.total[22])

# 2 Alcoholic beverages, tobacco and narcotics-----------------------------------------------------------------------
slr.2<- list()
for (i in 16:21) {
  slr.2[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}
for (i in 2:length(slr.2)) {
  found[[i]] <- match(names(slr.2[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.2[[i]]$coefficients)[names(slr.2[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
}
stargazer(slr.2[2:7],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("quarter1","quarter3","quarter4",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/4.15 quarterly data/2.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[16:21])),
          summary = FALSE,
          se = NULL, type = "html"
)
stargazer(slr.total[16:21],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("quarter1","quarter3","quarter4",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/4.15 quarterly data/2.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[16:21])),
          summary = FALSE,
          se = NULL, type = "html"
)
# 3 Clothing and footwear-----------------------------------------------------------------------
stargazer(slr.total[22:27],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("quarter1","quarter3","quarter4",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/4.15 quarterly data/2.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[22:27])),
          summary = FALSE,
          se = NULL, type = "html"
)

# 4 Housing, water, electricity, gas and other fuels-----------------------------------------------------------------------
stargazer(slr.total[28:40],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("quarter1","quarter3","quarter4",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/4.15 quarterly data/2.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[22:27])),
          summary = FALSE,
          se = NULL, type = "html"
)

# 5 Furnishings, household equipment and routine household maintenance-----------------------------------------------------------------------
stargazer(slr.total[28:40],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("quarter1","quarter3","quarter4",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/4.15 quarterly data/2.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[22:27])),
          summary = FALSE,
          se = NULL, type = "html"
)

# 6 Health-----------------------------------------------------------------------


# 07 Transport -----------------------------------------------------------------------


# 08 Communication -----------------------------------------------------------------------


# 09 Recreation and culture -----------------------------------------------------------------------


# 10 Education ----------------------------------------------------------------------


# 11 Restaurants and hotels ----------------------------------------------------------------------


# 12 Miscellaneous goods and services ----------------------------------------------------------------------

# CPI ---------------------------------------------------------------------


stargazer(CPI_, slr.cpi,
          star.char = c("*"), 
          star.cutoffs = c(0.05, 0.01),
          notes = c(" * p<0.05; ** p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,
                   "time_Mar","time_May","time_Nov","time_Oct","time_Sep", "VAT1","VAT2",
                   "VAT3","Recession","Trend",'Constant'), 
          dep.var.labels.include = FALSE, 
          notes.append = FALSE, # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), 
          report = "vc*", align = TRUE, header = FALSE, df = FALSE, digits = 2, single.row = TRUE,
          model.numbers = FALSE, column.labels = c("CPI","CPI(Stepwise)"), 
          summary = FALSE, out = "C:/Users/PC/Desktop/output/4.15 quarterly data/stepwise-cpi.html", flip = FALSE, se = NULL, type = "html" )
