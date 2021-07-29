rm(list = ls())

# DATA INPUT --------------------------------------------------------------


library(readxl)
cpi <- read_excel("cpi data/cpi+division+group.xlsx")
library(data.table)

setDF(cpi)#convert to 'data.frame'

#
cols.num <- c(colnames(cpi))
cpi[cols.num] <- sapply(cpi[cols.num],as.numeric)
#sapply(cpi,class)

cpi <- as.data.table(cpi)
colnames(cpi) = gsub("&", "and", colnames(cpi))
#class(cpi$`REPAIR OF HOUSEHOLD APPLIANCES1`)
COPY<-copy(cpi)
tracemem(COPY)==tracemem(cpi)

#LagCols <- colnames(cpi)
#LagLengths <- seq_len(12)
#for(y in LagCols){
#  for (z in LagLengths) set(cpi, j = eval(paste0(y,"_lag_",z)), value = shift(cpi[[y]],n = z, type = "lag"))
#}
Data_A = as.data.table (cpi)
#



#
Varnames <- copy(names(Data_A))
Data_A[, (names(Data_A)) := lapply(.SD, as.numeric)][,
                                                     rn := .I]
melted <- melt(Data_A, id.vars="rn")[,
                                     (paste0("lag_", 0:12)) := shift(value, 0:12, type="lag"),
                                     by=variable][, 
                                                  value:=NULL][]
res <- dcast.data.table(melt(melted, id.vars=c("rn", "variable"), variable.name="lag"),
                        rn ~ variable + lag, sum)

#view results
res[, ncol(res), with=FALSE]
# lagged variable generation ----------------------------------------------


cpi <- cpi[, unlist(lapply(.SD, shift, n = 0:12), recursive = FALSE)]#lags generation


# Monthly Dummies ---------------------------------------------------------


cpi$time <- seq.Date(from = as.Date("1988/01/01",format = "%Y/%m/%d"), by = "month", length.out = 387)
cpi$time <- format(cpi$time, format = "%b")
Sys.setlocale("LC_TIME","English")
cpi <- fastDummies::dummy_cols(cpi, select_columns = "time", remove_most_frequent_dummy = TRUE,
)
cpi$time1 <- seq.Date(from = as.Date("1988/01/01",format = "%Y/%m/%d"), by = "month", length.out = 387)
#



# Extract time from 1993-2019 ---------------------------------------------


library(lubridate)#extract cpi time to newdata(furniture48)
newdata <- with(cpi, cpi[(time1) >= "1993-01-01" & (time1) <= "2019-12-01", ])#start from 93.1
Trend <- seq_along(newdata$time1)
#sapply(newdata, class)



# deal with copy ----------------------------------------------------------


COPY$time1 <- seq.Date(from = as.Date("1988/01/01",format = "%Y/%m/%d"), by = "month", length.out = 387)
COPY <- with(COPY, COPY[(time1) >= "1993-01-01" & (time1) <= "2019-12-01", ])
COPY = subset(COPY, select = -c(time1) )
#lag  

# LAG ---------------------------------------------------------------------

library(dplyr) 
zlag1 <- newdata %>% 
  select(matches("(2)$"))
zlag2 <- newdata %>% 
  select(matches("(3)$"))
lag3 <- newdata %>% 
  select(matches("(4)$"))
lag4 <- newdata %>% 
  select(matches("(5)$"))
lag5 <- newdata %>% 
  select(matches("(6)$"))
lag6 <- newdata %>% 
  select(matches("(7)$"))
lag7 <- newdata %>% 
  select(matches("(8)$"))
lag8 <- newdata %>% 
  select(matches("(9)$"))
lag9 <- newdata %>% 
  select(matches("(10)$"))
lag10 <- newdata %>% 
  select(matches("(11)$"))
lag11 <- newdata %>% 
  select(matches("(12)$"))
lag12 <- newdata %>% 
  select(matches("(13)$"))

library(tidyverse) 
lag1 <- zlag1 %>% select(-contains("12"))

lag2 <- zlag2 %>% select(-contains("13"))
VAT1 <- as.numeric(newdata$`time1` == "2008-12-01")
VAT2 <- as.numeric(newdata$`time1` == "2010-01-01")
VAT3 <- as.numeric(newdata$`time1` == "2011-01-01")
Recession <- as.numeric(newdata$`time1` >= "2008-04-01" & newdata$`time1`<= "2009-06-01")


# MONTYHLY DUMMIES --------------------------------------------------------


time_Aug <- as.numeric(newdata$time_Aug)
time_Dec <- as.numeric(newdata$time_Dec)
time_Feb <- as.numeric(newdata$time_Apr)#原本是4月 改成2月
time_Jan <- as.numeric(newdata$time_Jan)
time_Jul <- as.numeric(newdata$time_Jul)
time_Jun <- as.numeric(newdata$time_Jun)
time_Mar <- as.numeric(newdata$time_Mar)
time_May <- as.numeric(newdata$time_May)
time_Nov <- as.numeric(newdata$time_Nov)
time_Oct <- as.numeric(newdata$time_Oct)
time_Sep <- as.numeric(newdata$time_Sep)
#
#time_Aug <- as.numeric(time_Aug)
#reg.model <- lm(formula = `newdata` ~ `lag2` + `lag3`  + `lag4` + `lag5` + 
#                  `lag6` + `lag7` + `lag8` + `lag9` + `lag10` + `lag11` + `lag12` + time_Aug + time_Dec+ time_Feb+ time_Jan+time_Jul+time_Jun +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend, data=newdata)

COPY <- as.data.frame(COPY)
lag1 <- as.data.frame(lag1)
lag2 <- as.data.frame(lag2)
lag3 <- as.data.frame(lag3)
lag4 <- as.data.frame(lag4)
lag5 <- as.data.frame(lag5)
lag6 <- as.data.frame(lag6)
lag7 <- as.data.frame(lag7)
lag8 <- as.data.frame(lag8)
lag9 <- as.data.frame(lag9)
lag10 <- as.data.frame(lag10)
lag11 <- as.data.frame(lag11)#174 ????��һ??????Ϊvat1??2??3
lag12 <- as.data.frame(lag12)
#if add vat dummmies in cpi step, lag1 2 would be exist vat1,2

# CPI-LAGS ----------------------------------------------------------------


CPI_lag1 <- (newdata$"CPI ALL ITEMS2")
CPI_lag2 <- (newdata$"CPI ALL ITEMS3")
CPI_lag3 <- (newdata$"CPI ALL ITEMS4")
CPI_lag4 <- (newdata$"CPI ALL ITEMS5")
CPI_lag5 <- (newdata$"CPI ALL ITEMS6")
CPI_lag6 <- (newdata$"CPI ALL ITEMS7")
CPI_lag7 <- (newdata$"CPI ALL ITEMS8")
CPI_lag8 <- (newdata$"CPI ALL ITEMS9")
CPI_lag9 <- (newdata$"CPI ALL ITEMS10")
CPI_lag10 <- (newdata$"CPI ALL ITEMS11")
CPI_lag11 <- (newdata$"CPI ALL ITEMS12")
CPI_lag12 <- (newdata$"CPI ALL ITEMS13")



# Regression Equation -----------------------------------------------------


reg.model <- lapply(1:173, function(x) lm(COPY[,x] ~ lag1[,x]+lag2[,x]+lag3[,x]+lag4[,x]+
                                            lag5[,x]+lag6[,x]+lag7[,x]+lag8[,x]+lag9[,x]+
                                            lag10[,x]+lag11[,x]+lag12[,x]+ time_Aug + 
                                            time_Dec+ time_Feb+ time_Jan+time_Jul+time_Jun +
                                            time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend)
)

for (i in 1:173) {
  names(reg.model[[i]]$coefficients)<-c('Constant','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
                                        "time_Aug" , 
                                        "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar",
                                        "time_May","time_Nov","time_Oct","time_Sep","VAT1","VAT2","VAT3","Recession","Trend")
}
#regression <- lapply(1:173, function(x) lm(COPY[,x] ~ CPI_lag1+CPI_lag2+CPI_lag3+CPI_lag4+CPI_lag5+CPI_lag6+CPI_lag7+CPI_lag8+CPI_lag9+
#                                           CPI_lag10+CPI_lag11+CPI_lag12+lag1[,x]+lag2[,x]+lag3[,x]+lag4[,x]+lag5[,x]+lag6[,x]+lag7[,x]+
#                                             lag8[,x]+lag9[,x]+lag10[,x]+lag11[,x]+lag12[,x]+ time_Aug + 
#                                            time_Dec+ time_Feb+ time_Jan+time_Jul+time_Jun +time_Mar+
#                                             time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend)
#)
regression <- lapply(1:173, function(x) lm(COPY[,x] ~ lag1[,x]+lag2[,x]+lag3[,x]+lag4[,x]+lag5[,x]+lag6[,x]+lag7[,x]+
                                             lag8[,x]+lag9[,x]+lag10[,x]+lag11[,x]+lag12[,x]+CPI_lag1+CPI_lag2+CPI_lag3+CPI_lag4+CPI_lag5+CPI_lag6+CPI_lag7+
                                             CPI_lag8+CPI_lag9+
                                             CPI_lag10+CPI_lag11+CPI_lag12+ time_Aug + 
                                             time_Dec+ time_Feb+ time_Jan+time_Jul+time_Jun +time_Mar+
                                             time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend)
)
for (i in 1:173) {
  names(regression[[i]]$coefficients)<-c('Constant','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
                                         "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4","CPI_lag5","CPI_lag6","CPI_lag7","CPI_lag8","CPI_lag9",
                                         "CPI_lag10","CPI_lag11","CPI_lag12","time_Aug" , 
                                         "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar",
                                         "time_May","time_Nov","time_Oct","time_Sep","VAT1","VAT2","VAT3","Recession","Trend")
}

# model select-------------------------------------------------------------------------

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


# -------------------------------------------------------------------------
keep.dummies <- c(
  "time_Aug",
  "time_Dec", "time_Feb", "time_Jan", "time_Jul", "time_Jun", "time_Mar", "time_May", "time_Nov", "time_Oct", "time_Sep",
  "VAT1", "VAT2", "VAT3", "Recession", "Trend"
)
# ?ٷֺ? ---------------------------------------------------------------------

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
percent(1)
# -------------------------------------------------------------------------


slr.total <- list()
for (i in 2:length(regression)) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}
for (i in 1:3) {
  a[[i]] <- lapply(slr.total[2:122], function(x) summary(x)$coefficients[paste0("VAT",i), 4])
  
  print(percent(length(which(a[[i]] < 0.01))/121))
}


a=list()
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  print(percent(length(which(a[[i]] < 0.05))/121))
  #print(a[[i]])
}
# Weight ------------------------------------------------------------------
weight<- list(0.109,	0.042,	0.063,	0.115,	0.067,	0.022,	0.152,	0.023,	0.152,	0.019,	0.137,	0.099
              
)
#FB01 --------------------------------------------------------------------
a=list()
FB01.0.05.cpi<-list()
FB01.0.01.cpi<-list()
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:15], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  print(length(which(a[[i]] < 0.01)))
  FB01.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[1]]
  #print(length(which(a[[i]] < 0.01))*weight[[1]])
  FB01.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[1]]
  #print(percent(length(which(a[[i]] < 0.05))/14))
  #print(percent(length(which(a[[i]] < 0.01))/14))
}
FB01.0.05.lag<-list()
FB01.0.01.lag<-list()
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:15], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  #print(length(which(a[[i]] < 0.05)))
  FB01.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[1]]
  print(length(which(a[[i]] < 0.01)))
  FB01.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[1]]
}

