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
time_Feb <- as.numeric(newdata$time_Feb)
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
lag11 <- as.data.frame(lag11)#174 多出来一个是因为vat1，2，3
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
#regression[[1]]$coefficients[14]
#names(regression[[1]]$coefficients[14])
#names(regression$coefficients)<-c('Intercept','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
#                                  "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4","CPI_lag5","CPI_lag6","CPI_lag7","CPI_lag8","CPI_lag9",
#                                  "CPI_lag10","CPI_lag11","CPI_lag12","time_Aug" , 
#                                  "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar",
#                                  "time_May","time_Nov","time_Oct","time_Sep","VAT1","VAT2","VAT3","Recession","Trend")

#names(regression[1:173]$coefficients[14])

# get col position --------------------------------------------------------

#which( colnames(furniture48)=='REPAIR OF HOUSEHOLD APPLIANCES1')
#library("writexl")
#write_xlsx(furniture48,"C:\\Users\\PC\\Desktop\\furniture48.xlsx")



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

#FB01.model <- lm(formula = `FB011` ~ `CPI2` + `CPI3`  + `CPI4` + `CPI5` + 
#                  `CPI6` + `CPI7` + `CPI8` + `CPI9` + `CPI10` + `CPI11` + 
#                  `CPI12` + `CPI13` + time_Aug + time_Dec+ time_Feb+ time_Jan+
#                  time_Jul+time_Jun +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+recession+trend+
#                   `FB012`+`FB013`+`FB014`+`FB015`+`FB016`+`FB017`+`FB018`+`FB019`+`FB0110`+`FB0111`+`FB0112`+`FB0113`
#                  , data=newdata)

#HTML ALL OUTPUT(BEFORE SIMPLY) ----------------------------------------------


library(sandwich)
cov <- vcovHC(CPI_, type = "HC")
robust.se <- sqrt(diag(cov))
library(stargazer)


stargazer(CPI_, FB01,AT02,CF03,HW04,FH05,HL06,TR07,CM08,RC09,ED10,RH11,MS12,
          star.cutoffs = c(0.05, 0.01),
           
          report = "vc*",
          align=TRUE,           
          header = FALSE, 
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# replace & with AND---------------------------------------------------------------

colnames(COPY) = gsub("&", "and", colnames(COPY))
# HTML CLASSES OUTPUT (BEFORE SIMPLY) -------------------------------------

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


# 02ALCOHOLIC BEVERAGES AND TOBACCO lower level ---------------------------------------
stargazer(regression[16:21],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[16:21])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")


# 03clothing and footwear lower level -------------------------------------
stargazer(regression[22:27],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[22:27])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 04housing ---------------------------------------------------------------
stargazer(regression[28:40],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[28:40])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 05furniture -------------------------------------------------------------
#(missing #REPAIR OF HOUSEHOLD APPLIANCES# positition 48)

stargazer(regression[41:47],regression[49:53],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[41:47]),colnames(COPY[49:53])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")



# 06health ----------------------------------------------------------------
#missing  57-61 |57 58 59 60 61
stargazer(regression[54:56],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[54:56])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")


# 07transport -------------------------------------------------------------
#missing 65,75,76
stargazer(regression[62:64],regression[66:74],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[62:64]),colnames(COPY[66:74])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 08communication ---------------------------------------------------------

stargazer(regression[77:79],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[77:79])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")


# 09recreation ------------------------------------------------------------
#missing 84,86，87，88，91，101
stargazer(regression[80:83],regression[85],regression[89:90],regression[92:100],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[80:83]),colnames(COPY[85]),colnames(COPY[89:90]),colnames(COPY[92:100])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 10EDUCATION -------------------------------------------------------------

stargazer(regression[102],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[102])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 11HOTEL -----------------------------------------------------------------
#107 missing
stargazer(regression[103:106],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[103:106])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 12MISCELLANEOUS GOODS AND SERVICES --------------------------------------
#115，118 missing
stargazer(regression[108:114], regression[116:117],regression[119:122],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[108:114]),colnames(COPY[116:117]),colnames(COPY[119:122])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

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

#\
# #names(slr.cpi$coefficients)<-c('Constant','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
#                             "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4","CPI_lag5","CPI_lag6","CPI_lag7","CPI_lag8","CPI_lag9",
#                             "CPI_lag10","CPI_lag11","CPI_lag12","time_Aug" , 
#                             "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar",
#                             "time_May","time_Nov","time_Oct","time_Sep","VAT1","VAT2","VAT3","Recession","Trend")

#if ("lag12[, x]"  %in% names(slr.cpi$coefficients))
#  {"lag1[,x]"<-c("lag1")}
# LVEAVE MONTHLY AND OTHERS DUMMIES IN ------------------------------------
keep.dummies <- c("time_Aug",
                  "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                  "VAT1","VAT2","VAT3","Recession","Trend")

# stepwise cpi and divisions ----------------------------------------------


slr.cpi <- model.select(CPI_,keep = keep.dummies,sig = 0.05,verbose = F)
# replace lag[,x] for stepwise output -------------------------------------


search_for_these <- c("lag1[, x]","lag2[, x]","lag3[, x]","lag4[, x]","lag5[, x]","lag6[, x]","lag7[, x]","lag8[, x]","lag9[, x]","lag10[, x]","lag11[, x]","lag12[, x]")
 replace_with_these <- c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12")
 
found <- match(names(slr.cpi$coefficients), search_for_these, nomatch = 0)
names(slr.cpi$coefficients)[names(slr.cpi$coefficients) %in% search_for_these] <- replace_with_these[found]
#
slr.list <- list(slr.table01.food, slr.table02.tobacco)
found <- list()
for (i in 1:15) {
  found[[i]] <- match(names(slr.table01.food[[i]]$coefficients), search_for_these, nomatch = 0)
  #names(slr.list[[i]]$coefficients)[names(slr.list[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found]
  
}
#FB01,AT02,CF03,HW04,FH05,HL06,TR07,CM08,RC09,ED10,RH11,MS12,
slr.FB01 <- model.select(FB01,keep = keep.dummies,sig = 0.05,verbose = F)
slr.AT02 <- model.select(AT02,keep = keep.dummies,sig = 0.05,verbose = F)
slr.CF03 <- model.select(CF03,keep = keep.dummies,sig = 0.05,verbose = F)
slr.HW04 <- model.select(HW04,keep = keep.dummies,sig = 0.05,verbose = F)
slr.FH05 <- model.select(FH05,keep = keep.dummies,sig = 0.05,verbose = F)
slr.HL06 <- model.select(HL06,keep = keep.dummies,sig = 0.05,verbose = F)
slr.TR07 <- model.select(TR07,keep = keep.dummies,sig = 0.05,verbose = F)
slr.CM08 <- model.select(CM08,keep = keep.dummies,sig = 0.05,verbose = F)
slr.RC09 <- model.select(RC09,keep = keep.dummies,sig = 0.05,verbose = F)
slr.ED10 <- model.select(ED10,keep = keep.dummies,sig = 0.05,verbose = F)
slr.RH11 <- model.select(RH11,keep = keep.dummies,sig = 0.05,verbose = F)
slr.MS12 <- model.select(MS12,keep = keep.dummies,sig = 0.05,verbose = F)
division.afterremove<-list(slr.cpi,slr.FB01,slr.AT02,slr.CF03,slr.HW04,slr.FH05,slr.HL06,slr.TR07,slr.CM08,slr.RC09,slr.ED10,slr.RH11,slr.MS12)



# stepwise regression for lower level coicop divisions --------------------
# test
#slr.FB01.test <- model.select(regression[[2]],keep = keep.dummies,sig = 0.05,verbose = F)#equal to slr.FB01




#bp test for heteroskedasticity
library(zoo)
library(lmtest)


bptest.result <- lapply(division.afterremove, bptest)
#DW test
dwtest.result <- lapply(division.afterremove, dwtest)

#HTML Results after Simplify each division ------------------------------------


stargazer(slr.cpi,slr.FB01,slr.AT02,slr.CF03,slr.HW04,slr.FH05,slr.HL06,slr.TR07,slr.CM08,slr.RC09,slr.ED10,slr.RH11,slr.MS12,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE, 
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html",
          add.lines = list(c("BP test(P.value)","0.04844","0.002688","0.0005043","0.007547","0.0008755","0.3513","0.000008181","0.0004531","0.3741",
                             "0.006611","0.08099","0.0005024","0.000073"),
                           c("DW test(P.value)", "0.4298","0.3854","0.5829","0.7541","0.4828","0.3513","0.7902","0.6817","0.725",
                             ".4751","0.4463","0.8805","0.9852" )
                           ),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/division.txt"
)

# stagazer for lower level: -----------------------------------------------

  

# 01food 01 level -----------------------------------------------------------
slr.table01.food <- list()
for (i in 2:15) {
  slr.table01.food[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

#found <- match(names(slr.table01.food[[9]]$coefficients), search_for_these, nomatch = 0)
#names(slr.table01.food[[9]]$coefficients)[names(slr.table01.food[[9]]$coefficients) %in% search_for_these] <- replace_with_these[found]

found<-list()
for (i in 2:15) {
  found[[i]] <- match(names(slr.table01.food[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.table01.food[[i]]$coefficients)[names(slr.table01.food[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
  
}
library(stargazer)
stargazer(slr.table01.food[2:15],
          star.char = c("*", "**"),
          star.cutoffs = c(0.05, 0.01),
          notes = c(" * p<0.05; ** p<0.01; "),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          #order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align=TRUE,           
          header = FALSE, 
          df=FALSE, 
          digits=2, single.row = TRUE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[2:15])),
          summary=FALSE,
          se = NULL, type = "html")

# 02tobacco 02 lower level --------------------------------------------------
slr.table02.tobacco <- list()
for (i in 16:21) {
  slr.table02.tobacco[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table02.tobacco[16:21],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[16:21])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")



# 03clothing  -------------------------------------------------------------
slr.table03.clothing <- list()
for (i in 22:27) {
  slr.table03.clothing[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table03.clothing[22:27],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[22:27])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")


# 04 housing --------------------------------------------------------------
slr.table04.housing <- list()
for (i in 28:40) {
  slr.table04.housing[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table04.housing[28:40],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[28:40])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 05furniture -------------------------------------------------------------
#(missing #REPAIR OF HOUSEHOLD APPLIANCES# positition 48)
slr.table05.furniture <- list()
for (i in 41:53) {
  if(i==48) next
  slr.table05.furniture[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table05.furniture[41:47],slr.table05.furniture[49:53],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[41:47]),colnames(COPY[49:53])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")




# 06health ----------------------------------------------------------------
#missing  57-61
slr.table06.health <- list()
for (i in 54:56) {
  slr.table06.health[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table06.health[54:56],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[54:56])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 07transport -------------------------------------------------------------
#missing 65,75,76

slr.table07.transport <- list()
for (i in 62:76) {
  if(i==65||i==75||i==76) next
  slr.table07.transport[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table07.transport[62:64],slr.table07.transport[66:74],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[62:64]),colnames(COPY[66:74])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")
# 08communication ---------------------------------------------------------
slr.table08.communication <- list()
for (i in 77:79) {
  slr.table08.communication[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table08.communication[77:79],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[77:79])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 09recreation ------------------------------------------------------------
#missing 84,86，87，88，91，101
slr.table09.recreation <- list()
for (i in 80:101) {
  if(i==84||i==86||i==87||i==88||i==91||i==101) next
  slr.table09.recreation[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table09.recreation[80:83],slr.table09.recreation[85],
          slr.table09.recreation[89:90],slr.table09.recreation[92:100],
          
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[80:83]),colnames(COPY[85]),colnames(COPY[89:90]),colnames(COPY[92:100])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 10EDUCATION -------------------------------------------------------------
slr.table10.eudcation <- list()
for (i in 102) {
  slr.table10.eudcation[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}
stargazer(slr.table10.eudcation[102],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[102])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 11HOTEL -----------------------------------------------------------------
#107 missing
slr.table11.hotel <- list()
for (i in 103:107) {
  if(i==107) next
  slr.table11.hotel[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table11.hotel[103:106],
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[103:106])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 12MISCELLANEOUS GOODS AND SERVICES --------------------------------------
#115，118 missing
slr.table12.MS<- list()
for (i in 108:122) {
  if(i==115||i==118) next
  slr.table12.MS[[i]] <-  model.select(regression[[i]],keep = keep.dummies,sig = 0.05,verbose = F)
  
}

stargazer(slr.table12.MS[108:114], slr.table12.MS[116:117],slr.table12.MS[119:122],
          
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          column.labels = c(colnames(COPY[108:114]),colnames(COPY[116:117]),colnames(COPY[119:122])),
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# furniture48 -------------------------------------------------------------


furniture48<-with(cpi, cpi[(time1) >= "1994-02-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(furniture48$time1)
# furniture 48 ------------------------------------------------------------
FH05.48<-lm(`REPAIR OF HOUSEHOLD APPLIANCES1` ~ `REPAIR OF HOUSEHOLD APPLIANCES2`+
              `REPAIR OF HOUSEHOLD APPLIANCES3`+`REPAIR OF HOUSEHOLD APPLIANCES4`+
              `REPAIR OF HOUSEHOLD APPLIANCES5`+`REPAIR OF HOUSEHOLD APPLIANCES6`+
              `REPAIR OF HOUSEHOLD APPLIANCES7`+`REPAIR OF HOUSEHOLD APPLIANCES8`+
              `REPAIR OF HOUSEHOLD APPLIANCES9`+`REPAIR OF HOUSEHOLD APPLIANCES10`+
              `REPAIR OF HOUSEHOLD APPLIANCES11`+`REPAIR OF HOUSEHOLD APPLIANCES12`+
              `REPAIR OF HOUSEHOLD APPLIANCES13`+`CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = furniture48)
#``
# 05 furniture 48 ---------------------------------------------------------
stargazer(FH05.48,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 05 furniture 48 ---------------------------------------------------------



slr.table05.furniture.48 <-  model.select(FH05.48,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table05.furniture.48,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")
# health other medical 57 -------------------------------------------------
health57<-with(cpi, cpi[(time1) >= "1996-01-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health57$time1)
# health 57 ---------------------------------------------------------------
#missing  57-61
HL06.57<-lm(`OTHER MEDICAL and THERAPEUTIC EQUIPMENT1` ~ `OTHER MEDICAL and THERAPEUTIC EQUIPMENT12`+
              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT3`+              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT4`+
              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT5`+
              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT6`+              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT7`+
              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT8`+
              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT9`+              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT10`+
              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT11`+
              `OTHER MEDICAL and THERAPEUTIC EQUIPMENT12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health57)

#health 57 ----------------------------------------------------------------------


stargazer(HL06.57,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# health 57 step wise stargazer -------------------------------------------


slr.table06.HL06.57 <-  model.select(HL06.57,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.57,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")






# health other medical 58 -------------------------------------------------
health58<-with(cpi, cpi[(time1) >= "2001-01-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health58$time1)
# health 58 ---------------------------------------------------------------

HL06.58<-lm(`OUT-PATIENT SERVICES1` ~ `OUT-PATIENT SERVICES2`+
              `OUT-PATIENT SERVICES3`+              `OUT-PATIENT SERVICES4`+
              `OUT-PATIENT SERVICES5`+
              `OUT-PATIENT SERVICES6`+              `OUT-PATIENT SERVICES7`+
              `OUT-PATIENT SERVICES8`+
              `OUT-PATIENT SERVICES9`+              `OUT-PATIENT SERVICES10`+
              `OUT-PATIENT SERVICES11`+
              `OUT-PATIENT SERVICES12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health58)

#health 58 ----------------------------------------------------------------------


stargazer(HL06.58,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# health 58 step wise stargazer -------------------------------------------


slr.table06.HL06.58 <-  model.select(HL06.58,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.58,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")







#-------------------------------------------
  # health other medical 59 -------------------------------------------------
health59<-with(cpi, cpi[(time1) >= "2001-01-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health58$time1)
# health 59 ---------------------------------------------------------------

HL06.59<-lm(`MEDICAL SERVICES AND PARAMEDICAL SERVICES1` ~ `MEDICAL SERVICES AND PARAMEDICAL SERVICES2`+
              `MEDICAL SERVICES AND PARAMEDICAL SERVICES3`+              `MEDICAL SERVICES AND PARAMEDICAL SERVICES4`+
              `MEDICAL SERVICES AND PARAMEDICAL SERVICES5`+
              `MEDICAL SERVICES AND PARAMEDICAL SERVICES6`+              `MEDICAL SERVICES AND PARAMEDICAL SERVICES7`+
              `MEDICAL SERVICES AND PARAMEDICAL SERVICES8`+
              `MEDICAL SERVICES AND PARAMEDICAL SERVICES9`+              `MEDICAL SERVICES AND PARAMEDICAL SERVICES10`+
              `MEDICAL SERVICES AND PARAMEDICAL SERVICES11`+
              `MEDICAL SERVICES AND PARAMEDICAL SERVICES12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health59)

#health 59 ----------------------------------------------------------------------


stargazer(HL06.59,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# health 59 step wise stargazer -------------------------------------------


slr.table06.HL06.59 <-  model.select(HL06.59,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.59,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")
---------------------------------------------------
  
  
  
  # health other medical 60 -------------------------------------------------
health60<-with(cpi, cpi[(time1) >= "2001-01-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health60$time1)
# health 60 ---------------------------------------------------------------

HL06.60<-lm(`DENTAL SERVICES1` ~ `DENTAL SERVICES2`+
              `DENTAL SERVICES3`+              `DENTAL SERVICES4`+
              `DENTAL SERVICES5`+
              `DENTAL SERVICES6`+              `DENTAL SERVICES7`+
              `DENTAL SERVICES8`+
              `DENTAL SERVICES9`+              `DENTAL SERVICES10`+
              `DENTAL SERVICES11`+
              `DENTAL SERVICES12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health60)

#health 60 ----------------------------------------------------------------------


stargazer(HL06.60,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# health 60 step wise stargazer -------------------------------------------


slr.table06.HL06.60 <-  model.select(HL06.60,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.60,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 
----------------------------------------------------------------------------------------------------------------  
  # health other medical 61 -------------------------------------------------
health61<-with(cpi, cpi[(time1) >= "2002-01-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health61$time1)
# health 61 ---------------------------------------------------------------

HL06.61<-lm(`HOSPITAL SERVICES1` ~ `HOSPITAL SERVICES2`+
              `HOSPITAL SERVICES3`+              `HOSPITAL SERVICES4`+
              `HOSPITAL SERVICES5`+
              `HOSPITAL SERVICES6`+              `HOSPITAL SERVICES7`+
              `HOSPITAL SERVICES8`+
              `HOSPITAL SERVICES9`+              `HOSPITAL SERVICES10`+
              `HOSPITAL SERVICES11`+
              `HOSPITAL SERVICES12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health61)

#health 61 ----------------------------------------------------------------------


stargazer(HL06.61,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# health 61 step wise stargazer -------------------------------------------


slr.table06.HL06.61 <-  model.select(HL06.61,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.61,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 
  
-----------------------------------------------------------------------------------------------------
  # new cars 65 -------------------------------------------------
health65<-with(cpi, cpi[(time1) >= "1997-02-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health65$time1)
#65 ---------------------------------------------------------------

HL06.65<-lm(`NEW CARS1` ~ `NEW CARS2`+
              `NEW CARS3`+              `NEW CARS4`+
              `NEW CARS5`+
              `NEW CARS6`+              `NEW CARS7`+
              `NEW CARS8`+
              `NEW CARS9`+              `NEW CARS10`+
              `NEW CARS11`+
              `NEW CARS12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health65)

#65 ----------------------------------------------------------------------


stargazer(HL06.65,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 65 step wise stargazer -------------------------------------------


slr.table06.HL06.65 <-  model.select(HL06.65,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.65,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 

-----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------
  # PASSENGER  TRANSPORT BY AIR  75 -------------------------------------------------
health75<-with(cpi, cpi[(time1) >= "1997-02-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health75$time1)
#75 ---------------------------------------------------------------

HL06.75<-lm(`PASSENGER TRANSPORT BY AIR1` ~ `PASSENGER TRANSPORT BY AIR2`+
              `PASSENGER TRANSPORT BY AIR3`+              `PASSENGER TRANSPORT BY AIR4`+
              `PASSENGER TRANSPORT BY AIR5`+
              `PASSENGER TRANSPORT BY AIR6`+              `PASSENGER TRANSPORT BY AIR7`+
              `PASSENGER TRANSPORT BY AIR8`+
              `PASSENGER TRANSPORT BY AIR9`+              `PASSENGER TRANSPORT BY AIR10`+
              `PASSENGER TRANSPORT BY AIR11`+
              `PASSENGER TRANSPORT BY AIR12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health75)

#75 ----------------------------------------------------------------------


stargazer(HL06.75,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 75 step wise stargazer -------------------------------------------


slr.table06.HL06.75 <-  model.select(HL06.75,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.75,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 

-----------------------------------------------------------------------------------------------------
  
  # PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY  76 -------------------------------------------------
health76<-with(cpi, cpi[(time1) >= "1997-02-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health76$time1)
#76 ---------------------------------------------------------------

HL06.76<-lm(`PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY1` ~ `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY2`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY3`+              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY4`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY5`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY6`+              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY7`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY8`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY9`+              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY10`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY11`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health76)

#76 ----------------------------------------------------------------------


stargazer(HL06.76,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 76 step wise stargazer -------------------------------------------


slr.table06.HL06.76 <-  model.select(HL06.76,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.76,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 

-----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------
  
  # PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY  76 -------------------------------------------------
health76<-with(cpi, cpi[(time1) >= "1997-02-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health76$time1)
#76 ---------------------------------------------------------------

HL06.76<-lm(`PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY1` ~ `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY2`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY3`+              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY4`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY5`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY6`+              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY7`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY8`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY9`+              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY10`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY11`+
              `PASSENGER TRANSPORT BY SEA AND INLAND WATERWAY12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health76)

#76 ----------------------------------------------------------------------


stargazer(HL06.76,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 76 step wise stargazer -------------------------------------------


slr.table06.HL06.76 <-  model.select(HL06.76,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.76,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 

-----------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------
  
  # DATA  PROCESSING EQUIPMENT   84 -------------------------------------------------
health84<-with(cpi, cpi[(time1) >= "1992-01-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health84$time1)
#84 ---------------------------------------------------------------

HL06.84<-lm(`DATA PROCESSING EQUIPMENT1` ~ `DATA PROCESSING EQUIPMENT2`+
              `DATA PROCESSING EQUIPMENT3`+              `DATA PROCESSING EQUIPMENT4`+
              `DATA PROCESSING EQUIPMENT5`+
              `DATA PROCESSING EQUIPMENT6`+              `DATA PROCESSING EQUIPMENT7`+
              `DATA PROCESSING EQUIPMENT8`+
              `DATA PROCESSING EQUIPMENT9`+              `DATA PROCESSING EQUIPMENT10`+
              `DATA PROCESSING EQUIPMENT11`+
              `DATA PROCESSING EQUIPMENT12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health84)

#84 ----------------------------------------------------------------------


stargazer(HL06.84,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 86 step wise stargazer -------------------------------------------


slr.table06.HL06.84 <-  model.select(HL06.84,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.84,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 

-----------------------------------------------------------------------------------------------------
  
  -----------------------------------------------------------------------------------------------------
  
  #REPAIR OF  AUDIO-VISUAL & RELATED PRODUCTS    86 -------------------------------------------------
health84<-with(cpi, cpi[(time1) >= "1997-02-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health84$time1)
#86 ---------------------------------------------------------------

HL06.84<-lm(`REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS1` ~ `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS2`+
              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS3`+              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS4`+
              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS5`+
              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS6`+              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS7`+
              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS8`+
              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS9`+              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS10`+
              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS11`+
              `REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health84)

#86 ----------------------------------------------------------------------


stargazer(HL06.86,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 84 step wise stargazer -------------------------------------------


slr.table06.HL06.86 <-  model.select(HL06.86,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.86,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 


-----------------------------------------------------------------------------------------------------
  #OTHER  MAJOR DURABLES FOR RECREATION AND CULTURE   87 -------------------------------------------------
health87<-with(cpi, cpi[(time1) >= "2001-01-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health87$time1)
#87 ---------------------------------------------------------------

HL06.87<-lm(`OTHER MAJOR DURABLES FOR RECREATION AND CULTURE1` ~ `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE2`+
              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE3`+              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE4`+
              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE5`+
              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE6`+              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE7`+
              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE8`+
              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE9`+              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE10`+
              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE11`+
              `OTHER MAJOR DURABLES FOR RECREATION AND CULTURE12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health84)

#87 ----------------------------------------------------------------------


stargazer(HL06.87,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 87 step wise stargazer -------------------------------------------


slr.table06.HL06.87 <-  model.select(HL06.87,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.87,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 

-----------------------------------------------------------------------------------------------------
  
  -----------------------------------------------------------------------------------------------------
  #MAJOR  DURABLES FOR IN/OUTDOOR RECREATION    88 -------------------------------------------------
health88<-with(cpi, cpi[(time1) >= "2001-01-01" & (time1) <= "2019-12-01", ])
Trend <- seq_along(health88$time1)
#88 ---------------------------------------------------------------

HL06.88<-lm(`MAJOR DURABLES FOR IN/OUTDOOR RECREATION1` ~ `MAJOR DURABLES FOR IN/OUTDOOR RECREATION2`+
              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION3`+              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION4`+
              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION5`+
              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION6`+              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION7`+
              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION8`+
              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION9`+              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION10`+
              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION11`+
              `MAJOR DURABLES FOR IN/OUTDOOR RECREATION12`+              
              `CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = health84)

#87 ----------------------------------------------------------------------


stargazer(HL06.87,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 87 step wise stargazer -------------------------------------------


slr.table06.HL06.87 <-  model.select(HL06.87,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.table06.HL06.87,
          star.cutoffs = c(0.05, 0.01),
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html") 

-----------------------------------------------------------------------------------------------------
  
  
  

COPY[misscolname[1]]
which(!is.na(COPY$misscolname[1]))
which(!is.na(cpi$`misscolnameafter1[1]`))
which( colnames(furniture48)=='REPAIR OF HOUSEHOLD APPLIANCES1')
a<-which(!is.na(cpi$`REPAIR OF AUDIO-VISUAL and RELATED PRODUCTS1`))

# -------------------------------------------------------------------------

#sapply(reg.model, coef)
cpi$VAT1 <- as.numeric(cpi$`time1` == "2008-12-01")
#print(cpi$"VAT1")
cpi$VAT2 <- as.numeric(cpi$`time1` == "2010-01-01")
cpi$VAT3 <- as.numeric(cpi$`time1` == "2011-01-01")
cpi$Recession <- as.numeric(cpi$`time1` >= "2008-04-01" & cpi$`time1`<= "2009-06-01")
#cpi$Trend <- seq_along(cpi$time1)
# auto selecte na division ------------------------------------------------


misscolname <- colnames(COPY)[colSums(is.na(COPY)) > 0]#find which col has na

misscolnameafter1<-paste(misscolname,"1",sep = "")#add 1 after 19 miss value column(lag_1),misscolnameafter1[1]="REPAIR OF HOUSEHOLD APPLIANCES1"
x <- c(1:13)
#one2nine <- paste0(misscolname, x)

#sadas<- list()
#for (i in 1:length(misscolname)) {
#  sadas[i] <- paste0(misscolname[i], x)
#}
#paste0(misscolname[1], x)
adssad<- list()
for (i in 1:length(misscolnameafter1)) {
  adssad[i]<-as.numeric(which( colnames(cpi)==misscolnameafter1[i]))#back na column position in number,eg:612th cola
#  a<-which(!is.na(cpi[,612]))#na col which rows start non-na
}
adssad
#which(!is.na(cpi[,612]))
#caonima<-c(612,729)
#for (i in caonima) {
#  print(which.min(is.na(cpi[,i])))
#}


#which.min(is.na(cpi$paste('`',misscolname[1], '`',)))

cpi<- as.data.frame(cpi)
x1 <- vector("numeric")
for (j in misscolnameafter1) {
  x1[j]<-min(which(!is.na(cpi[,j])))
}
x1#back na row position in number
#a.test1<-with(cpi, cpi[(time1) >= cpi[x1[1]+12,"time1"] & (time1) <= "2019-12-01", ])# plus12 rows(month) extract time
#a.test1$Trend <- seq_along(a.test1$time1)
#df.after.remove.<-data.frame()
#for (i in 1:length(x1)) {
#  df.after.remove.[i]<-# plus12 rows(month) extract time
  
#}
#xy<-vector("list",20)
for (i in 1:length(x1)) {
  assign(paste0("y",i), with(cpi, cpi[(time1) >= cpi[x1[i]+12,"time1"] & (time1) <= "2019-12-01", ]))    
}
df.list<-lapply(1:length(x1), function(x) eval(parse(text=paste0("y", x)))) #In order to store all datasets in one list using their name
names(df.list)<-lapply(1:length(x1), function(x) paste0("y", x)) #Adding the name of each df in case you want to unlist the list afterwards
#re-creation 
for (i in 1:length(df.list)) {
  df.list[[i]][["Trend"]]<-seq_along(df.list[[i]]$time1)
}
list2env(df.list,.GlobalEnv)
#cpi[a[1],time1]="1993-02-01",cpi[a[1]+12,time1]="1994-02-01",cpi[a[1,time1]] non na rows what times in that col

#a.test<-with(cpi, cpi[(time1) >= cpi[a[1+12],"time1"] & (time1) <= "2019-12-01", ])# plus12 rows(month) extract time
#a.test$Trend <- seq_along(a.test$time1)


# furniture48 -------------------------------------------------------------


#furniture48<-with(cpi, cpi[(time1) >= "1994-02-01" & (time1) <= "2019-12-01", ])
#Trend <- seq_along(furniture48$time1)
# furniture 48 ------------------------------------------------------------
FH05.48<-lm(a.test[,612] ~ `REPAIR OF HOUSEHOLD APPLIANCES2`+
              `REPAIR OF HOUSEHOLD APPLIANCES3`+`REPAIR OF HOUSEHOLD APPLIANCES4`+
              `REPAIR OF HOUSEHOLD APPLIANCES5`+`REPAIR OF HOUSEHOLD APPLIANCES6`+
              `REPAIR OF HOUSEHOLD APPLIANCES7`+`REPAIR OF HOUSEHOLD APPLIANCES8`+
              `REPAIR OF HOUSEHOLD APPLIANCES9`+`REPAIR OF HOUSEHOLD APPLIANCES10`+
              `REPAIR OF HOUSEHOLD APPLIANCES11`+`REPAIR OF HOUSEHOLD APPLIANCES12`+
              `REPAIR OF HOUSEHOLD APPLIANCES13`+`CPI ALL ITEMS2`+`CPI ALL ITEMS3`+`CPI ALL ITEMS4`+
              `CPI ALL ITEMS5`+`CPI ALL ITEMS6`+`CPI ALL ITEMS7`+`CPI ALL ITEMS8`+`CPI ALL ITEMS9`+
              `CPI ALL ITEMS10`+`CPI ALL ITEMS11`+`CPI ALL ITEMS12`+`CPI ALL ITEMS13`+time_Aug+time_Dec+time_Feb+time_Jan+time_Jul+time_Jun
            +time_Mar+time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend,data = a.test)
a.test.result <- lm(
  as.formula(paste('`',colnames(a.test)[612], '`',sep = "","~",
                   paste('`', colnames(a.test)[c(613,614,615,616,617,618,619,620,621,622,623,624,2,3,4,5,6,7,8,9,10,11,12,13,
                                                 2251,2252,2253,2254,2255,2256,2257,2258,2259,2260,2261,2263,2264,2265,2266,2267)], '`', sep = "", collapse = "+")
                   
  )),
  data=a.test
)


repair1 <- lm(
  as.formula(paste('`',colnames(y1)[612], '`',sep = "","~",
                   paste('`', colnames(y1)[c(613,614,615,616,617,618,619,620,621,622,623,624,2,3,4,5,6,7,8,9,10,11,12,13,
                                                 2251,2252,2253,2254,2255,2256,2257,2258,2259,2260,2261,2263,2264,2265,2266,2267)], '`', sep = "", collapse = "+")
                   
  )),
  data=y1
)
#``
# nice stargazer ---------------------------------------------------------
stargazer(repair1,a.test.result,
          star.char = c("*", "**"),
          star.cutoffs = c(0.05, 0.01),
          notes = c(" * p<0.05; ** p<0.01; "), 
          notes.append = FALSE,
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")

# 05 furniture 48 ---------------------------------------------------------



slr.repair1 <-  model.select(repair1,keep = keep.dummies,sig = 0.05,verbose = F)



stargazer(slr.repair1,
          star.char = c("*", "**"),
          star.cutoffs = c(0.05, 0.01),
          notes = c(" * p<0.05; ** p<0.01; "), 
          
          report = "vc*",
          align=TRUE,           
          header = FALSE,
          model.numbers=FALSE,
          
          df=FALSE, 
          digits=2, single.row = TRUE,
          summary=FALSE,
          se = NULL, type = "html")
  # SUR ------------------------------------------------- --------------------
library( "systemfit" )




nederland_sur<-systemfit(COPY[,x] ~ CPI_lag1+CPI_lag2+CPI_lag3+CPI_lag4+CPI_lag5+CPI_lag6+CPI_lag7+CPI_lag8+CPI_lag9+
                           CPI_lag10+CPI_lag11+CPI_lag12+lag1[,x]+lag2[,x]+lag3[,x]+lag4[,x]+lag5[,x]+lag6[,x]+lag7[,x]+
                           lag8[,x]+lag9[,x]+lag10[,x]+lag11[,x]+lag12[,x]+ time_Aug + 
                           time_Dec+ time_Feb+ time_Jan+time_Jul+time_Jun +time_Mar+
                           time_May+time_Nov+time_Oct+time_Sep+VAT1+VAT2+VAT3+Recession+Trend  , method = "SUR")


# Find names of columns which contain missing values ----------------------

# how to add multiple unconsecutive range var -----------------------------


#c(colnames(COPY[1:2]),colnames(COPY[3:4]))
