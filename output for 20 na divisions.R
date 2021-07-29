 rm(list = ls())
# this for coefficent name exactly for what i want
# DATA INPUT --------------------------------------------------------------

Sys.setlocale("LC_TIME", "English")
library(readxl)
cpi <- read_excel("cpi data/cpi+division+group.xlsx")

library(data.table)
colnames(cpi) <- gsub("&", "and", colnames(cpi))
# class(cpi$`REPAIR OF HOUSEHOLD APPLIANCES1`)
cpi <- as.data.table(cpi)
COPY <- copy(cpi)
Data_A <- as.data.table(cpi)

#
COPY$time1 <- seq.Date(from = as.Date("1988/01/01", format = "%Y/%m/%d"), by = "month", length.out = 387)
COPY <- with(COPY, COPY[(time1) >= "1993-01-01" & (time1) <= "2019-12-01", ])
COPY <- subset(COPY, select = -c(time1))


#
Varnames <- copy(names(Data_A))
Data_A[, (names(Data_A)) := lapply(.SD, as.numeric)][
  ,
  rn := .I
]
melted <- melt(Data_A, id.vars = "rn")[, (paste0("lag_", 0:12)) := shift(value, 0:12, type = "lag"),
  by = variable
][
  ,
  value := NULL
][]
# melted <- melt(Data_A, id.vars="rn")[,
#                                     (paste("lag", 0:12,"[,x]")) := shift(value, 0:12, type="lag"),
#                                     by=variable][,
#                                                  value:=NULL][]
res <- dcast.data.table(
  melt(melted, id.vars = c("rn", "variable"), variable.name = "lag"),
  rn ~ variable + lag, sum
)






# #cpi lag name 3:14 ------------------------------------------------------


varnames.cpi <- c("CPI_lag")

names(res)[3:14] <- paste0(rep(varnames.cpi, each = 12), 1:12)

# change to lax[,x] -------------------------------------------------------


# varnames.other<-c("lag")
# names(res)[614:625]<- paste0(rep("lag", each=12),1:12,"[,x]")
# gsub('`', "", names(res)[614:625])
#

res$time1 <- seq.Date(from = as.Date("1988/01/01", format = "%Y/%m/%d"), by = "month", length.out = 387)
# res <- with(res, res[(time1) >= "1993-01-01" & (time1) <= "2019-12-01", ])
#

# sapply(reg.model, coef)
res$time <- seq.Date(from = as.Date("1988/01/01", format = "%Y/%m/%d"), by = "month", length.out = 387)
res$time <- format(res$time, format = "%b")

res <- fastDummies::dummy_cols(res, select_columns = "time", remove_most_frequent_dummy = TRUE, )
res$VAT1 <- as.numeric(res$`time1` == "2008-12-01")
# print(cpi$"VAT1")
res$VAT2 <- as.numeric(res$`time1` == "2010-01-01")
res$VAT3 <- as.numeric(res$`time1` == "2011-01-01")
res$Recession <- as.numeric(res$`time1` >= "2008-04-01" & res$`time1` <= "2009-06-01")
# cpi$Trend <- seq_along(cpi$time1)
cpi$VAT1 <- as.numeric(cpi$`time1` == "2008-12-01")
# print(cpi$"VAT1")
cpi$VAT2 <- as.numeric(cpi$`time1` == "2010-01-01")
cpi$VAT3 <- as.numeric(cpi$`time1` == "2011-01-01")
cpi$Recession <- as.numeric(cpi$`time1` >= "2008-04-01" & cpi$`time1` <= "2009-06-01")

misscolname <- colnames(COPY)[colSums(is.na(COPY)) > 0] # find which col has na
misscolnameafter1 <- paste(misscolname, "_", "lag", "_", "0", sep = "") # add 1 after 19 miss value column(lag_1),misscolnameafter1[1]="REPAIR OF HOUSEHOLD APPLIANCES1"
x <- c(1:13)
adssad <- list()
for (i in 1:length(misscolname)) {
  adssad[i] <- as.numeric(which(colnames(res) == misscolnameafter1[i])) # back na division lag_1 column position by number in res dataframe position ,eg:612th cola
  #  a<-which(!is.na(cpi[,612]))#na col which rows start non-na
}
adssad

cpi <- as.data.frame(cpi)
res <- as.data.frame(res)
x1 <- vector("numeric")

for (j in misscolnameafter1) {
  x1[j] <- min(which(!is.na(res[, j])))
}
x1 # back na row position in number

for (i in 1:length(x1)) {
  assign(paste0("y", i), with(res, res[(time1) >= res[x1[i] + 12, "time1"] & (time1) <= "2019-12-01", ]))
}
df.list <- lapply(1:length(x1), function(x) eval(parse(text = paste0("y", x)))) # In order to store all datasets in one list using their name
names(df.list) <- lapply(1:length(x1), function(x) paste0("y", x)) # Adding the name of each df in case you want to unlist the list afterwards
# re-creation trend
for (i in 1:length(df.list)) {
  df.list[[i]][["Trend"]] <- seq_along(df.list[[i]]$time1)
}
list2env(df.list, .GlobalEnv)

# 0.1 ---------------------------------------------------------------------
# #change colname ---------------------------------------------------------
y1 <- as.data.frame(y1)
# names(res)[614:625]<- paste0(rep("lag", each=12),1:12)
# names(y1)[c(614)] <- c('lag1[]')
# no.2: "CPI ALL ITEMS_lag_0",  NO14:"CPI ALL ITEMS_lag_12", NO2252:VAT1, NO2256:time, NO2268:trend
res1 <- lm(
  as.formula(paste("`", colnames(y1)[613], "`",
    sep = "", "~",
    paste("`", colnames(y1)[c(
      614, 615, 616, 617, 618, 619, 620, 621, 622, 623, 624, 625, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
      2253, 2254, 2255, 2256, 2257, 2258, 2259, 2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2268
    )], "`", sep = "", collapse = "+")
  )),
  data = y1
)

# ?????ǲ????????? ----------------------------------------------------------------

# res1.simple <- model.select(res1, keep = keep.dummies, sig = 0.05, verbose = F)
# names(res1$coefficients) #<-
# coefs <- lapply(regression, function(x) names(coefficients(x))[2])
# names(res1$coefficients) <- c(
#  "Constant", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", "lag8", "lag9", "lag10", "lag11", "lag12",
#  "CPI_lag1", "CPI_lag2", "CPI_lag3", "CPI_lag4", "CPI_lag5", "CPI_lag6", "CPI_lag7", "CPI_lag8", "CPI_lag9",
#  "CPI_lag10", "CPI_lag11", "CPI_lag12", "time_Aug",
#  "time_Dec", "time_Feb", "time_Jan", "time_Jul", "time_Jun", "time_Mar",
#  "time_May", "time_Nov", "time_Oct", "time_Sep", "VAT1", "VAT2", "VAT3", "Recession", "Trend"
# )
for (i in 1:173) {
  names(regression[[i]]$coefficients) <- c(
    "Constant", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", "lag8", "lag9", "lag10", "lag11", "lag12",
    "CPI_lag1", "CPI_lag2", "CPI_lag3", "CPI_lag4", "CPI_lag5", "CPI_lag6", "CPI_lag7", "CPI_lag8", "CPI_lag9",
    "CPI_lag10", "CPI_lag11", "CPI_lag12", "time_Aug",
    "time_Dec", "time_Feb", "time_Jan", "time_Jul", "time_Jun", "time_Mar",
    "time_May", "time_Nov", "time_Oct", "time_Sep", "VAT1", "VAT2", "VAT3", "Recession", "Trend"
  )
}
# names(regression[[1]]$coefficients)<-c('Constant','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
#                                       "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4","CPI_lag5","CPI_lag6","CPI_lag7","CPI_lag8","CPI_lag9",
#                                       "CPI_lag10","CPI_lag11","CPI_lag12","time_Aug" ,
#                                       "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar",
#                                       "time_May","time_Nov","time_Oct","time_Sep","VAT1","VAT2","VAT3","Recession","Trend")
# names(regression[[3]]$coefficients)
#
# tr <- createTexreg(coef.names = coefficient.names,
#                   coef = coefficients,
#                   se = numeric(0),
#                   pvalues = significance,
#                   gof.names = gof.names,
#                   gof = gof,
#                   gof.decimal = decimal.places)
# tr <- extract(res1)
# tr@se<- numeric(0)
# new.regression <- list(regression,res)
# new.tr <- extract(new.regression)
# names(res1$coefficients) <- c('Constant','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
#                                    "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4","CPI_lag5","CPI_lag6","CPI_lag7","CPI_lag8","CPI_lag9",
#                                    "CPI_lag10","CPI_lag11","CPI_lag12","time_Aug" ,
##                                    "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar",
#                                    "time_May","time_Nov","time_Oct","time_Sep","VAT1","VAT2","VAT3","Recession","Trend")
# regression<-c(res1)
# screenreg( regression[1],
#           single.row = TRUE, stars = c(0.01, 0.05),
#           custom.coef.names = c('Intercept','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
#                                 "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4","CPI_lag5","CPI_lag6","CPI_lag7","CPI_lag8","CPI_lag9",
###                                 "CPI_lag10","CPI_lag11","CPI_lag12","time_Aug" ,
#                               "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar",
#                               "time_May","time_Nov","time_Oct","time_Sep","VAT1","VAT2","VAT3","Recession","Trend")
#         )
# screenreg(tr)
# extract(regression, include.se=FALSE)


COPY <- as.data.frame(COPY)


# ``---------------------------------------------
# stargazer(regression[41:47],res1,regression[49:53],
#
#          star.char = c("*", "**"),
#          star.cutoffs = c(0.05, 0.01),
#          notes = c(" * p<0.05; ** p<0.01; "),
##          notes.append = FALSE,
#
#          dep.var.labels.include = FALSE,
#          column.labels = c(colnames(COPY[41:47]),"REPAIR OF HOUSEHOLD APPLIANCES",colnames(COPY[49:53])),
#          covariate.labels = c('Intercept','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10",
#                               "lag11","lag12", "CPI.lag1","CPI.lag2","CPI.lag3","CPI.lag4","CPI.lag5","CPI.lag6","CPI.lag7","CPI.lag8","CPI.lag9",
#                               "CPI.lag10","CPI.lag11","CPI.lag12","time.Aug" ,"time.Dec", "time.Feb", "time.Jan","time.Jul","time.Jun" ,"time.Mar","time.May",
#                               "time.Nov","time.Oct","time.Sep","VAT1","VAT2","VAT3","Recession","Trend"),
#
##
#          report = "vc*",
#          align=TRUE,
#          header = FALSE,
#          model.numbers=FALSE,
#
#          df=FALSE,
#          digits=2, single.row = TRUE,
##          summary=FALSE,
#          se = NULL, type = "html")#


# stargazer(res1,regression[1],
#
#          star.char = c("*", "**"),
#          star.cutoffs = c(0.05, 0.01),
# notes = c(" * p<0.05; ** p<0.01; "),
# notes.append = FALSE,
#
# dep.var.labels.include = FALSE,
# column.labels = c(colnames(COPY[41:47]),"REPAIR OF HOUSEHOLD APPLIANCES",colnames(COPY[49:53])),
# covariate.labels = c('Intercept','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10",
#                      "lag11","lag12", "CPI.lag1","CPI.lag2","CPI.lag3","CPI.lag4","CPI.lag5","CPI.lag6","CPI.lag7","CPI.lag8","CPI.lag9",
#                      "CPI.lag10","CPI.lag11","CPI.lag12","time.Aug" ,"time.Dec", "time.Feb", "time.Jan","time.Jul","time.Jun" ,"time.Mar","time.May",
#                      "time.Nov","time.Oct","time.Sep","VAT1","VAT2","VAT3","Recession","Trend"),
#
#
# report = "vc*",
# align=TRUE,
# header = FALSE,
# model.numbers=FALSE,
#
# df=FALSE,
# digits=2, single.row = TRUE,
# summary=FALSE,
# se = NULL, type = "html")

# library(outreg2)
#
# stargazer(regression[41:47],res1,regression[49:53],
#
#           star.char = c("*", "**"),
#           star.cutoffs = c(0.05, 0.01),
#           notes = c(" * p<0.05; ** p<0.01; "),
#           notes.append = FALSE,
#
#           dep.var.labels.include = FALSE,
#           column.labels = c(colnames(COPY[41:47]),"REPAIR OF HOUSEHOLD APPLIANCES",colnames(COPY[49:53])),
#           covariate.labels = c('Intercept','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
#                                "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4","CPI_lag5","CPI_lag6","CPI_lag7","CPI_lag8","CPI_lag9",
#                                "CPI_lag10","CPI_lag11","CPI_lag12","time_Aug" ,
#                                "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar",
#                                "time_May","time_Nov","time_Oct","time_Sep","VAT1","VAT2","VAT3","Recession","Trend"),
#
#
#           report = "vc*",
#           align=TRUE,
#           header = FALSE,
#           model.numbers=FALSE,
#
#           df=FALSE,
#           digits=2, single.row = TRUE,
#           summary=FALSE,
#           se = NULL, type = "html")


# after stepwise ----------------------------------------------------------
library(lubridate) # extract cpi time to newdata(furniture48)
newdata <- with(cpi, cpi[(time1) >= "1993-01-01" & (time1) <= "2019-12-01", ]) # start from 93.1
Trend <- seq_along(newdata$time1)

slr.total <- list()
keep.dummies <- c(
  "time_Aug",
  "time_Dec", "time_Feb", "time_Jan", "time_Jul", "time_Jun", "time_Mar", "time_May", "time_Nov", "time_Oct", "time_Sep",
  "VAT1", "VAT2", "VAT3", "Recession", "Trend"
)
# remember delete
# keep.dummies <- c("time_Aug",
#                  "time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
#                  "VAT1","VAT2","VAT3","Recession","Trend",'Constant','lag1','lag2',"lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12",
#                  "CPI_lag1","CPI_lag2","CPI_lag3","CPI_lag4","CPI_lag5","CPI_lag6","CPI_lag7","CPI_lag8","CPI_lag9",
#                  "CPI_lag10","CPI_lag11","CPI_lag12")

# 0.05 --------------------------------------------------------------------

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

#
search_for_these <- c("lag1[, x]", "lag2[, x]", "lag3[, x]", "lag4[, x]", "lag5[, x]", "lag6[, x]", "lag7[, x]", "lag8[, x]", "lag9[, x]", "lag10[, x]", "lag11[, x]", "lag12[, x]", "VAT1")
replace_with_these <- c("lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", "lag8", "lag9", "lag10", "lag11", "lag12", "VAT1")
for (i in 2:173) {
  found[[i]] <- match(names(regression[[i]]$coefficients), search_for_these, nomatch = 0)
  names(regression[[i]]$coefficients)[names(regression[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
}
for (i in 2:173) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
}

# 1,86,141???в???ȫ????????
# screenreg(list(regression[[1]], regression[[86]], regression[[141]]))
# 1 -----------------------------------------------------------------------
library(stargazer)
stargazer(regression[2:15],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[2:15])),
  summary = FALSE,
  se = NULL, type = "html"
)

stargazer(slr.total[2:15],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[2:15])),
  summary = FALSE,
  

  se = NULL, type = "html"
)

stargazer(slr.total[2:15],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
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
          column.labels = c(colnames(COPY[2:15])),
          summary = FALSE,
          out = "C:/Users/PC/Desktop/output/7.18 reference output/1.html",
          flip = FALSE,
          se = NULL, type = "html"
)

stargazer(slr.total[2:7],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
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
          out = "C:/Users/PC/Desktop/output/7.18 reference output/1-1.html",
          flip = FALSE,
          se = NULL, type = "html"
)

stargazer(slr.total[8:15],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
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
          column.labels = c(colnames(COPY[8:15])),
          summary = FALSE,
          out = "C:/Users/PC/Desktop/output/7.18 reference output/1-2.html",
          flip = FALSE,
          se = NULL, type = "html"
)

# 2 -----------------------------------------------------------------------
stargazer(regression[16:21],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
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
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/2.html",
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


# 3 -----------------------------------------------------------------------
stargazer(regression[22:27],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
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

stargazer(slr.total[22:27],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/3.html",
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

# 4 -----------------------------------------------------------------------
stargazer(regression[28:40],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[28:40])),
  summary = FALSE,
  se = NULL, type = "html"
)

stargazer(slr.total[28:40],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/4.html",
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[28:40])),
  summary = FALSE,
  se = NULL, type = "html"
)

# 5 -----------------------------------------------------------------------


stargazer(regression[41:53],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  notes.append = FALSE,

  dep.var.labels.include = FALSE,
  column.labels = c(colnames(COPY[41:53])),



  report = "vc*",
  align = TRUE,
  header = FALSE,
  model.numbers = FALSE,

  df = FALSE,
  digits = 2, single.row = TRUE,
  summary = FALSE,
  se = NULL, type = "html"
)
stargazer(slr.total[41:53],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/5.html",
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[41:53])),
  summary = FALSE,
  se = NULL, type = "html"
)
# 6 -----------------------------------------------------------------------
stargazer(regression[54:61],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  notes.append = FALSE,

  dep.var.labels.include = FALSE,
  column.labels = c(colnames(COPY[54:61])),



  report = "vc*",
  align = TRUE,
  header = FALSE,
  model.numbers = FALSE,

  df = FALSE,
  digits = 2, single.row = TRUE,
  summary = FALSE,
  se = NULL, type = "html"
)
stargazer(slr.total[54:61],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/6.html",
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[54:61])),
  summary = FALSE,
  se = NULL, type = "html"
)

# 7 -----------------------------------------------------------------------
stargazer(regression[62:76],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  notes.append = FALSE,

  dep.var.labels.include = FALSE,
  column.labels = c(colnames(COPY[62:76])),



  report = "vc*",
  align = TRUE,
  header = FALSE,
  model.numbers = FALSE,

  df = FALSE,
  digits = 2, single.row = TRUE,
  summary = FALSE,
  se = NULL, type = "html"
)
stargazer(slr.total[62:75],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/7.html",
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[62:76])),
  summary = FALSE,
  se = NULL, type = "html"
)

stargazer(slr.total[62:75],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/7.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[62:67]),colnames(COPY[69:76])),
          summary = FALSE,
          se = NULL, type = "html"
)

stargazer(slr.total[62:67],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/7-1.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[62:67])),
          summary = FALSE,
          se = NULL, type = "html"
)

stargazer(slr.total[68:75],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/7-2.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[69:76])),
          summary = FALSE,
          se = NULL, type = "html"
)

# 8 -----------------------------------------------------------------------

stargazer(regression[77:79],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  notes.append = FALSE,

  dep.var.labels.include = FALSE,
  column.labels = c(colnames(COPY[77:79])),



  report = "vc*",
  align = TRUE,
  header = FALSE,
  model.numbers = FALSE,

  df = FALSE,
  digits = 2, single.row = TRUE,
  summary = FALSE,
  se = NULL, type = "html"
)
stargazer(slr.total[77:79],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[77:79])),
  summary = FALSE,
  se = NULL, type = "html"
)

stargazer(slr.total[76:77],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/8.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[77]),colnames(COPY[79])),
          summary = FALSE,
          se = NULL, type = "html"
)
# 9 -----------------------------------------------------------------------
# 86??????
# slr.total <- list()
# for (i in 2:length(regression)) {
#  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
# }
# y10 <- as.data.frame(y10)
# names(res)[614:625]<- paste0(rep("lag", each=12),1:12)
# names(y1)[c(614)] <- c('lag1[]')
# no.2: "CPI ALL ITEMS_lag_0",  NO14:"CPI ALL ITEMS_lag_12", NO2252:VAT1, NO2256:time, NO2268:trend

# 9.since all cpi lag and self lag insignificant, just run without --------


res10 <- lm(
  as.formula(paste("`", colnames(y10)[1107], "`",
    sep = "", "~",
    paste("`", colnames(y1)[c(
      2253, 2254, 2255, 2256, 2257, 2258, 2259, 2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2268
    )], "`", sep = "", collapse = "+")
  )),
  data = y10
)
a <- list(res1, res10)
# res10.simple <- model.select(regression[[86]], keep = keep.dummies, sig = 0.05, verbose = F)
#
names(res10$coefficients) <- c(
  "Constant", "time_Aug",
  "time_Dec", "time_Feb", "time_Jan", "time_Jul", "time_Jun", "time_Mar",
  "time_May", "time_Nov", "time_Oct", "time_Sep", "VAT1", "VAT2", "VAT3", "Recession", "Trend"
)

stargazer(regression[80:101],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  notes.append = FALSE,

  dep.var.labels.include = FALSE,
  column.labels = c(colnames(COPY[80:101])),



  report = "vc*",
  align = TRUE,
  header = FALSE,
  model.numbers = FALSE,

  df = FALSE,
  digits = 2, single.row = TRUE,
  summary = FALSE,
  se = NULL, type = "html"
)
stargazer(slr.total[80:85], res10, slr.total[86:100],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[80:101])),
  summary = FALSE,
  se = NULL, type = "html"
)


stargazer(slr.total[78:95],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/9.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[80:85]),colnames(COPY[89:98]),colnames(COPY[100:101])),
          summary = FALSE,
          se = NULL, type = "html"
)

stargazer(slr.total[78:87],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/9.1.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[80:85]),colnames(COPY[89:92])),
          summary = FALSE,
          se = NULL, type = "html"
)

stargazer(slr.total[88:95],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/9.2.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[93:98]),colnames(COPY[100:101])),
          summary = FALSE,
          se = NULL, type = "html"
)
# 10 ----------------------------------------------------------------------

stargazer(regression[102],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  notes.append = FALSE,

  dep.var.labels.include = FALSE,
  column.labels = c(colnames(COPY[102])),



  report = "vc*",
  align = TRUE,
  header = FALSE,
  model.numbers = FALSE,

  df = FALSE,
  digits = 2, single.row = TRUE,
  summary = FALSE,
  se = NULL, type = "html"
)
stargazer(slr.total[101],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[102])),
  summary = FALSE,
  se = NULL, type = "html"
)

stargazer(slr.total[96],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/10.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[102])),
          summary = FALSE,
          se = NULL, type = "html"
)
# 11 ----------------------------------------------------------------------

stargazer(regression[103:107],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  notes.append = FALSE,

  dep.var.labels.include = FALSE,
  column.labels = c(colnames(COPY[103:107])),



  report = "vc*",
  align = TRUE,
  header = FALSE,
  model.numbers = FALSE,

  df = FALSE,
  digits = 2, single.row = TRUE,
  summary = FALSE,
  se = NULL, type = "html"
)
stargazer(slr.total[102:106],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[103:107])),
  summary = FALSE,
  se = NULL, type = "html"
)

stargazer(slr.total[97:101],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/11.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[103:107])),
          summary = FALSE,
          se = NULL, type = "html"
)
# 12 ----------------------------------------------------------------------



stargazer(regression[108:122],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  notes.append = FALSE,

  dep.var.labels.include = FALSE,
  column.labels = c(colnames(COPY[108:122])),



  report = "vc*",
  align = TRUE,
  header = FALSE,
  model.numbers = FALSE,

  df = FALSE,
  digits = 2, single.row = TRUE,
  summary = FALSE,
  se = NULL, type = "html"
)
stargazer(slr.total[107:121],
  star.char = c("*", "**"),
  star.cutoffs = c(0.05, 0.01),
  notes = c(" * p<0.05; ** p<0.01; "),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2, single.row = TRUE,
  model.numbers = FALSE,
  column.labels = c(colnames(COPY[108:122])),
  summary = FALSE,
  se = NULL, type = "html",
  #out = "C:/Users/PC/Desktop/output/7.18 reference output/1.html"
)


stargazer(slr.total[102:108],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/12.1.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[109:115])),
          summary = FALSE,
          se = NULL, type = "html"
         
)


stargazer(slr.total[109:115],
          star.char = c("*"),
          star.cutoffs = c(0.01),
          notes = c(" * p<0.01; "),
          omit = c("time_Aug","time_Dec", "time_Feb", "time_Jan","time_Jul","time_Jun" ,"time_Mar","time_May","time_Nov","time_Oct","time_Sep",
                   "VAT1","VAT2","VAT3","Recession","Trend",'Constant'),
          out = "C:/Users/PC/Desktop/output/7.18 reference output/12.2.html",
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          # order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
          report = "vc*",
          align = TRUE,
          header = FALSE,
          df = FALSE,
          digits = 2, single.row = TRUE,
          model.numbers = FALSE,
          column.labels = c(colnames(COPY[116:122])),
          summary = FALSE,
          se = NULL, type = "html"

)
























# library(stargazer)
# stargazer(slr.table05.furniture[41:47], res1.simple, slr.table05.furniture[49:53],
#   star.char = c("*", "**"),
#   star.cutoffs = c(0.05, 0.01),
#   notes = c(" * p<0.05; ** p<0.01; "),
#   notes.append = FALSE,
#
#   dep.var.labels.include = FALSE,
#   column.labels = c(colnames(COPY[41:47]), "REPAIR OF HOUSEHOLD APPLIANCES", colnames(COPY[49:53])),
#
#
#   report = "vc*",
#   align = TRUE,
#   header = FALSE,
#   model.numbers = FALSE,
#
#   df = FALSE,
#   digits = 2, single.row = TRUE,
#   summary = FALSE,
#   se = NULL, type = "html"
# )

# 1 2 3 4 5
# 1
# res2 <- lm(
#   as.formula(paste("`", colnames(y1)[730], "`",
#     sep = "", "~",
#     paste("`", colnames(y1)[c(
#       731, 732, 733, 734, 735, 736, 737, 738, 739, 740, 741, 742, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
#       2252, 2253, 2254, 2255, 2257, 2258, 2259, 2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2268
#     )], "`", sep = "", collapse = "+")
#   )),
#   data = y2
# )
# # 2
# res3 <- lm(
#   as.formula(paste("`", colnames(y1)[743], "`",
#     sep = "", "~",
#     paste("`", colnames(y1)[c(
#       744, 745, 746, 747, 748, 749, 750, 751, 752, 753, 754, 755, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
#       2252, 2253, 2254, 2255, 2257, 2258, 2259, 2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2268
#     )], "`", sep = "", collapse = "+")
#   )),
#   data = y3
# )
# # 3
# res4 <- lm(
#   as.formula(paste("`", colnames(y1)[756], "`",
#     sep = "", "~",
#     paste("`", colnames(y1)[c(
#       757, 758, 759, 760, 761, 762, 763, 764, 765, 766, 767, 768, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
#       2252, 2253, 2254, 2255, 2257, 2258, 2259, 2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2268
#     )], "`", sep = "", collapse = "+")
#   )),
#   data = y4
# )
# # 4
# res5 <- lm(
#   as.formula(paste("`", colnames(y1)[769], "`",
#     sep = "", "~",
#     paste("`", colnames(y1)[c(
#       770, 771, 772, 773, 774, 775, 776, 777, 778, 779, 780, 781, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
#       2252, 2253, 2254, 2255, 2257, 2258, 2259, 2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2268
#     )], "`", sep = "", collapse = "+")
#   )),
#   data = y5
# )
# # 5
# res6 <- lm(
#   as.formula(paste("`", colnames(y1)[782], "`",
#     sep = "", "~",
#     paste("`", colnames(y1)[c(
#       783, 784, 785, 786, 787, 788, 789, 790, 791, 792, 793, 794, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
#       2252, 2253, 2254, 2255, 2257, 2258, 2259, 2260, 2261, 2262, 2263, 2264, 2265, 2266, 2267, 2268
#     )], "`", sep = "", collapse = "+")
#   )),
#   data = y6
# )
# stargazer(regression[54:56], res2, res3, res4, res5, res6,
#   star.char = c("*", "**"),
#   star.cutoffs = c(0.05, 0.01),
#   notes = c(" * p<0.05; ** p<0.01; "),
#   notes.append = FALSE,
#
#   dep.var.labels.include = FALSE,
#   column.labels = c(
#     "06HL", "06ME", "06PP",
#     "06OT", " 06OP",
#     "06MP", "06DS", "06HS"
#   ),
#
#
#   report = "vc*",
#   align = TRUE,
#   header = FALSE,
#   model.numbers = FALSE,
#
#   df = FALSE,
#   digits = 2, single.row = TRUE,
#   summary = FALSE,
#   se = NULL, type = "html"
# )
#
# healt.list <- list(res2, res3, res4, res5, res6)
# x2 <- list()
# for (i in 1:5) {
#   x2[[i]] <- model.select(healt.list[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
# }
#
# stargazer(slr.table06.health[54:56], x2[1:5],
#   star.char = c("*", "**"),
#   star.cutoffs = c(0.05, 0.01),
#   notes = c(" * p<0.05; ** p<0.01; "),
#   notes.append = FALSE,
#
#   dep.var.labels.include = FALSE,
#   column.labels = c(
#     "06HL", "06ME", "06PP",
#     "06OT", " 06OP",
#     "06MP", "06DS", "06HS"
#   ),
#
#   report = "vc*",
#   align = TRUE,
#   header = FALSE,
#   model.numbers = FALSE,
#
#   df = FALSE,
#   digits = 2, single.row = TRUE,
#   summary = FALSE,
#   se = NULL, type = "html"
# )

# count significant coefficient -------------------------------------------


# summary(regression[[2:12]])$coefficients[,4]
# summary(regression[[2]])

# which(summary(regression[[2]])[,4]<0.05)
# coef(summary(regression[[2]]))["lag1", 4]
# which(coef(summary(slr.total[[2:122]]))[, 4]<0.05)
# regression[[2]]$coefficients
# lapply(regression, coef()[,4])

# coef(slr.total[[2]])[, 4]



# lapply(slr.total[2:3], function(x) summary(x)$coefficients[,4])
# which(lapply(slr.total[2:3], function(x) summary(x)$coefficients["CPI_lag1",4])<0.05)
# lapply(slr.total[2:3],function(x) summary(x)$coefficients[,4])
# coef(summary(slr.total[[2]]),"lag1",4)
# lapply(test_lst, function(x) intersect(unlist(x), nms))
# lapply(slr.total[2:3],function(x) coef)

library(broom)
# tidy(slr.total[[2]])
#

# cpi lag -----------------------------------------------------------------



a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag1", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag2", 4])
length(which(a < 0.05))
length(which(a < 0.01))


a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag3", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag4", 4])
length(which(a < 0.05))
length(which(a < 0.01))


a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag5", 4])
length(which(a < 0.05))
length(which(a < 0.01))


a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag6", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag7", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag8", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag9", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag10", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag11", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["CPI_lag12", 4])
length(which(a < 0.05))
length(which(a < 0.01))
a <- lapply(regression[2:122], function(x) summary(x)$coefficients["VAT1", 4])
a <- lapply(slr.total[2:122], function(x) summary(x)$coefficients["VAT1", 4])
length(a)
 # ?ٷֺ? ---------------------------------------------------------------------

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
percent(1)

# loop --------------------------------------------------------------------


a=list()
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  print(percent(length(which(a[[i]] < 0.05))/121))
  #print(a[[i]])
}
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  print(percent(length(which(a[[i]] < 0.01))/121))
}

for (i in 1:12) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  print(percent(length(which(a[[i]] < 0.05))/121))
  #print(a[[i]])
}
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  print(percent(length(which(a[[i]] < 0.01))/121))
}
# -------------------------------------------------------------------------


slr.total <- list()
for (i in 2:length(regression)) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}
for (i in 1:3) {
  a[[i]] <- lapply(slr.total[2:122], function(x) summary(x)$coefficients[paste0("VAT",i), 4])
  
  print(percent(length(which(a[[i]] < 0.01))/121))
}

# Weight ------------------------------------------------------------------
weight<- list(0.109,	0.042,	0.063,	0.115,	0.067,	0.022,	0.152,	0.023,	0.152,	0.019,	0.137,	0.099
              
)
Reduce("+",weight)
 
#0.109	+0.042+	0.063	+0.115+	0.067+	0.022+	0.152+	0.023+	0.152+	0.019+	0.137+	0.099

# weight stepwise ---------------------------------------------------------
slr.total <- list()
for (i in 2:length(regression)) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}
search_for_these <- c("lag1[, x]", "lag2[, x]", "lag3[, x]", "lag4[, x]", "lag5[, x]", "lag6[, x]", "lag7[, x]", "lag8[, x]", "lag9[, x]", "lag10[, x]", "lag11[, x]", "lag12[, x]", "VAT1")
replace_with_these <- c("lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", "lag8", "lag9", "lag10", "lag11", "lag12", "VAT1")
for (i in 2:173) {
  found[[i]] <- match(names(regression[[i]]$coefficients), search_for_these, nomatch = 0)
  names(regression[[i]]$coefficients)[names(regression[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
}
for (i in 2:173) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
}
 
#FB01 --------------------------------------------------------------------

FB01.0.05.cpi<-list()
FB01.0.01.cpi<-list()
for (i in 1:12) {
  a[[i]] <- lapply(slr.total[2:15], function(x) summary(x)$coefficients[paste0("CPI_lag",i), 4])
  
  print(length(which(a[[i]] < 0.01)))
 FB01.0.05.cpi[i]<-length(which(a[[i]] < 0.05))*weight[[1]]
  #print(length(which(a[[i]] < 0.01))*weight[[1]])
 FB01.0.01.cpi[i]<-length(which(a[[i]] < 0.01))*weight[[1]]
  #print(percent(length(which(a[[i]] < 0.05))/14))
  #print(percent(length(which(a[[i]] < 0.01))/14))
}
fb01.0.05.lag<-list()
fb01.0.01.lag<-list()
for (i in 1:12) {
  a[[i]] <- lapply(slr.total[2:15], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  
  #print(length(which(a[[i]] < 0.05)))
 FB01.0.05.lag[i]<-length(which(a[[i]] < 0.05))*weight[[1]]
  #print(length(which(a[[i]] < 0.01)))
 FB01.0.01.lag[i]<-length(which(a[[i]] < 0.01))*weight[[1]]
}


# lag-------------------------------------------------------------------------

# -------------------------------------------------------------------------


a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag1", 4])
a <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("lag1"), 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag2", 4])
length(which(a < 0.05))
length(which(a < 0.01))


a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag3", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag4", 4])
length(which(a < 0.05))
length(which(a < 0.01))


a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag5", 4])
length(which(a < 0.05))
length(which(a < 0.01))


a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag6", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag7", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag8", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag9", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag10", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag11", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a <- lapply(regression[2:122], function(x) summary(x)$coefficients["lag12", 4])
length(which(a < 0.05))
length(which(a < 0.01))

a=list()
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  print(length(which(a[[i]] < 0.05)))
  print(percent(length(which(a[[i]] < 0.05))/121))
}
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  print(length(which(a[[i]] < 0.01)))
  print(percent(length(which(a[[i]] < 0.01))/121))
  
}
# stepwise ----------------------------------------------------------------
a=list()
for (i in 1:12) {
  a[[i]] <- lapply(regression[2:122], function(x) summary(x)$coefficients[paste0("lag",i), 4])
  print(length(which(a[[i]] < 0.05)))
  print(percent(length(which(a[[i]] < 0.05))/121))
}



# convert to dataframe ----------------------------------------------------

library(broom)
library(dplyr)
library(texreg)
screenreg(slr.total[122])
screenreg(slr.0.05[2])
library(plyr)
# out = ldply(slr.total, coefficients)
# rownames(out) = sprintf('model%d', 0:2)
# coef <- ldply(slr.total, coef)



# tidy for 0.05--------------------------------------------------------------------
slr.0.05 <- list() # regression ??122
for (i in 2:122) {
  slr.0.05[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F) # 122???û??1??86
}

slr.86 <- model.select(regression[[86]],keep = keep.dummies,sig = 0.01,verbose = F)
 # ?滻??��????
for (i in 2:122) {
  found[[i]] <- match(names(slr.0.05[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.0.05[[i]]$coefficients)[names(slr.0.05[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n

#tidy_mods <- lapply(slr.0.05[2:121], tidy)#why 2:121?????
tidy_mods <- lapply(slr.0.05, tidy)
# add names to each data frame and combine into one big data frame
for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)

length(grep(x = a$term, pattern ="\\bVAT1\\b"))

length(grep(x = a$term, pattern ="CPI_lag1"))
#"cpi_lag1" include "cpi lag12"
length(grep(x = a$term, pattern ="\\bCPI_lag1\\b"))
length(grep(x = a$term, pattern = "CPI_lag2"))
length(grep(x = a$term, pattern ="\\bCPI_lag2\\b"))
length(grep(x = a$term, pattern = "CPI_lag3"))
length(grep(x = a$term, pattern ="\\bCPI_lag3\\b"))

length(grep(x = a$term, pattern = "CPI_lag4"))
length(grep(x = a$term, pattern ="\\bCPI_lag4\\b"))

length(grep(x = a$term, pattern = "CPI_lag5"))
length(grep(x = a$term, pattern ="\\bCPI_lag5\\b"))

length(grep(x = a$term, pattern = "CPI_lag6"))
length(grep(x = a$term, pattern ="\\bCPI_lag6\\b"))

length(grep(x = a$term, pattern = "CPI_lag7"))
length(grep(x = a$term, pattern ="\\bCPI_lag7\\b"))

length(grep(x = a$term, pattern = "CPI_lag8"))
length(grep(x = a$term, pattern ="\\bCPI_lag8\\b"))

length(grep(x = a$term, pattern = "CPI_lag9"))
length(grep(x = a$term, pattern ="\\bCPI_lag9\\b"))

length(grep(x = a$term, pattern = "CPI_lag10"))
length(grep(x = a$term, pattern ="\\bCPI_lag10\\b"))

length(grep(x = a$term, pattern = "CPI_lag11"))
length(grep(x = a$term, pattern ="\\bCPI_lag11\\b"))

length(grep(x = a$term, pattern = "CPI_lag12"))
length(grep(x = a$term, pattern ="\\bCPI_lag12\\b"))



length(grep(x = a$term, pattern = "lag1"))
length(grep(x = a$term, pattern ="\\blag1\\b"))
length(grep(x = a$term, pattern = "lag2"))
length(grep(x = a$term, pattern ="\\blag2\\b"))
length(grep(x = a$term, pattern = "lag3"))
length(grep(x = a$term, pattern ="\\blag3\\b"))
length(grep(x = a$term, pattern = "lag4"))
length(grep(x = a$term, pattern ="\\blag4\\b"))
length(grep(x = a$term, pattern = "lag5"))
length(grep(x = a$term, pattern ="\\blag5\\b"))
length(grep(x = a$term, pattern = "lag6"))
length(grep(x = a$term, pattern ="\\blag6\\b"))
length(grep(x = a$term, pattern = "lag7"))
length(grep(x = a$term, pattern ="\\blag7\\b"))
length(grep(x = a$term, pattern = "lag8"))
length(grep(x = a$term, pattern ="\\blag8\\b"))
length(grep(x = a$term, pattern = "lag9"))
length(grep(x = a$term, pattern ="\\blag9\\b"))
length(grep(x = a$term, pattern = "lag10"))
length(grep(x = a$term, pattern ="\\blag10\\b"))
length(grep(x = a$term, pattern = "lag11"))
length(grep(x = a$term, pattern ="\\blag11\\b"))
length(grep(x = a$term, pattern = "lag12"))
length(grep(x = a$term, pattern ="\\blag12\\b"))
# for 0.01 ----------------------------------------------------------------
slr.0.01 <- list() # regression ??122
for (i in 2:122) {
  slr.0.01[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F) # 122??
}
# ?滻??��????
for (i in 2:122) {
  found[[i]] <- match(names(slr.0.01[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.0.01[[i]]$coefficients)[names(slr.0.01[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 116=n

tidy_mods <- lapply(slr.0.01, tidy)
# add names to each data frame and combine into one big data frame
for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)

length(grep(x = a$term, pattern ="CPI_lag1"))
length(grep(x = a$term, pattern ="\\bCPI_lag1\\b"))
length(grep(x = a$term, pattern = "CPI_lag2"))
length(grep(x = a$term, pattern ="\\bCPI_lag2\\b"))
length(grep(x = a$term, pattern = "CPI_lag3"))
length(grep(x = a$term, pattern ="\\bCPI_lag3\\b"))

length(grep(x = a$term, pattern = "CPI_lag4"))
length(grep(x = a$term, pattern ="\\bCPI_lag4\\b"))

length(grep(x = a$term, pattern = "CPI_lag5"))
length(grep(x = a$term, pattern ="\\bCPI_lag5\\b"))

length(grep(x = a$term, pattern = "CPI_lag6"))
length(grep(x = a$term, pattern ="\\bCPI_lag6\\b"))

length(grep(x = a$term, pattern = "CPI_lag7"))
length(grep(x = a$term, pattern ="\\bCPI_lag7\\b"))

length(grep(x = a$term, pattern = "CPI_lag8"))
length(grep(x = a$term, pattern ="\\bCPI_lag8\\b"))

length(grep(x = a$term, pattern = "CPI_lag9"))
length(grep(x = a$term, pattern ="\\bCPI_lag9\\b"))

length(grep(x = a$term, pattern = "CPI_lag10"))
length(grep(x = a$term, pattern ="\\bCPI_lag10\\b"))

length(grep(x = a$term, pattern = "CPI_lag11"))
length(grep(x = a$term, pattern ="\\bCPI_lag11\\b"))

length(grep(x = a$term, pattern = "CPI_lag12"))
length(grep(x = a$term, pattern ="\\bCPI_lag12\\b"))

length(grep(x = a$term, pattern = "lag1"))
length(grep(x = a$term, pattern ="\\blag1\\b"))
length(grep(x = a$term, pattern = "lag2"))
length(grep(x = a$term, pattern ="\\blag2\\b"))
length(grep(x = a$term, pattern = "lag3"))
length(grep(x = a$term, pattern ="\\blag3\\b"))
length(grep(x = a$term, pattern = "lag4"))
length(grep(x = a$term, pattern ="\\blag4\\b"))
length(grep(x = a$term, pattern = "lag5"))
length(grep(x = a$term, pattern ="\\blag5\\b"))
length(grep(x = a$term, pattern = "lag6"))
length(grep(x = a$term, pattern ="\\blag6\\b"))
length(grep(x = a$term, pattern = "lag7"))
length(grep(x = a$term, pattern ="\\blag7\\b"))
length(grep(x = a$term, pattern = "lag8"))
length(grep(x = a$term, pattern ="\\blag8\\b"))
length(grep(x = a$term, pattern = "lag9"))
length(grep(x = a$term, pattern ="\\blag9\\b"))
length(grep(x = a$term, pattern = "lag10"))
length(grep(x = a$term, pattern ="\\blag10\\b"))
length(grep(x = a$term, pattern = "lag11"))
length(grep(x = a$term, pattern ="\\blag11\\b"))
length(grep(x = a$term, pattern = "lag12"))
length(grep(x = a$term, pattern ="\\blag12\\b"))

length(grep(x = a$term, pattern ="\\bVAT1\\b"))
# 74 lag1
# 43 lag2
# 35 lag3
# for (i in 1:12) {
#   a[i]<-lapply(regression[2:122], function(x) summary(x)$coefficients[paste("lag",i,sep = ""),4])
# }
# a[1]
for (i in 1:12) {
  #print(length(grep(x = a$term, pattern =paste0("\\blag",i,"\\b"))))
  print(percent(length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))/121))
}
for (i in 1:12) {
  print(percent(length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))/121))
  #print(percent(length(which(a[[i]] < 0.05))/121))
}


# 0.05-------------------------------------------------------------------------
slr.total <- list()
for (i in 2:122) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 2:122) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n

#tidy_mods <- lapply(slr.0.05[2:121], tidy)#why 2:121?????
tidy_mods <- lapply(slr.total, tidy)
# add names to each data frame and combine into one big data frame
for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


# print -------------------------------------------------------------------

# ??????????5%???䶼?????? -----------------------------------------------------------



length(grep(x = a$term, pattern ="\\bVAT1\\b"))
length(
  grep(x = a$term, pattern =paste0("\\bVAT",1,"\\b"))
  )
length(
  grep(x = a$term, pattern ="\\blag2\\b")
  )#?Ե?

#length(grep(x = a$term, pattern ="lag9"))
#length(grep(x = a$term, pattern =paste0("lag",9)))
#length(
#  grep(x = a$term, pattern =paste0("\\lag",9,"\\b"))
#  )
length(
  which(a$term == "lag2")
)#a[1828,] = lag9[, x]??????????????
length(
  which(a$term ==paste0("lag",9))
)
#lag9 ??????34?ö????ԣ???????cpi????lag9?? 19?????ǶԵã?
for (i in 1:12) {
  #print(length(grep(x = a$term, pattern =paste0("\\blag",i,"\\b"))))
  print(percent(length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))/121))
}
for (i in 1:12) {
  print(percent(length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))/121))
  #print(percent(length(which(a[[i]] < 0.05))/121))
}

#my.data.frame <- subset(a , p.value < 0.01 | term == "\\blag1\\b")
#length(grep(x = my.data.frame$term, pattern ="\\blag1\\b"))

#help
#my.data.frame <- subset(help , "C" > 0.01 | "A" > 0.5)
#my.data.frame <- help[(help$C >0.5 ) | (help$A < 0.5), ]

#my.data.frame <- a[(a$term =="\\blag1\\b" ) | (a$p.value < 0.05), ]
#my.data.frame <- a[(a$term =="VAT1" ),]
#my.data.frame <- a[(a$term =="CPI_lag1" ),]
#my.data.frame <- a[(a$term =="CPI_lag1" ) & (a$p.value < 0.05), ]

#percent(nrow(my.data.frame)/121)

# stepwise lag for 0.06 ---------------------------------------------------


for (i in 1:12) {

    my.data.frame <- a[(a$term ==paste0("lag",i)) & (a$p.value < 0.05), ]
    print(
      percent(nrow(my.data.frame)/121)
    )
  
  
}


for (i in 1:12) {
  
  my.data.frame <- a[(a$term ==paste0("CPI_lag",i)) & (a$p.value < 0.05), ]
  print(
    percent(nrow(my.data.frame)/121)
  )
  
}
#cpi lag 1
percent(
length(
  which(a$term == "lag12")
)/121
)
percent(
length(
  grep(x = a$term, pattern ="\\blag12\\b")
)/121)#?Ե?
# 0.01 step cpi lag 1-12 --------------------------------------------------

y<-list()
for (i in 1:12) {
  
  my.data.frame <- a[(a$term ==paste0("CPI_lag",i)) & (a$p.value < 0.01), ]
  print(
    percent(nrow(my.data.frame)/121)
  )
  y[i]<-percent(nrow(my.data.frame)/121)
  setwd("C:\\Users\\PC\\Desktop\\output\\7.18 reference output")
  write.table(y,"STEP.0.01.CPIlag.csv",sep=",") 
}

y<-list()
for (i in 1:12) {
  
  my.data.frame <- a[(a$term ==paste0("lag",i)) & (a$p.value < 0.01), ]
  print(
    percent(nrow(my.data.frame)/121)
  )
  y[i]<-percent(nrow(my.data.frame)/121)
  setwd("C:\\Users\\PC\\Desktop\\output\\7.18 reference output")
  write.table(y,"STEP.0.01.lag.csv",sep=",")
}

my.data.frame <- a[(a$term ==paste0("CPI_lag",1)) & (a$p.value < 0.05), ]
percent(nrow(my.data.frame)/121)
 #length(grep(x = my.data.frame$term, pattern ="\\bVAT1\\b"))
#library(dplyr)
#my.data.frame <- filter(a, p.value < 0.5 & term == "\\blag1\\b")
#percent(length(grep(x = my.data.frame$term, pattern ="\\blag1\\b"))/121) 

#  stepwise ---------------------------------------------------------

# ?鿴120??Trend??λ?? ------------------------------------------------------------
y<-which(a$term=="Trend")
write.table(y,"Trend.csv",sep=",")

# ??һ ----------------------------------------------------------------------
d1 <- a[1:305,]
length(which(d1$term=="Trend"))
which(d1$term=="Trend")

d1lag0.01<-list()
d1cpi0.01<-list()
d1lag0.05<-list()
d1cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d1[(d1$term ==paste0("lag",i)) & (d1$p.value < 0.01), ]
  d1lag0.01[i] <- nrow(my.data.frame)*weight[[1]]
  
  my.data.frame <- d1[(d1$term ==paste0("CPI_lag",i)) & (d1$p.value < 0.01), ]
  d1cpi0.01[i] <- nrow(my.data.frame)*weight[[1]]
  
  my.data.frame <- d1[(d1$term ==paste0("lag",i)) & (d1$p.value < 0.05), ]
  d1lag0.05[i] <- nrow(my.data.frame)*weight[[1]]
  
  my.data.frame <- d1[(d1$term ==paste0("CPI_lag",i)) & (d1$p.value < 0.05), ]
  d1cpi0.05[i] <- nrow(my.data.frame)*weight[[1]]


    }
#print(
#  nrow(my.data.frame)*weight[[1]]
#)

# ?? -----------------------------------------------------------------------
d2 <- a[306:439,]
length(which(d2$term=="Trend"))
d2lag0.01<-list()
d2cpi0.01<-list()
d2lag0.05<-list()
d2cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d2[(d2$term ==paste0("lag",i)) & (d2$p.value < 0.01), ]
  d2lag0.01[i] <- nrow(my.data.frame)*weight[[2]]
  
  my.data.frame <- d2[(d2$term ==paste0("CPI_lag",i)) & (d2$p.value < 0.01), ]
  d2cpi0.01[i] <- nrow(my.data.frame)*weight[[2]]
  
  my.data.frame <- d2[(d2$term ==paste0("lag",i)) & (d2$p.value < 0.05), ]
  d2lag0.05[i] <- nrow(my.data.frame)*weight[[2]]
  
  my.data.frame <- d2[(d2$term ==paste0("CPI_lag",i)) & (d2$p.value < 0.05), ]
  d2cpi0.05[i] <- nrow(my.data.frame)*weight[[2]]
  
  
}

# ?? -----------------------------------------------------------------------
d3 <- a[440:575,]
length(which(d3$term=="Trend"))
d3lag0.01<-list()
d3cpi0.01<-list()
d3lag0.05<-list()
d3cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d3[(d3$term ==paste0("lag",i)) & (d3$p.value < 0.01), ]
  d3lag0.01[i] <- nrow(my.data.frame)*weight[[3]]
  
  my.data.frame <- d3[(d3$term ==paste0("CPI_lag",i)) & (d3$p.value < 0.01), ]
  d3cpi0.01[i] <- nrow(my.data.frame)*weight[[3]]
  
  my.data.frame <- d3[(d3$term ==paste0("lag",i)) & (d3$p.value < 0.05), ]
  d3lag0.05[i] <- nrow(my.data.frame)*weight[[3]]
  
  my.data.frame <- d3[(d3$term ==paste0("CPI_lag",i)) & (d3$p.value < 0.05), ]
  d3cpi0.05[i] <- nrow(my.data.frame)*weight[[3]]
  
  
}

# ?? -----------------------------------------------------------------------
d4 <- a[576:831,]
length(which(d4$term=="Trend"))
d4lag0.01<-list()
d4cpi0.01<-list()
d4lag0.05<-list()
d4cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d4[(d4$term ==paste0("lag",i)) & (d4$p.value < 0.01), ]
  d4lag0.01[i] <- nrow(my.data.frame)*weight[[4]]
  
  my.data.frame <- d4[(d4$term ==paste0("CPI_lag",i)) & (d4$p.value < 0.01), ]
  d4cpi0.01[i] <- nrow(my.data.frame)*weight[[4]]
  
  my.data.frame <- d4[(d4$term ==paste0("lag",i)) & (d4$p.value < 0.05), ]
  d4lag0.05[i] <- nrow(my.data.frame)*weight[[4]]
  
  my.data.frame <- d4[(d4$term ==paste0("CPI_lag",i)) & (d4$p.value < 0.05), ]
  d4cpi0.05[i] <- nrow(my.data.frame)*weight[[4]]
  
  
}

# ?? -----------------------------------------------------------------------
d5 <- a[832:1126,]
length(which(d5$term=="Trend"))
d5lag0.01<-list()
d5cpi0.01<-list()
d5lag0.05<-list()
d5cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d5[(d5$term ==paste0("lag",i)) & (d5$p.value < 0.01), ]
  d5lag0.01[i] <- nrow(my.data.frame)*weight[[5]]
  
  my.data.frame <- d5[(d5$term ==paste0("CPI_lag",i)) & (d5$p.value < 0.01), ]
  d5cpi0.01[i] <- nrow(my.data.frame)*weight[[5]]
  
  my.data.frame <- d5[(d5$term ==paste0("lag",i)) & (d5$p.value < 0.05), ]
  d5lag0.05[i] <- nrow(my.data.frame)*weight[[5]]
  
  my.data.frame <- d5[(d5$term ==paste0("CPI_lag",i)) & (d5$p.value < 0.05), ]
  d5cpi0.05[i] <- nrow(my.data.frame)*weight[[5]]
  
  
}

# ?? -----------------------------------------------------------------------
d6 <- a[1127:1298,]
length(which(d6$term=="Trend"))
d6lag0.01<-list()
d6cpi0.01<-list()
d6lag0.05<-list()
d6cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d6[(d6$term ==paste0("lag",i)) & (d6$p.value < 0.01), ]
  d6lag0.01[i] <- nrow(my.data.frame)*weight[[6]]
  
  my.data.frame <- d6[(d6$term ==paste0("CPI_lag",i)) & (d6$p.value < 0.01), ]
  d6cpi0.01[i] <- nrow(my.data.frame)*weight[[6]]
  
  my.data.frame <- d6[(d6$term ==paste0("lag",i)) & (d6$p.value < 0.05), ]
  d6lag0.05[i] <- nrow(my.data.frame)*weight[[6]]
  
  my.data.frame <- d6[(d6$term ==paste0("CPI_lag",i)) & (d6$p.value < 0.05), ]
  d6cpi0.05[i] <- nrow(my.data.frame)*weight[[6]]
  
  
}

# ?? -----------------------------------------------------------------------
d7 <- a[1299:1640,]
length(which(d7$term=="Trend"))
d7lag0.01<-list()
d7cpi0.01<-list()
d7lag0.05<-list()
d7cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d7[(d7$term ==paste0("lag",i)) & (d7$p.value < 0.01), ]
  d7lag0.01[i] <- nrow(my.data.frame)*weight[[7]]
  
  my.data.frame <- d7[(d7$term ==paste0("CPI_lag",i)) & (d7$p.value < 0.01), ]
  d7cpi0.01[i] <- nrow(my.data.frame)*weight[[7]]
  
  my.data.frame <- d7[(d7$term ==paste0("lag",i)) & (d7$p.value < 0.05), ]
  d7lag0.05[i] <- nrow(my.data.frame)*weight[[7]]
  
  my.data.frame <- d7[(d7$term ==paste0("CPI_lag",i)) & (d7$p.value < 0.05), ]
  d7cpi0.05[i] <- nrow(my.data.frame)*weight[[7]]
  

}

# ?? -----------------------------------------------------------------------
d8 <- a[1641:1696,]
length(which(d8$term=="Trend"))
d8lag0.01<-list()
d8cpi0.01<-list()
d8lag0.05<-list()
d8cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d8[(d8$term ==paste0("lag",i)) & (d8$p.value < 0.01), ]
  d8lag0.01[i] <- nrow(my.data.frame)*weight[[8]]
  
  my.data.frame <- d8[(d8$term ==paste0("CPI_lag",i)) & (d8$p.value < 0.01), ]
  d8cpi0.01[i] <- nrow(my.data.frame)*weight[[8]]
  
  my.data.frame <- d8[(d8$term ==paste0("lag",i)) & (d8$p.value < 0.05), ]
  d8lag0.05[i] <- nrow(my.data.frame)*weight[[8]]
  
  my.data.frame <- d8[(d8$term ==paste0("CPI_lag",i)) & (d8$p.value < 0.05), ]
  d8cpi0.05[i] <- nrow(my.data.frame)*weight[[8]]
  
  
}

# ?? -----------------------------------------------------------------------
d9 <- a[1697:2140,]
length(which(d9$term=="Trend"))
d9lag0.01<-list()
d9cpi0.01<-list()
d9lag0.05<-list()
d9cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d9[(d9$term ==paste0("lag",i)) & (d9$p.value < 0.01), ]
  d9lag0.01[i] <- nrow(my.data.frame)*weight[[9]]
  
  my.data.frame <- d9[(d9$term ==paste0("CPI_lag",i)) & (d9$p.value < 0.01), ]
  d9cpi0.01[i] <- nrow(my.data.frame)*weight[[9]]
  
  my.data.frame <- d9[(d9$term ==paste0("lag",i)) & (d9$p.value < 0.05), ]
  d9lag0.05[i] <- nrow(my.data.frame)*weight[[9]]
  
  my.data.frame <- d9[(d9$term ==paste0("CPI_lag",i)) & (d9$p.value < 0.05), ]
  d9cpi0.05[i] <- nrow(my.data.frame)*weight[[9]]
  
  
}

# 10 ----------------------------------------------------------------------
d10 <- a[2141:2159,]
length(which(d10$term=="Trend"))
d10lag0.01<-list()
d10cpi0.01<-list()
d10lag0.05<-list()
d10cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d10[(d10$term ==paste0("lag",i)) & (d10$p.value < 0.01), ]
  d10lag0.01[i] <- nrow(my.data.frame)*weight[[10]]
  
  my.data.frame <- d10[(d10$term ==paste0("CPI_lag",i)) & (d10$p.value < 0.01), ]
  d10cpi0.01[i] <- nrow(my.data.frame)*weight[[10]]
  
  my.data.frame <- d10[(d10$term ==paste0("lag",i)) & (d10$p.value < 0.05), ]
  d10lag0.05[i] <- nrow(my.data.frame)*weight[[10]]
  
  my.data.frame <- d10[(d10$term ==paste0("CPI_lag",i)) & (d10$p.value < 0.05), ]
  d10cpi0.05[i] <- nrow(my.data.frame)*weight[[10]]
  
  
}

# 11 ---------------------------------------------------------------------
d11 <- a[2160:2260,]
length(which(d11$term=="Trend"))
d11lag0.01<-list()
d11cpi0.01<-list()
d11lag0.05<-list()
d11cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d11[(d11$term ==paste0("lag",i)) & (d11$p.value < 0.01), ]
  d11lag0.01[i] <- nrow(my.data.frame)*weight[[11]]
  
  my.data.frame <- d11[(d11$term ==paste0("CPI_lag",i)) & (d11$p.value < 0.01), ]
  d11cpi0.01[i] <- nrow(my.data.frame)*weight[[11]]
  
  my.data.frame <- d11[(d11$term ==paste0("lag",i)) & (d11$p.value < 0.05), ]
  d11lag0.05[i] <- nrow(my.data.frame)*weight[[11]]
  
  my.data.frame <- d11[(d11$term ==paste0("CPI_lag",i)) & (d11$p.value < 0.05), ]
  d11cpi0.05[i] <- nrow(my.data.frame)*weight[[11]]
  
  
}

# 12 ----------------------------------------------------------------------
d12 <- a[2261:2571,]
length(which(d12$term=="Trend"))
d12lag0.01<-list()
d12cpi0.01<-list()
d12lag0.05<-list()
d12cpi0.05<-list()
for (i in 1:12) {
  
  my.data.frame <- d12[(d12$term ==paste0("lag",i)) & (d12$p.value < 0.01), ]
  d12lag0.01[i] <- nrow(my.data.frame)*weight[[12]]
  
  my.data.frame <- d12[(d12$term ==paste0("CPI_lag",i)) & (d12$p.value < 0.01), ]
  d12cpi0.01[i] <- nrow(my.data.frame)*weight[[12]]
  
  my.data.frame <- d12[(d12$term ==paste0("lag",i)) & (d12$p.value < 0.05), ]
  d12lag0.05[i] <- nrow(my.data.frame)*weight[[12]]
  
  my.data.frame <- d12[(d12$term ==paste0("CPI_lag",i)) & (d12$p.value < 0.05), ]
  d12cpi0.05[i] <- nrow(my.data.frame)*weight[[12]]
  
  
}

# sum and divide ----------------------------------------------------------

# add them up and divide by 121 -------------------------------------------
y<-percent(
  mapply(function(x1, y1,a,b1,c,d,e,f,h,i,j,k) x1[[1]]+y1[[1]]+a[[1]]+b1[[1]]+c[[1]]+d[[1]]+e[[1]]+f[[1]]+
           h[[1]]+i[[1]]+j[[1]]+k[[1]],
         d12lag0.01, d11lag0.01, d10lag0.01, d9lag0.01, d8lag0.01, d7lag0.01, d6lag0.01, d5lag0.01,
         d4lag0.01,d3lag0.01,d2lag0.01, d1lag0.01)/121)
setwd("C:\\Users\\PC\\Desktop\\output\\7.18 reference output")
write.table(y,"w.0.01.lag.csv",sep=",")
y<-
  percent(
    mapply(function(x1, y1,a,b1,c,d,e,f,h,i,j,k) x1[[1]]+y1[[1]]+a[[1]]+b1[[1]]+c[[1]]+d[[1]]+e[[1]]+f[[1]]+
             h[[1]]+i[[1]]+j[[1]]+k[[1]],
           d12lag0.05, d11lag0.05, d10lag0.05, d9lag0.05, d8lag0.05, d7lag0.05, d6lag0.05, d5lag0.05,
           d4lag0.05,d3lag0.05,d2lag0.05, d1lag0.05)/121)
setwd("C:\\Users\\PC\\Desktop\\output\\7.18 reference output")
write.table(y,"w.0.05.lag.csv",sep=",")

y<-
  percent(
    mapply(function(x1, y1,a,b1,c,d,e,f,h,i,j,k) x1[[1]]+y1[[1]]+a[[1]]+b1[[1]]+c[[1]]+d[[1]]+e[[1]]+f[[1]]+
             h[[1]]+i[[1]]+j[[1]]+k[[1]],
           d12cpi0.05, d11cpi0.05, d10cpi0.05, d9cpi0.05, d8cpi0.05, d7cpi0.05, d6cpi0.05, d5cpi0.05,
           d4cpi0.05,d3cpi0.05,d2cpi0.05, d1cpi0.05)/121)
setwd("C:\\Users\\PC\\Desktop\\output\\7.18 reference output")
write.table(y,"w.0.05.cpi.csv",sep=",")
y<-
  percent(
    mapply(function(x1, y1,a,b1,c,d,e,f,h,i,j,k) x1[[1]]+y1[[1]]+a[[1]]+b1[[1]]+c[[1]]+d[[1]]+e[[1]]+f[[1]]+
             h[[1]]+i[[1]]+j[[1]]+k[[1]],
           d12cpi0.01, d11cpi0.01, d10cpi0.01, d9cpi0.01, d8cpi0.01, d7cpi0.01, d6cpi0.01, d5cpi0.01,
           d4cpi0.01,d3cpi0.01,d2cpi0.01, d1cpi0.01)/121)
setwd("C:\\Users\\PC\\Desktop\\output\\7.18 reference output")
write.table(y,"w.0.01.cpi.csv",sep=",")
# 0.01 --------------------------------------------------------------------

# 0.05-------------------------------------------------------------------------
slr.total <- list()
for (i in 2:122) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 2:122) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n

#tidy_mods <- lapply(slr.0.05[2:121], tidy)#why 2:121?????
tidy_mods <- lapply(slr.total, tidy)
# add names to each data frame and combine into one big data frame
for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)

# print -------------------------------------------------------------------


length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  #print(length(grep(x = a$term, pattern =paste0("\\blag",i,"\\b"))))
  print(percent(length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))/121))
}
for (i in 1:12) {
  print(percent(length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))/121))
  #print(percent(length(which(a[[i]] < 0.05))/121))
}

# w. stepwise -----------------------------------------------------------------

#FB01 --------------------------------------------------------------------





#slr.total <- list()
#for (i in 2:12) {
#  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
#}


#for (i in 2:12) {
#  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
#  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
#} # 121=n

#tidy_mods <- lapply(slr.0.05[2:121], tidy)#why 2:121?????
tidy_mods <- lapply(slr.total, tidy)
# add names to each data frame and combine into one big data frame
for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



fb01.0.01.cpi<-list()

fb01.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  #print(length(grep(x = a$term, pattern =paste0("\\blag",i,"\\b"))))
  print(
    (length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b"))))*weight[[1]]
        )

   FB01.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[1]]
}
for (i in 1:12) {
  print(
    length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[1]]
    )
 FB01.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[1]]
}



slr.total <- list()
for (i in 2:12) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 2:12) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n

#tidy_mods <- lapply(slr.0.05[2:121], tidy)#why 2:121?????
tidy_mods <- lapply(slr.total, tidy)
# add names to each data frame and combine into one big data frame
for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


fb01.0.05.cpi<-list()

fb01.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  #print(length(grep(x = a$term, pattern =paste0("\\blag",i,"\\b"))))
  print(
    (length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b"))))*weight[[1]]
  )
  
 FB01.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[1]]
}
for (i in 1:12) {
  print(
    length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[1]]
  )
 FB01.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[1]]
}


# at02 --------------------------------------------------------------------





slr.total <- list()
for (i in 16:21) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 16:21) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



AT02.0.01.cpi<-list()

AT02.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  #print(length(grep(x = a$term, pattern =paste0("\\blag",i,"\\b"))))

  AT02.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[2]]
}
for (i in 1:12) {
 
  AT02.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[2]]
}



slr.total <- list()
for (i in 16:21) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 16:21) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


AT02.0.05.cpi<-list()

AT02.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {


  
  AT02.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[2]]
}
for (i in 1:12) {

  AT02.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[2]]
}

# cf03 --------------------------------------------------------------------

slr.total <- list()
for (i in 22:27) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 22:27) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



CF03.0.01.cpi<-list()

CF03.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {

  
  CF03.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[3]]
}
for (i in 1:12) {
  
  CF03.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[3]]
}



slr.total <- list()
for (i in 22:27) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 22:27) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


CF03.0.05.cpi<-list()

CF03.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  CF03.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[3]]
}
for (i in 1:12) {
  
  CF03.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[3]]
}


# HW04 --------------------------------------------------------------------


slr.total <- list()
for (i in 28:40) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 28:40) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



HW04.0.01.cpi<-list()

HW04.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  HW04.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[4]]
}
for (i in 1:12) {
  
  HW04.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[4]]
}



slr.total <- list()
for (i in 28:40) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 28:40) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


HW04.0.05.cpi<-list()

HW04.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  HW04.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[4]]
}
for (i in 1:12) {
  
  HW04.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[4]]
}


# FH05 --------------------------------------------------------------------
slr.total <- list()
for (i in 41:53) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 41:53) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



FH05.0.01.cpi<-list()

FH05.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  FH05.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[5]]
}
for (i in 1:12) {
  
  FH05.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[5]]
}



slr.total <- list()
for (i in 41:53) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 41:53) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


FH05.0.05.cpi<-list()

FH05.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  FH05.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[5]]
}
for (i in 1:12) {
  
  FH05.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[5]]
}

# HL06 --------------------------------------------------------------------

slr.total <- list()
for (i in 54:61) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 54:61) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



HL06.0.01.cpi<-list()

HL06.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  HL06.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[6]]
}
for (i in 1:12) {
  
  HL06.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[6]]
}



slr.total <- list()
for (i in 54:61) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 54:61) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


HL06.0.05.cpi<-list()

HL06.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  HL06.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[6]]
}
for (i in 1:12) {
  
  HL06.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[6]]
}

# TR07 --------------------------------------------------------------------?е????? 68û??0.01
slr.total <- list()
for (i in 62:76) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 62:76) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



TR07.0.01.cpi<-list()

TR07.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  TR07.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[7]]
}
for (i in 1:12) {
  
  TR07.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[7]]
}



slr.total <- list()
for (i in 62:76) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 62:76) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


TR07.0.05.cpi<-list()

TR07.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  TR07.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[7]]
}
for (i in 1:12) {
  
  TR07.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[7]]
}

# CM08 --------------------------------------------------------------------
slr.total <- list()
for (i in 77:79) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 77:79) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



CM08.0.01.cpi<-list()

CM08.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  CM08.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[8]]
}
for (i in 1:12) {
  
  CM08.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[8]]
}



slr.total <- list()
for (i in 77:79) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 77:79) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


CM08.0.05.cpi<-list()

CM08.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  CM08.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[8]]
}
for (i in 1:12) {
  
  CM08.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[8]]
}


# RC09 --------------------------------------------------------------------
slr.total <- list()
for (i in 80:101) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 80:101) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



RC09.0.01.cpi<-list()

RC09.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  RC09.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[9]]
}
for (i in 1:12) {
  
  RC09.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[9]]
}



slr.total <- list()
for (i in 80:101) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 80:101) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


RC09.0.05.cpi<-list()

RC09.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  RC09.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[9]]
}
for (i in 1:12) {
  
  RC09.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[9]]
}


# ED10 --------------------------------------------------------------------
slr.total <- list()
for (i in 54:61) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 54:61) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



HL06.0.01.cpi<-list()

HL06.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  HL06.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[6]]
}
for (i in 1:12) {
  
  HL06.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[6]]
}



slr.total <- list()
for (i in 54:61) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 54:61) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


HL06.0.05.cpi<-list()

HL06.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  HL06.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[6]]
}
for (i in 1:12) {
  
  HL06.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[6]]
}


# RH11 --------------------------------------------------------------------
slr.total <- list()
for (i in 54:61) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 54:61) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



HL06.0.01.cpi<-list()

HL06.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  HL06.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[6]]
}
for (i in 1:12) {
  
  HL06.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[6]]
}



slr.total <- list()
for (i in 54:61) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 54:61) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


HL06.0.05.cpi<-list()

HL06.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  HL06.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[6]]
}
for (i in 1:12) {
  
  HL06.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[6]]
}


# MS12 --------------------------------------------------------------------
slr.total <- list()
for (i in 54:61) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.01, verbose = F)
}


for (i in 54:61) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)



HL06.0.01.cpi<-list()

HL06.0.01.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  HL06.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[6]]
}
for (i in 1:12) {
  
  HL06.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[6]]
}



slr.total <- list()
for (i in 54:61) {
  slr.total[[i]] <- model.select(regression[[i]], keep = keep.dummies, sig = 0.05, verbose = F)
}


for (i in 54:61) {
  found[[i]] <- match(names(slr.total[[i]]$coefficients), search_for_these, nomatch = 0)
  names(slr.total[[i]]$coefficients)[names(slr.total[[i]]$coefficients) %in% search_for_these] <- replace_with_these[found[[i]]]
} # 121=n


tidy_mods <- lapply(slr.total, tidy)

for (i in 1:length(tidy_mods)) tidy_mods[[i]]$mod <- names(tidy_mods[i])
a <- do.call(rbind.data.frame, tidy_mods)


HL06.0.05.cpi<-list()

HL06.0.05.lag<-list()
length(grep(x = a$term, pattern ="\\bVAT1\\b"))
for (i in 1:12) {
  
  
  
  HL06.0.01.lag[i]<-length(grep(x = a$term,  pattern =paste0("\\blag",i,"\\b")))*weight[[6]]
}
for (i in 1:12) {
  
  HL06.0.01.cpi[i]<-length(grep(x = a$term,  pattern =paste0("\\bCPI_lag",i,"\\b")))*weight[[6]]
}




