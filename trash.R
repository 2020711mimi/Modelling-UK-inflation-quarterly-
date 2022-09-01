
# trash -------------------------------------------------------------------


cm <-
  c(
    'L(ts.divs.percent[, i], 1)' = "LDV1",
    'L(ts.divs.percent[, i], 2)' = "LDV2",
    'L(ts.divs.percent[, i], 3)' = "LDV3",
    'L(ts.divs.percent[, i], 4)' = "LDV4",
    'L(ts.divs.percent[, i], 5)' = "LDV5",
    'L(ts.divs.percent[, i], 6)' = "LDV6",
    'L(ts.divs.percent[, i], 7)' = "LDV7",
    'L(ts.divs.percent[, i], 8)' = "LDV8",
    'L(ts.divs.percent[, i], 9)' = "LDV19",
    'L(ts.divs.percent[, i], 10)' = "LDV10",
    'L(ts.divs.percent[, i], 11)' = "LDV11",
    'L(ts.divs.percent[, i], 12)' = "LDV12",
    'L(ts.divs.percent[, 1], 1)' = 'CPI-LDV1',
    'L(ts.divs.percent[, 1], 2)' = 'CPI-LDV2',
    'L(ts.divs.percent[, 1], 3)' = 'CPI-LDV3',
    'L(ts.divs.percent[, 1], 4)' = 'CPI-LDV4',
    'L(ts.divs.percent[, 1], 5)' = 'CPI-LDV5',
    'L(ts.divs.percent[, 1], 6)' = 'CPI-LDV6',
    'L(ts.divs.percent[, 1], 7)' = 'CPI-LDV7',
    'L(ts.divs.percent[, 1], 8)' = 'CPI-LDV8',
    'L(ts.divs.percent[, 1], 9)' = 'CPI-LDV9',
    'L(ts.divs.percent[, 1], 10)' = 'CPI-LDV10',
    'L(ts.divs.percent[, 1], 11)' = 'CPI-LDV11',
    'L(ts.divs.percent[, 1], 12)' = 'CPI-LDV12',
    "VAT1" = "VAT1" ,
    "VAT2" = "VAT2" ,
    "VAT3" = "VAT3" ,
    "Recession" = "Recession",
    "D.August" =  "D.August",
    "D.December" = "D.December",
    "D.February" = "D.February",
    "D.January" = "D.January",
    "D.July" = "D.July",
    "D.June" = "D.June",
    "D.March" = "D.March",
    "D.May" = "D.May",
    "D.November" = "D.November",
    "D.October" = "D.October",
    "D.September" = "D.September",
    '(Intercept)' = 'Constant'
    
    
  )

# -------------------------------------------------------------------------
df <- transform(df, ethnicity=lookup[ethnicode], stringsAsFactors=FALSE)
names(y2$coefficients)
cm
cm[ names(y2$coefficients)]
 transform(names(y2$coefficients)
, ethnicity=cm[oldname], stringsAsFactors=FALSE)

 data.table::setnames (data.frame( names(y2$coefficients)),ugly_name,new_name)
 

# -------------------------------------------------------------------------
model2.div <- model.div
 model2.div[sapply(model2.div, is.null)] <- NULL
 
 lapply(model2.div, function(x)
   names(which(coef(summary(
     x
   ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.05)) %in% LDV.NAME]) 
summary(model2.div[[1]]) 

stargazer(model2.div[[2]],type = "text")
modelsummary(model2.div,stars = TRUE)
