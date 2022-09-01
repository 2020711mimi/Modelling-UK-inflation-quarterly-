LDV <- c(
  "Intercept",
  "LDV1",
  "LDV2",
  "LDV3",
  "LDV4",
  "LDV5",
  "LDV6",
  "LDV7",
  "LDV8",
  "LDV9",
  "LDV10",
  "LDV11",
  "LDV12",
  "CPI-LDV1",
  " CPI-LDV2",
  " CPI-LDV3",
  " CPI-LDV4",
  " CPI-LDV5",
  " CPI-LDV6",
  " CPI-LDV7",
  "CPI-LDV8",
  " CPI-LDV9",
  "CPI-LDV10",
  "CPI-LDV11",
  "CPI-LDV12",
  "VAT1",
  "VAT2",
  "VAT3",
  "Recession"   ,
  "D.August"         ,
  "D.December"    ,
  "D.February",
  "D.January",
  "D.July" ,
  "D.June",
  "D.March"  ,
  "D.May"    ,
  "D.November"         ,
  "D.October"   ,
  "D.September"      ,
  "Trend"
)

ugly_name <-  c(
  paste0("L(ts.divs.percent[, i], ", 1:12, ")"),
  paste0("L(ts.divs.percent[, 1], ", 1:12, ")")
)
ugly_name

new_name <- c(paste0("LDV", 1:12),
              paste0("CPI-LDV", 1:12))

search_for_these <- ugly_name
replace_with_these <- new_name

div.name <-
  c("01FB",
    "02AT",
    "03CF",
    "04HW",
    "05FH",
    "06HL",
    "07TR",
    "08CM",
    "09RC",
    "10ED" ,
    "11RH",
    "12MS")

div.name_8 <- c("02AT",	"03CF",	"05FH",	"06HL",	"08CM",	"09RC",	"10ED",	"11RH"
)
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