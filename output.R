 source("stage 1.R")
 source("stage2.R")
 source("stage 3.R")



# 1)	The general 12 lag version (for the appendix). -----------------------
model.div
found <- list()
for (i in 1:length(model.div)) {
  found[[i]] <-
    match(names(model.div[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(model.div[[i]]$coefficients)[names(model.div[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
vars.order <- new_name
model.div[sapply(model.div, is.null)] <- NULL

stargazer(
  model.div,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  column.labels = c(div.name),
  report = "vc*p",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se = NULL,
  #out = "general 12 lag version P.html",
  type = "text"
)


names(model.div) <- c(div.name)
modelsummary(
  model.div,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "general 12 lag version.html"
)


# 2)	The AIC minimised version. (stage 1)  --------------------------------
y.list1 <- y.list[[1]]
found <- list()
for (i in 1:length(y.list1)) {
  found[[i]] <-
    match(names(y.list1[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(y.list1[[i]]$coefficients)[names(y.list1[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}



vars.order <- new_name
y.list1[sapply(y.list1, is.null)] <- NULL

stargazer(
  y.list1,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  column.labels = c(div.name),
  report = "vc*p",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se = NULL,
  out = "table/The AIC minimised version P.html",
  type = "text"
)

# same line p value-------------------------------------------------------------------------

stargazer(
  y.list1,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  column.labels = c(div.name),
  report = "vc*p",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se = NULL,
  out = "The AIC minimised version P.html",
  type = "text"
)

y.list2 <- y.list[[1]]
names(y.list2) <- c(div.name)
modelsummary(
  y.list2,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = cm,
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "The AIC minimised version.html"
)


# 3)	The  regressions when you have removed the insignificant variables using F tests --------
all_sig_output
all_sig_output11 <- all_sig_output
found <- list()
for (i in 1:length(all_sig_output11)) {
  found[[i]] <-
    match(names(all_sig_output11[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output11[[i]]$coefficients)[names(all_sig_output11[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}



vars.order <- new_name
all_sig_output11[sapply(all_sig_output11, is.null)] <- NULL

stargazer(
  all_sig_output11,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  column.labels = c(div.name_8),
  report = "vc*p",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se = NULL,
  out = "table/The  regressions when you have removed the insignificant variables using F tests P.html",
  type = "text"
)
all_sig_output22 <- all_sig_output
names(all_sig_output22) <- c(div.name_8)
modelsummary(
  all_sig_output22,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = cm,
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "The  regressions when you have removed the insignificant variables using F tests.html"
)


# 4)	AIC stage 3. ----------------------

all_sig_output_stage2
all_sig_output11_stage2 <- all_sig_output_stage2
found <- list()
for (i in 1:length(all_sig_output11_stage2)) {
  found[[i]] <-
    match(names(all_sig_output11_stage2[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output11_stage2[[i]]$coefficients)[names(all_sig_output11_stage2[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}



vars.order <- new_name
all_sig_output11_stage2[sapply(all_sig_output11_stage2, is.null)] <-
  NULL

stargazer(
  all_sig_output11_stage2,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  column.labels = c(div.name_8),
  report = "vc*p",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se = NULL,
  out = "table/stage 2 using F tests P.html",
  type = "text"
)
all_sig_output_stage2_2 <- stage_2_ver_1
all_sig_output_stage2_2[sapply(all_sig_output_stage2_2, is.null)] <-
  NULL

names(all_sig_output_stage2_2) <- c(div.name_8)

modelsummary(
  all_sig_output_stage2_2,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = cm,
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "stage 2 using F tests.html"
)

# 4)	Final 12 divs. ----------------------

# What you need to do is to provide a Table (to be put in the appendix) in which you take each of the 7 equations with and without the variables, and at the bottom for each equation to state the Rbarsquared, AIC and in the “new” equations the F-test statistic for the restrictions. We need to see if the estimated coefficients have changed.
#
final.list <-
  c(y.list1[1],
    all_sig_output11_stage2[1:2],
    y.list1[4],
    all_sig_output11_stage2[3:4],
    y.list1[7],
    all_sig_output11_stage2[5:8],
    y.list1[12]
  )
stargazer(
  final.list,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  column.labels = c(div.name),
  report = "vcs*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se=lapply(final.list, function(x) summary(x)$coef[,4]),
  out = "table/F12 divs P.html",
  type = "text"
)


names(final.list) <- c(div.name)
modelsummary(
  final.list,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  coef_map = new_name,
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "F 12 DIVS .html"
)

# 4)	A table of the F tests used to get from 2 to 3. ----------------------



found <- list()
for (i in 1:length(y_sig_divs_list)) {
  found[[i]] <-
    match(names(y_sig_divs_list[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(y_sig_divs_list[[i]]$coefficients)[names(y_sig_divs_list[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}

for (i in 1:8) {
  assign(paste0("change",i), c(y_sig_divs_list[i],all_sig_output11[i],all_sig_output11_stage2[i]))
}

vars.order <- new_name


for (i in 1:8) {
  stargazer(
    mget(paste0("change",i)),
    order = paste0("^", vars.order , "$"),
    dep.var.labels.include = FALSE,
    notes.append = FALSE,
    report = "vc*p",
    align = TRUE,
    header = FALSE,
    df = FALSE,
    digits = 2,
    single.row = TRUE,
    model.numbers = FALSE,
    summary = FALSE,
    flip = FALSE,
    se = NULL,
    out =c(paste("change",div.name_8[i],".html")),
    type = "text"
  )
  
}

for (i in 1:8) {
  modelsummary(
    get(paste0("change",i)),
    statistic = NULL,
    stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
    coef_map = new_name,
    fmt = "%.2f",
    coef_omit = "Intercept",
    output = c(paste("change(no P)",div.name_8[i],".html")),
  )
  
  
}

#
for (i in insig_divs) {
  print  (linearHypothesis(ynew_list[[1]][[i]], new.insignificant[[i]]) )
}

#stage 2 for 08cm
linearHypothesis(all_sig_output[[5]], "L(ts.divs.percent[, 1], 5)")
summary(all_sig_output[[5]])

#stage 2 for 03cf

linearHypothesis(all_sig_output[[2]], "L(ts.divs.percent[, 1], 1)")
summary(all_sig_output[[2]])

#06hl----
#I would suggest that we set CPI-LAG7=CPI-LAG12=0 which passes the F test.

summary(y7
)

linearHypothesis(y7,c("L(ts.divs.percent[, 1], 7)","L(ts.divs.percent[, 1], 12)"))

stage2_06 <- dynlm(formula = ts.divs.percent[, 7] ~ L(ts.divs.percent[, 7], 
                                         1) + L(ts.divs.percent[, 7], 2) + L(ts.divs.percent[, 7], 
                                                                             3) + L(ts.divs.percent[, 7], 4) + L(ts.divs.percent[, 7], 
                                                                                                                 5) + L(ts.divs.percent[, 7], 6) + L(ts.divs.percent[, 7], 
                                                                                                                                                     7) + L(ts.divs.percent[, 7], 11) + L(ts.divs.percent[, 7], 
                                                                                                                                                                                          12) + L(ts.divs.percent[, 1], 
                                                                                                                                                                                                                               8)  + VAT1 + VAT2 + VAT3 + Recession + 
        D. + Trend, start = c(1993, 1), end = c(2019, 12))
summary(stage2_06)



linearHypothesis(stage2_06,c("L(ts.divs.percent[, 7], 7)","L(ts.divs.percent[, 7], 12)"))

stage3_06 <- dynlm(formula = ts.divs.percent[, 7] ~ L(ts.divs.percent[, 7], 
                                                      1) + L(ts.divs.percent[, 7], 2) + L(ts.divs.percent[, 7], 
                                                                                          3) + L(ts.divs.percent[, 7], 4) + L(ts.divs.percent[, 7],                                                                                                                       5) + L(ts.divs.percent[, 7], 6) + L(ts.divs.percent[, 7], 11)  + L(ts.divs.percent[, 1], 
                                                                                                                                                                                                               8)  + VAT1 + VAT2 + VAT3 + Recession + 
                     D. + Trend, start = c(1993, 1), end = c(2019, 12))

summary(stage3_06)

change_for_06hl <- list()
change_for_06hl <- list(y7,stage2_06,stage3_06)

found <- list()
search_for_these
search_for_these1 <- c(paste0("L(ts.divs.percent[, 7], ",1:12,")"),
                       paste0("L(ts.divs.percent[, 1],", 1:12,")")
                       )
for (i in 1:length(change_for_06hl)) {
  found[[i]] <-
    match(names(change_for_06hl[[i]]$coefficients),
          search_for_these1,
          nomatch = 0)
  names(change_for_06hl[[i]]$coefficients)[names(change_for_06hl[[i]]$coefficients) %in% search_for_these1] <-
    replace_with_these[found[[i]]]
}

stargazer(
  change_for_06hl,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  column.labels = c(div.name_8),
  report = "vc*p",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se = NULL,
  out = "06hl.ver1.html",
  type = "text"
)

modelsummary(
  change_for_06hl,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "06hl ver1 no p.html"
)

