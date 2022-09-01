# source("cpi ppi consumption unemployment/consum unemployment ppi res.R")
# source("LDV.R")
# General ---------------------------------------------------------------------
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

ugly_name <-  c(
  paste0("L(div[, i], ", 1:12, ")"),
  paste0("L(cpi[, 1], ", 1:12, ")"),
  paste0("L(demand[, i], ", 1:12, ")"))
ugly_name

new_name <- c(paste0("LDV", 1:12),
              paste0("CPI-LDV", 1:12),
              paste0("Demand-LDV", 1:12)
              )
new_name

search_for_these <- ugly_name
replace_with_these <- new_name

cm <- setNames(new_name, ugly_name)
cm

new_name1 <- c(paste0("LDV", 1:12),
               paste0("CPI-LDV", 1:12),
               paste0("Demand-LDV", 1:12),"ppi","unemploy")



names(model.div) <- (div.name)

found <- list()
for (i in 1:length(model.div)) {
  found[[i]] <-
    match(names(model.div[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(model.div[[i]]$coefficients)[names(model.div[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}

vars.order <- new_name1

stargazer(model.div,  
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
          out = "cpi ppi consumption unemployment/general 12 lag version P.html",
          type = "text"
)

modelsummary(
  model.div,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "cpi ppi consumption unemployment/general 12 lag version.html"
)
# DIVS AIC-------------------------------------------------------------------------




y.list1 <- ynew_list[[1]]
names(y.list1) <- c(div.name)

found <- list()
for (i in 1:length(y.list1)) {
  found[[i]] <-
    match(names(y.list1[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(y.list1[[i]]$coefficients)[names(y.list1[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}


stargazer(y.list1,  
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
          out = "cpi ppi consumption unemployment/AIC P.html",
          type = "text"
)

modelsummary(
  y.list1,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "cpi ppi consumption unemployment/AIC .html"
)
# stage 1-------------------------------------------------------------------------
names(all_sig_output) <- c(div.name)

found <- list()
for (i in 1:length(all_sig_output)) {
  found[[i]] <-
    match(names(all_sig_output[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output[[i]]$coefficients)[names(all_sig_output[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}

stargazer(all_sig_output,  
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
          out = "cpi ppi consumption unemployment/stage1 p.html",
          type = "text"
)

modelsummary(
  all_sig_output,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "cpi ppi consumption unemployment/stage1 .html"
)

# stage 2 -----------------------------------------------------------------
stage2_name <- div.name[c(1,3:10,12)]
stage2_name
names(all_sig_output_stage2) <- c(div.name[c(1,3:10,12)])

found <- list()
for (i in 1:length(all_sig_output_stage2)) {
  found[[i]] <-
    match(names(all_sig_output_stage2[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output_stage2[[i]]$coefficients)[names(all_sig_output_stage2[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}

stargazer(all_sig_output_stage2,  
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(div.name[c(1,3:10,12)]),
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
          out = "cpi ppi consumption unemployment/stage2 p.html",
          type = "text"
)

modelsummary(
  all_sig_output_stage2,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "cpi ppi consumption unemployment/stage2 .html"
)

# stage 3 -----------------------------------------------------------------
stage3_name <- (stage2_name[c(1,5,9)])
stage3_name
names(all_sig_output_stage3) <- c(stage2_name[c(1,5,9)])

found <- list()
for (i in 1:length(all_sig_output_stage3)) {
  found[[i]] <-
    match(names(all_sig_output_stage3[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output_stage3[[i]]$coefficients)[names(all_sig_output_stage3[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}

stargazer(all_sig_output_stage3,  
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(stage2_name[c(1,5,9)]),
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
          out = "cpi ppi consumption unemployment/stage3 p.html",
          type = "text"
)

modelsummary(
  all_sig_output_stage3,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "cpi ppi consumption unemployment/stage3 .html"
)


# stage 4 -----------------------------------------------------------------
stage4_name <- (stage3_name[c(2,3)])
stage4_name
names(all_sig_output_stage4) <- c(stage3_name[c(2,3)])

found <- list()
for (i in 1:length(all_sig_output_stage4)) {
  found[[i]] <-
    match(names(all_sig_output_stage4[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output_stage4[[i]]$coefficients)[names(all_sig_output_stage4[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}

stargazer(all_sig_output_stage4,  
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(stage3_name[c(2,3)]),
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
          out = "cpi ppi consumption unemployment/stage4 p.html",
          type = "text"
)

modelsummary(
  all_sig_output_stage4,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "cpi ppi consumption unemployment/stage4 .html"
)

# stage 5 -----------------------------------------------------------------
stage5_name <- (stage4_name[c(1)])
stage5_name
names(all_sig_output_stage5) <- c(stage4_name[c(1)])

found <- list()
for (i in 1:length(all_sig_output_stage5)) {
  found[[i]] <-
    match(names(all_sig_output_stage5[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output_stage5[[i]]$coefficients)[names(all_sig_output_stage5[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}

stargazer(all_sig_output_stage5,  
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c(stage4_name[c(1)]),
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
          out = "cpi ppi consumption unemployment/stage5 p.html",
          type = "text"
)

modelsummary(
  all_sig_output_stage5,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  output = "cpi ppi consumption unemployment/stage5 .html"
)
