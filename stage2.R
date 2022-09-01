all_sig_output <- list()

for (i in insig_inde) {
  all_sig_output[[i]] <-
    dynlm(all__formula[[i]],
          start = c(1993, 1),
          end = c(2019, 12))
  
}

all_sig_output[sapply(all_sig_output, is.null)] <- NULL


new_name
new_and_old <- c(LDV.NAME,new_name)

insiginifcant.coef_stage_2 <-
  lapply(all_sig_output, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% new_and_old])

insiginifcant.coef_stage_2

#f test
#03cf for CPI-LDV1 P=0.1218
linearHypothesis(all_sig_output[[2]],insiginifcant.coef_stage_2[[2]])

#08CM FOR CPI-LDV5 P=0.1029
linearHypothesis(all_sig_output[[5]],insiginifcant.coef_stage_2[[5]])


coef_without_insigni_stage2 <- list()
for (i in 1:length(all_sig_output)) {
  coef_without_insigni_stage2[[i]] <-
    print(names(all_sig_output[[i]][[1]])[names(all_sig_output[[i]][[1]]) %in% ugly_name &
                                             names(all_sig_output[[i]][[1]]) %!in% insiginifcant.coef_stage_2[[i]]])
}
coef_without_insigni_stage2



var_stage2 <- lapply(coef_without_insigni_stage2, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage2


all_sig_formula_stage2 <- lapply(var_stage2, function(x)
  as.formula(paste(
    "ts.divs.percent[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage2 <- list()
all__formula_stage2[c(insig_inde)] <- all_sig_formula_stage2
all__formula_stage2


all_sig_output_stage2 <- list()

for (i in insig_inde) {
  all_sig_output_stage2[[i]] <-
    dynlm(all__formula_stage2[[i]],
          start = c(1993, 1),
          end = c(2019, 12))
  
}

stage_2_ver_1 <- all_sig_output_stage2

found <- list()
for (i in 1:length(all_sig_output_stage2)) {
  found[[i]] <-
    match(names(all_sig_output_stage2[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(all_sig_output_stage2[[i]]$coefficients)[names(all_sig_output_stage2[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
vars.order <- new_name
all_sig_output_stage2[sapply(all_sig_output_stage2, is.null)] <- NULL

stargazer(
  all_sig_output_stage2,
  order = paste0("^", vars.order , "$"),
  dep.var.labels.include = FALSE,
  notes.append = FALSE,
  column.labels = c("02AT",	"03CF",	"05FH",	"06HL",	"08CM",	"09RC",	"10ED",	"11RH"
),
  report = "vc*",
  align = TRUE,
  header = FALSE,
  df = FALSE,
  digits = 2,
  single.row = TRUE,
  model.numbers = FALSE,
  summary = FALSE,
  flip = FALSE,
  se = NULL,
  type = "text"
)

