all_sig_output_stage3[sapply(all_sig_output_stage3, is.null)] <- NULL

insiginifcant.coef_stage_4 <-
  lapply(all_sig_output_stage3, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV.NAME])

insiginifcant.coef_stage_4

insig_divs_stage4 <- which(lapply(insiginifcant.coef_stage_4, FUN = length ) != 0   )

insig_divs_stage4

for (i in insig_divs_stage4) {
  print(  linearHypothesis(all_sig_output_stage3[[i]],insiginifcant.coef_stage_4[[i]]) )
}

#divs 5 and 9 still

# -------------------------------------------------------------------------


coef_without_insigni_stage4 <- list()
for (i in 1:length(all_sig_output_stage3)) {
  coef_without_insigni_stage4[[i]] <-
    print(names(all_sig_output_stage3[[i]][[1]])[names(all_sig_output_stage3[[i]][[1]]) %in% LDV.NAME &
                                                   names(all_sig_output_stage3[[i]][[1]]) %!in% insiginifcant.coef_stage_4[[i]]])
}
coef_without_insigni_stage4

var_stage4 <- lapply(coef_without_insigni_stage4, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage4


all_sig_formula_stage4 <- lapply(var_stage4, function(x)
  as.formula(paste(
    "div[, i] ~",
    paste(x, collapse = "+")
    
  )))

all__formula_stage4 <- list()
all__formula_stage4[insig_divs_stage4] <- all_sig_formula_stage4[insig_divs_stage4]
all__formula_stage4


all_sig_output_stage4 <- list()

for (i in insig_divs_stage4) {
  all_sig_output_stage4[[i]] <-
    dynlm(all__formula_stage4[[i]],
          start = c(1998, 03),
          end = c(2019, 12))
  
}
