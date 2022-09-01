all_sig_output_stage2[sapply(all_sig_output_stage2, is.null)] <- NULL

insiginifcant.coef_stage_3 <-
  lapply(all_sig_output_stage2, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV.NAME])

insiginifcant.coef_stage_3
#divs 1,5,9 still need to continue

insig_divs_stage3 <- which(lapply(insiginifcant.coef_stage_3, FUN = length ) != 0   )

insig_divs_stage3

# f test ------------------------------------------------------------------
#div 1,5,9, still insignificant

for (i in insig_divs_stage3) {
  print(  linearHypothesis(all_sig_output_stage2[[i]],insiginifcant.coef_stage_3[[i]]) )
}


# -------------------------------------------------------------------------


coef_without_insigni_stage3 <- list()
for (i in 1:length(all_sig_output_stage2)) {
  coef_without_insigni_stage3[[i]] <-
    print(names(all_sig_output_stage2[[i]][[1]])[names(all_sig_output_stage2[[i]][[1]]) %in% LDV.NAME &
                                            names(all_sig_output_stage2[[i]][[1]]) %!in% insiginifcant.coef_stage_3[[i]]])
}
coef_without_insigni_stage3

var_stage3 <- lapply(coef_without_insigni_stage3, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage3


all_sig_formula_stage3 <- lapply(var_stage3, function(x)
  as.formula(paste(
    "div[, i] ~",
    paste(x, collapse = "+")
    
  )))

all__formula_stage3 <- list()
all__formula_stage3[insig_divs_stage3] <- all_sig_formula_stage3[insig_divs_stage3]
all__formula_stage3


all_sig_output_stage3 <- list()

for (i in insig_divs_stage3) {
  all_sig_output_stage3[[i]] <-
    dynlm(all__formula_stage3[[i]],
          start = c(1998, 03),
          end = c(2019, 12))
  
}
