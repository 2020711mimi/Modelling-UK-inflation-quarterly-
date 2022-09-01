all_sig_output_stage4[sapply(all_sig_output_stage4, is.null)] <- NULL

insiginifcant.coef_stage_5 <-
  lapply(all_sig_output_stage4, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV.NAME])

insiginifcant.coef_stage_5

insig_divs_stage5 <- which(lapply(insiginifcant.coef_stage_5, FUN = length ) != 0   )

insig_divs_stage5

for (i in insig_divs_stage5) {
  print(  linearHypothesis(all_sig_output_stage4[[i]],insiginifcant.coef_stage_5[[i]]) )
}


# -------------------------------------------------------------------------


coef_without_insigni_stage5 <- list()
for (i in 1:length(all_sig_output_stage4)) {
  coef_without_insigni_stage5[[i]] <-
    print(names(all_sig_output_stage4[[i]][[1]])[names(all_sig_output_stage4[[i]][[1]]) %in% LDV.NAME &
                                                   names(all_sig_output_stage4[[i]][[1]]) %!in% insiginifcant.coef_stage_5[[i]]])
}
coef_without_insigni_stage5

var_stage5 <- lapply(coef_without_insigni_stage5, function(x)
  c(x, "VAT1", "VAT2",
    "VAT3",
    "Recession",
    "D.",
    "Trend"))


var_stage5


all_sig_formula_stage5 <- lapply(var_stage5, function(x)
  as.formula(paste(
    "div[, i] ~",
    paste(x, collapse = "+")
    
  )))

all__formula_stage5 <- list()
all__formula_stage5[insig_divs_stage5] <- all_sig_formula_stage5[insig_divs_stage5]
all__formula_stage5


all_sig_output_stage5 <- list()

for (i in insig_divs_stage5) {
  all_sig_output_stage5[[i]] <-
    dynlm(all__formula_stage5[[i]],
          start = c(1998, 03),
          end = c(2019, 12))
  
}
