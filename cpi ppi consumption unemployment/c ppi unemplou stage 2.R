#source("cpi ppi consumption unemployment/consum unemployment ppi res.R")



# stage 2 -----------------------------------------------------------------


insiginifcant.coef_stage_2 <-
  lapply(all_sig_output, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV.NAME])

insiginifcant.coef_stage_2

insig_divs_stage2 <- which(lapply(insiginifcant.coef_stage_2, FUN = length ) != 0   )

# f test ------------------------------------------------------------------
#div 2 and 11 all sig
#insig_divs_stage2 <- c(1,3:10,12)
insig_divs_stage2

lapply(insiginifcant.coef_stage_2[[]],rlang::is_empty)




for (i in insig_divs_stage2) {
print(  linearHypothesis(all_sig_output[[i]],insiginifcant.coef_stage_2[[i]]) )
}



coef_without_insigni_stage2 <- list()
for (i in 1:length(all_sig_output)) {
  coef_without_insigni_stage2[[i]] <-
    print(names(all_sig_output[[i]][[1]])[names(all_sig_output[[i]][[1]]) %in% LDV.NAME &
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
    "div[, i] ~",
    paste(x, collapse = "+")
    
  )))



all__formula_stage2 <- list()
all__formula_stage2[insig_divs_stage2] <- all_sig_formula_stage2[insig_divs_stage2]
all__formula_stage2


all_sig_output_stage2 <- list()

for (i in insig_divs_stage2) {
  all_sig_output_stage2[[i]] <-
    dynlm(all__formula_stage2[[i]],
          start = c(1998, 03),
          end = c(2019, 12))
  
}
