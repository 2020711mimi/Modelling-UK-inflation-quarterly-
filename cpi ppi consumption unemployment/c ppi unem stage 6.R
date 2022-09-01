all_sig_output_stage5[sapply(all_sig_output_stage5, is.null)] <- NULL

insiginifcant.coef_stage_6 <-
  lapply(all_sig_output_stage5, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% LDV.NAME])

insiginifcant.coef_stage_6

insig_divs_stage5 <- which(lapply(insiginifcant.coef_stage_6, FUN = length ) != 0   )

insig_divs_stage5
