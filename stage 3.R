all_sig_output_stage2[sapply(all_sig_output_stage2, is.null)] <- NULL

insiginifcant.coef_stage_3 <-
  lapply(all_sig_output_stage2, function(x)
    names(which(coef(summary(   x    ))[, 'Pr(>|t|)'] > 0.1))[names(which(coef(summary(x))[, 'Pr(>|t|)'] > 0.1)) %in% new_and_old])

insiginifcant.coef_stage_3

#z