# source("cpi ppi consumption unemployment/consum unemployment ppi res.R")
# source("LDV.R")
# CPI ---------------------------------------------------------------------

ugly_name <-  c(
  paste0("L(div[, i], ", 1:12, ")"),
  paste0("L(cpi[, 1], ", 1:12, ")"),
  paste0("L(demand[, i], ", 1:12, ")"),
  paste0("L(consum[, 1], ", 1:12, ")")
)
ugly_name

new_name <- c(paste0("LDV", 1:12),
              paste0("CPI-LDV", 1:12),
              paste0("Demand-LDV", 1:12),
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

cpi_aic <- list(reg.cpi,step_CPI)

found <- list()
for (i in 1:length(cpi_aic)) {
  found[[i]] <-
    match(names(cpi_aic[[i]]$coefficients),
          search_for_these,
          nomatch = 0)
  names(cpi_aic[[i]]$coefficients)[names(cpi_aic[[i]]$coefficients) %in% search_for_these] <-
    replace_with_these[found[[i]]]
}
vars.order <- new_name1


stargazer(cpi_aic,
          order = paste0("^", vars.order , "$"),
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          column.labels = c("CPI","CPI(AIC stepwise"),
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
names(cpi_aic) <- c("CPI","CPI(AIC stepwise)")

modelsummary(
  cpi_aic,
  coef_map = new_name1,
  statistic = NULL,
  stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
  fmt = "%.2f",
  coef_omit = "Intercept",
  #output = "general 12 lag version.html"
)
