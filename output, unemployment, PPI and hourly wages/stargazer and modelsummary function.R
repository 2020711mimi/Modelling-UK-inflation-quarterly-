starp <- function(x) stargazer(x,
                              #order = paste0("^", vars.order , "$"),
                              dep.var.labels.include = FALSE,
                              notes.append = FALSE,
                              #column.labels = c("CPI","CPI(AIC stepwise"),
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

ms <- function(x) modelsummary(x,  
                               #coef_map = new_name1,
                               statistic = NULL,
                               stars = c("*" =  0.1, '**' =   0.05, '***' =   0.01),
                               fmt = "%.2f",
                               coef_omit = "Intercept",
                               #output = "general 12 lag version.html"
)


star <- function(x) stargazer(x,
                              #order = paste0("^", vars.order , "$"),
                              dep.var.labels.include = FALSE,
                              notes.append = FALSE,
                              #column.labels = c("CPI","CPI(AIC stepwise"),
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
                              #out = "general 12 lag version P.html",
                              type = "text"
)

'%!in%' <- function(x, y)
  ! ('%in%'(x, y))
