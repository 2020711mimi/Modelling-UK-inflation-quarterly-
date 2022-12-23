#Packages
library(readxl)
library(data.table)
library(lubridate)#extract cpi time to newdata(furniture48)
library(scales)
library(tidyverse)
library(broom)
library(stargazer)
library(knitr)
library(officer)
library(flextable)
library(magrittr)
library(dplyr)
library(moments)
library(Inflation)
library(ggplot2)
library(zoo)
library(xts)
library(hydroTSM)
library(openair)
library(tibble)
library(miscTools)
library(priceR)
library(Metrics)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(kableExtra)
library(gridExtra)
library(egg)
library(grid)
library(cowplot)
library(spatstat)#shift有重复
library(SciViews)
library(sos)
library(parallel)
library(benchmarkme)
library(predict3d)
library(interactions)
library(aTSA)
library(tseries)
library(forecast)
library(officer)
library(tidyverse)
library(rvg)
library(equatiomatic)
options(scipen = 200)

rm(list = ls())
'%!in%' <- function(x, y)
        ! ('%in%'(x, slack))

# -------------------------------------------------------------------------

df <- fread("all core inflation measures/OVERALL.csv")
sapply(df, class)
df.all <- ts(df[, -1], start = c(2005, 01), frequency = 12)

df <- df[, -1]
nt <- ts(
        start = c(2005, 1),
        end = c(2019, 12) ,
        frequency = 12
)
FE <- ggplot(data = df, aes(x = time(nt))) +
        geom_line(aes(y = SARIMA, color = "SARIMA")) +
        geom_line(aes(y = Headline, color = "Headline")) +
        geom_line(aes(y = AR, color = "AR")) +
        geom_line(aes(y = TM, color = "Trimmed Mean")) +
        geom_line(aes(y = Sticky, color = "Sticky CPI")) +
        geom_line(aes(y = EXFD, color = "EXFD")) +
        labs(x = "Time", y = "Inflation")
plot2 <- FE + theme_bw() + theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
)
plot2

pptx <- read_pptx()

pptx %>%
        add_slide() %>%
        # This first line puts it in as a static png image for comparison
        ph_with(plot2, location = ph_location_type(type = "body")) %>%
        add_slide() %>%
        # This line puts in a shape object, which can be ungrouped and edited
        ph_with(
                rvg::dml(ggobj = plot2),
                width = 8,
                height = 4,
                location = ph_location_type(type = "body")
        )

#print(pptx, "all core inflation measures/ALL.pptx")


# cogley ------------------------------------------------------------------

#left hand side
#
#
#
headline_df <-
        fread("all core inflation measures/series-161222.csv")

CPI_alll <-
        headline_df[, unlist(lapply(.SD, data.table::shift, type = "lead", n = 0:12),
                             recursive = FALSE), .SDcol = 2]

lfs <- head(CPI_alll[, -1],-12) - df$Headline

# 假设数据帧名为df，其中一列名为col1
# 得到数据帧的列名列表
col_names <- names(df)

# 删除col1列的名称
col_names <- col_names[col_names != "Headline"]

# 遍历其余列，计算它们与Headline列的差
for (col_name in col_names) {
        lfs[[paste0(col_name, "_diff")]] <-
                df[["Headline"]] - df[[col_name]]
}


# -------------------------------------------------------------------------

lfs <- data.frame(lfs)
colnames(lfs)[1:12] <- c(1:12)



# -------------------------------------------------------------------------
# 在r中，前12列的每一列作为因变量，分别对后5列的每一列作为自变量，分别进行ols回归，数据帧的名字lfs，仅输出参数结果（intercept and beta），所有列相同的结果合并

#
#

# 创建空列表，用于存储结果
results <- list()

# 循环遍历前 12 列
for (i in 1:12) {
        # 循环遍历后 5 列
        for (j in 13:17) {
                # 运行 OLS 回归
                fit <- lm(lfs[, i] ~ lfs[, j])
                
                # 提取结果
                result <- tidy(fit)
                
                # 将结果添加到列表中
                results[[paste0(names(lfs)[i], " vs ", names(lfs)[j])]] <-
                        result
        }
}

# 将所有结果合并为单个数据帧
result_df <- do.call(rbind, results)

# 分离出截距和系数
intercept <- result_df %>% filter(term == "(Intercept)")
beta <- result_df %>% filter(term != "(Intercept)")

# 显示结果
intercept
beta

# -------------------------------------------------------------------------
intercept <- cbind(intercept, rep(1:5, each = 1))
groups <- split(intercept, intercept$`rep(1:5, each = 1)`)

#write.csv(groups, file = "all core inflation measures/ intercept_groups.csv")

# -------------------------------------------------------------------------
beta <- cbind(beta, rep(1:5, each = 1))
groups.beta <- split(beta, beta$`rep(1:5, each = 1)`)

#write.csv(groups.beta, file = "all core inflation measures/ beta_groups.csv")


# -------------------------------------------------------------------------

estimates <- list()
for (i in 1:5) {
        estimates[[i]] <- groups.beta[[i]]$estimate
}

beta <- as.data.frame(estimates)
names(beta) <- colnames(df)[c(1, 3, 4, 5, 6)]



# beta-------------------------------------------------------------------------



ggplot(data = beta, aes(x = 1:nrow(beta))) +
        geom_point(aes(y = beta[, "AR"], color = "AR", group = "AR")) +
        geom_point(aes(y = beta[, "SARIMA"], color = "SARIMA", group = "SARIMA")) +
        geom_point(aes(y = beta[, "EXFD"], color = "EXFD", group = "EXFD")) +
        geom_point(aes(y = beta[, "TM"], color = "TM", group = "TM")) +
        geom_point(aes(y = beta[, "Sticky"], color = "Sticky", group = "Sticky")) +
        scale_x_continuous(name = "Forecast Horizon", breaks = 1:12) +
        scale_y_continuous(name = "Beta") +
        labs(color = "Core inflation Measures") +
        theme(panel.background = element_blank())

ggsave(
        "all core inflation measures/beta.png",
        width = 8,
        height = 6,
        dpi = 300
)



# RMSE-------------------------------------------------------------------------
#
#

# Create a list to store the RMSE values for each regression
rmse_values <- list()

# Loop through the first 12 columns of the data
for (i in 1:12) {
        # Select the response variable (i-th column of the data)
        y <- lfs[, i]
        
        # Loop through the last 5 columns of the data
        for (j in 13:17) {
                # Select the explanatory variable (j-th column of the data)
                x <- lfs[, j]
                
                # Fit the OLS regression model
                model <- lm(y ~ x)
                
                # Predict the response variable using the model
                y_pred <- predict(model, lfs)
                
                # Calculate the RMSE
                rmse <- sqrt(mean((y - y_pred) ^ 2))
                
                # Store the RMSE in the list
                rmse_values[[paste(i, j)]] <- rmse
        }
}

rmse_values

df.rmse <- data.frame(cbind(rmse_values))
rmse <- (cbind(df.rmse, rep(1:5, each = 1)))
groups.rmse <- split(rmse, rmse$`rep(1:5, each = 1)`)

estimates <- list()
for (i in 1:5) {
        estimates[[i]] <- groups.rmse[[i]]$rmse_values
}
names(estimates) <- colnames(df)[c(1, 3, 4, 5, 6)]
# Extract all elements from the estimates list and convert them to a numeric vector
new_df <- data.frame(t(do.call(rbind, estimates)))



colnames(new_df) <- colnames(df)[c(1, 3, 4, 5, 6)]

#write.csv(groups.rmse, file = "all core inflation measures/ rmse_groups.csv")

# RMSE plot -------------------------------------------------------------------------
new_df <- as.data.frame(lapply(new_df, as.numeric))
#write.csv(new_df, file = "all core inflation measures/ rmse_groups.csv")
library(flextable)
new_df <- round(new_df, 2)

# 在数据框的第一列之前插入一列
new_df <- cbind(1:12, new_df)
colnames(new_df) <-
        c("Months Ahead", "AR", "SARIMA", "EXFD", "TM", "Sticky")

# 将数据框转换为 flextable 对象
ft <- flextable(new_df)

# 设置 flextable 的样式
ft <- theme_zebra(ft)

# 将 flextable 对象输出

doc <- read_docx()
doc <- doc %>% body_add_flextable(ft)
print(doc, target = "all core inflation measures/rmse output.docx")


colnames(new_df)
ggplot(data = new_df, aes(x = 1:nrow(new_df))) +
        geom_point(aes(y = new_df[, "AR"], color = "AR", group = "AR")) +
        geom_point(aes(y = new_df[, "SARIMA"], color = "SARIMA", group = "SARIMA")) +
        geom_point(aes(y = new_df[, "EXFD"], color = "EXFD", group = "EXFD")) +
        geom_point(aes(y = new_df[, "TM"], color = "TM", group = "TM")) +
        geom_point(aes(y = new_df[, "Sticky"], color = "Sticky", group = "Sticky")) +
        scale_x_continuous(name = "Forecast Horizon", breaks = 1:12) +
        scale_y_continuous(name = "RMSE") +
        labs(color = "Core inflation Measures") +
        theme(panel.background = element_blank())

ggsave("plot.png",
       width = 8,
       height = 6,
       dpi = 300)


# r suared ----------------------------------------------------------------
v.df <- lfs
colnames(v.df)[1:ncol(v.df)] <- paste0("V", 1:17)
# Create a data frame with all possible combinations of the 12 variables and the 5 response variables
combinations <- expand.grid(1:12, 13:17)
names(combinations) <- c("x", "y")

# Use lapply to loop through all combinations and fit a linear model for each one
r_squared_values <- lapply(1:nrow(combinations), function(i) {
        model <-
                lm(paste0("V", combinations[i, "y"], " ~ V", combinations[i, "x"]), data = v.df)
        summary(model)$r.squared
})

# Extract the R squared values from the list
r_squared_values <- (sapply(r_squared_values, function(x)
        x[1]))

# Split the data frame into a list of 5 data frames
r_squared_values_split <- split(r_squared_values, rep(1:5, each = 12))

summary(lm(V12 ~ V17, data =  v.df))$r.squared

# plot --------------------------------------------------------------------
r_squared_values_split <- data.frame(r_squared_values_split)
colnames(r_squared_values_split) <- c("AR", "SARIMA", "EXFD", "TM", "Sticky")

ggplot(data = r_squared_values_split, aes(x = 1:nrow(r_squared_values_split))) +
        geom_line(aes(y = r_squared_values_split[, "AR"], color = "AR", group = "AR")) +
        geom_line(aes(y = r_squared_values_split[, "SARIMA"], color = "SARIMA", group = "SARIMA")) +
        geom_line(aes(y = r_squared_values_split[, "EXFD"], color = "EXFD", group = "EXFD")) +
        geom_line(aes(y = r_squared_values_split[, "TM"], color = "TM", group = "TM")) +
        geom_line(aes(y = r_squared_values_split[, "Sticky"], color = "Sticky", group = "Sticky")) +
        scale_x_continuous(name = "Forecast Horizon (Months)", breaks = 1:12) +
        scale_y_continuous(name = "R-Square") +
        labs(color = "Core inflation Measures") +
        theme(panel.background = element_blank())

ggsave("all core inflation measures/ R Square plot.png",
       width = 8,
       height = 6,
       dpi = 300)
