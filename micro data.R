rm(list = ls())
library(haven)
micro_data <- read_dta("C:/Users/44869/Desktop/db_prices.dta")
head(micro_data)
tail(micro_data)
i210101 <- which(micro_data$item_id == 210101)

df210101<-micro_data[i21]