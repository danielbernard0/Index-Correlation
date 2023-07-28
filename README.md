library(tidyverse)
library(readxl)
library(zoo)

vix_data <- read_csv("C:/Users/danie/OneDrive/Desktop/RGNCM/VIX_History (1).csv")
spy_data <- read_csv("C:/Users/danie/OneDrive/Desktop/RGNCM/SPY.csv")

vix_data$Date <- as.Date(vix_data$DATE, format = "%Y-%m-%d")
spy_data$Date <- as.Date(spy_data$Date, format = "%Y-%m-%d")

end_date <- max(vix_data$Date)  
start_date <- end_date - 9      

vix_subset <- vix_data %>%
  filter(Date >= start_date & Date <= end_date)

spy_subset <- spy_data %>%
  filter(Date >= start_date & Date <= end_date)

common_dates <- intersect(vix_data$Date, spy_data$Date)

vix_subset <- vix_data[vix_data$Date %in% common_dates, ]
spy_subset <- spy_data[spy_data$Date %in% common_dates, ]

vix_subset <- tail(vix_subset, 10)
spy_subset <- tail(spy_subset, 10)


vix_subset <- vix_subset %>%
  mutate(across(everything(), as.numeric))

spy_subset <- spy_subset %>%
  mutate(across(everything(), as.numeric))


correlation <- cor(vix_subset[, -1], spy_subset[, -1])  

print(correlation)

rolling_correlation <- rollapply(
  data = cbind(vix_subset[, -1], spy_subset[, -1]),  
  width = 10,
  FUN = function(x) cor(x[, 1], x[, 2]),
  align = "right",
  fill = NA,
  by.column = FALSE
)




correlation <- cor(vix_subset[, -1], spy_subset[, -1])  


write.csv(correlation, file = "correlation.csv", row.names = FALSE)


rolling_correlation <- rollapply(
  data = cbind(vix_subset[, -1], spy_subset[, -1]),  
  width = 10,
  FUN = function(x) cor(x[, 1], x[, 2]),
  align = "right",
  fill = NA,
  by.column = FALSE
)


write.csv(rolling_correlation, file = "rolling_correlation.csv", row.names = FALSE)

