library(dplyr)
library(lubridate)

file_path <- "../../data/hotelData.csv"

df <- read.csv(file_path)

# engineering the features aka feature engineering
# encoding categories to number representation
df <- df %>%
  mutate(
    Date = as.Date(Date),
    City = as.factor(Location),   
    Month = as.factor(month(Date)),
    Weekday = as.factor(wday(Date))
  )

# training the reg model
model <- lm(Average ~ City + Month + Weekday, data = df)

# calculate errors pred vs real
predictions <- predict(model, df)
actuals <- df$Average
errors <- actuals - predictions

# calc error metrics
mae <- mean(abs(errors))
rmse <- sqrt(mean(errors^2))

print(paste("Global Model MAE: €", round(mae, 2)))
print(paste("Global Model RMSE: €", round(rmse, 2)))s


print("Coefficients for Cities (Relative to base city):")
print(summary(model)$coefficients[grep("City", rownames(summary(model)$coefficients)), ])