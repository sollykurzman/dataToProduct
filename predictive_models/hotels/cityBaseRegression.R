library(dplyr)
library(lubridate)

data_path <- "../../data/hotelData.csv"
models_dir <- "saved_models" # folder for the trained models

# create the directory for models if it doesn't exist
if (!dir.exists(models_dir)) {
  dir.create(models_dir)
}

df <- read.csv(data_path)

df_clean <- df %>%
  mutate(
    Date = as.Date(Date),
    Month = as.factor(month(Date)),
    Weekday = as.factor(wday(Date))  
  )

cities <- unique(df_clean$Location) # filter unique cities

# creating df for sacing models performance data
model_performance <- data.frame(
  City = character(),
  MAE = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

for (city in cities) {
  
  # A. Filter data for the specific city
  city_data <- df_clean %>% 
    filter(Location == city)
  
  model <- lm(Average ~ Month + Weekday, data = city_data)
  
  predictions <- predict(model, city_data)
  actuals <- city_data$Average
  errors <- actuals - predictions
  
  mae <- mean(abs(errors))
  rmse <- sqrt(mean(errors^2))
  
  # saving model
  save_path <- file.path(models_dir, paste0("model_", city, ".rds"))
  saveRDS(model, save_path)
  
  model_performance <- rbind(model_performance, data.frame(
    City = city,
    MAE = round(mae, 2),
    RMSE = round(rmse, 2)
  ))
}


print("Training complete! Models saved in 'saved_models/' folder.")
print("Model Performance Summary:")
print(model_performance)

# Optional: Write the performance metrics to a CSV for your report
write.csv(model_performance, "model_performance_metrics.csv", row.names = FALSE)