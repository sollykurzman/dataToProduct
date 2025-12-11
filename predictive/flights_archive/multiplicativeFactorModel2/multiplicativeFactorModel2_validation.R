library(tidyverse)
library(lubridate)

# load the prediction function
# make sure predict_flight.R is in the same folder!
source("multiplicativeFactorModel2_utils.R")

print("loading validation data...")
val_data <- read_csv("../../data/flightDataValidation.csv")

# copy paste clean function again
val_data$Date <- ymd(val_data$Date)
val_data$Scraped <- ymd(val_data$Scraped)
val_data$Price <- as.numeric(str_remove(str_remove(val_data$Price, "£"), ","))

# make a place to store predictions
# doing a loop because its easier to debug
preds <- numeric(nrow(val_data))

print("predicting... this might take a sec")

for(i in 1:nrow(val_data)) {
  
  # get the row
  row <- val_data[i, ]
  
  # run the function we made
  p <- get_flight_prediction(
    origin = row$From,
    dest = row$To,
    date = row$Date,
    scraped_date = row$Scraped
  )
  
  # save it
  preds[i] <- p
  
  # print every 100 so i know its working
  if (i %% 100 == 0) print(paste("done", i))
}

# add predictions to the table
val_data$predicted_price <- preds

# remove NAs if any failed
val_data <- val_data %>% filter(!is.na(predicted_price))

# CALC METRICS
# 1. MAE (Mean Absolute Error)
# abs means absolute value (removes negatives)
diffs <- abs(val_data$predicted_price - val_data$Price)
mae <- mean(diffs)

# 2. RMSE (Root Mean Square Error)
# square the error, mean it, then square root
squared_diffs <- (val_data$predicted_price - val_data$Price) ^ 2
rmse <- sqrt(mean(squared_diffs))

print("---------------------------------")
print(paste("MAE: £", round(mae, 2)))
print(paste("RMSE: £", round(rmse, 2)))
print("---------------------------------")

# save the results to look at later
write_csv(val_data, "validation_results.csv")