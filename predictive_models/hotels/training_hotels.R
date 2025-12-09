# get libraries SOlly
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(readr)) install.packages("readr")

library(dplyr)
library(lubridate)
library(readr)

# load data
df <- read_csv("data/hotelData.csv", show_col_types = FALSE)

# cleansing
df_clean <- df %>%
  mutate(
    DateObj = ymd(Date),
    Month = factor(month(DateObj)),
    DayOfWeek = factor(wday(DateObj))
  ) %>%
  # fix tallinn outliers manually
  filter(!(Location == "Tallinn" & DateObj >= "2026-12-24")) %>%
  # fix 1 Vienna outlier
  filter(!(Location == "Vienna" & Average > 600)) %>%
  select(Location, Average, Month, DayOfWeek)

# training prep list
model_list <- list()
cities <- unique(df_clean$Location)

# for model eval
scorecard <- data.frame(
  City = character(),
  Test_RMSE = numeric(),
  Test_MAE = numeric(),
  stringsAsFactors = FALSE
)

set.seed(123) # randomsied splitting and seed defined for reproducibility

cat("=== TRAINING AND EVALUATING MODELS ===\n\n")

for(city in cities) {
  
  # isoalte city
  city_data <- df_clean %>% filter(Location == city)
  
  # create random split
  sample_size <- floor(0.8 * nrow(city_data))
  train_indices <- sample(seq_len(nrow(city_data)), size = sample_size)
  
  train_set <- city_data[train_indices, ]
  test_set  <- city_data[-train_indices, ]
  
  # train
  eval_model <- lm(Average ~ Month + DayOfWeek, data = train_set)
  
  # predict
  predictions <- tryCatch({
    predict(eval_model, newdata = test_set)
  }, error = function(e) return(rep(NA, nrow(test_set))))
  
  # calcualte performance error metrics
  if(any(is.na(predictions))) {
    rmse <- NA
    mae <- NA
  } else {
    rmse <- sqrt(mean((test_set$Average - predictions)^2))
    mae  <- mean(abs(test_set$Average - predictions))
  }
  
  # add to eval scorecard
  scorecard[nrow(scorecard) + 1, ] <- list(city, round(rmse, 2), round(mae, 2))
  
  # train final on all data
  final_model <- lm(Average ~ Month + DayOfWeek, data = city_data)
  model_list[[city]] <- final_model
}

# save models
saveRDS(model_list, "hotel_models.rds")