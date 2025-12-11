library(tidyverse)
library(lubridate)
library(modelr) 

# reproducible rando seed
set.seed(123)


df_master <- read_csv("data/master_flight_data.csv", show_col_types = FALSE) %>%
  mutate(
    # re-parsing dates and factors for training
    Date = ymd(Date),
    Scraped = ymd(Scraped),
    Month = factor(month(Date, label = TRUE), ordered = FALSE),
    Weekday = factor(wday(Date, label = TRUE), ordered = FALSE),
    days_ahead = as.numeric(days_ahead)
  )

# 2. SPLIT DATA
# ------------------------------------------------------------------------------
# doing the standard 80/20 split 
# train on 80% and keep 20% just to check rmse at the end
train_idx <- sample(seq_len(nrow(df_master)), size = 0.8 * nrow(df_master))
train_set <- df_master[train_idx, ]
test_set  <- df_master[-train_idx, ]

# getting the median price for every route + month combo
# using median to ignore crazy expensive outlier flights
layer1_seasonality <- train_set %>%
  group_by(From, To, Month) %>%
  summarise(base_price = median(numeric_price), .groups = "drop")

# global fallback just in case we see a route we dont know
global_median <- median(train_set$numeric_price)

# calculate 'residual A' (what's left after accounting for season)
train_step1 <- train_set %>%
  left_join(layer1_seasonality, by = c("From", "To", "Month")) %>%
  mutate(res_A = numeric_price - base_price) %>%
  filter(!is.na(res_A))


# checking if fridays/weekends are more expensive for specific routes
layer2_weekday <- train_step1 %>%
  group_by(From, To, Weekday) %>%
  summarise(day_modifier = median(res_A), .groups = "drop")

# calculate 'residual B' (stripping away the weekday effect too)
train_step2 <- train_step1 %>%
  left_join(layer2_weekday, by = c("From", "To", "Weekday")) %>%
  mutate(
    day_modifier = replace_na(day_modifier, 0),
    res_B = res_A - day_modifier
  )


# 5-fold cross validation to find the best span
# we don't want a wiggly line (overfitting) so min span is 0.3
span_grid <- seq(0.3, 0.9, by = 0.1)

# helper function to test a span
cv_score <- function(s, data) {
  folds <- crossv_kfold(data, k = 5)
  rmse_vals <- map2_dbl(folds$train, folds$test, function(train_dat, test_dat) {
    t_df <- as.data.frame(train_dat)
    v_df <- as.data.frame(test_dat)
    
    # training on the residuals (res_B)
    m <- loess(res_B ~ days_ahead, data = t_df, span = s, 
               control = loess.control(surface = "interpolate"))
    
    preds <- predict(m, newdata = v_df)
    sqrt(mean((v_df$res_B - preds)^2, na.rm = TRUE))
  })
  mean(rmse_vals, na.rm = TRUE)
}

# picking the winner
cv_results <- tibble(span = span_grid) %>%
  mutate(rmse = map_dbl(span, ~ cv_score(.x, train_step2)))

best_span <- cv_results %>% slice_min(rmse) %>% pull(span)
print(paste("Best span found:", best_span))

# training the final curve on all data
final_loess <- loess(res_B ~ days_ahead, 
                     data = train_step2, 
                     span = best_span,
                     control = loess.control(surface = "direct"))


# bundling everything into a list so the app can use it
model_bundle <- list(
  l1_seasonality = layer1_seasonality,
  l2_weekday = layer2_weekday,
  l3_curve = final_loess,
  residuals = final_loess$residuals, # saving errors for the CFD plot
  global_median = global_median
)

# save to file
saveRDS(model_bundle, "flight_model.rds")
print("Model saved as 'flight_model.rds'")