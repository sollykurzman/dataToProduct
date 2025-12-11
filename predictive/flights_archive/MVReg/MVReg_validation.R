library(tidyverse)
library(lubridate)

# load the csvs
# added a source column just in case i need it later
d1 <- read_csv("../../data/flightData.csv") %>% mutate(src = "nov")
d2 <- read_csv("../../data/flightData2.csv") %>% mutate(src = "dec")

# simple function to clean up the mess
fix_stuff <- function(d) {
  d %>%
    # get rid of the pound sign and make it a number
    mutate(price = as.numeric(gsub("£", "", Price))) %>%
    
    # fix the dates so r understands them
    mutate(
      Date = ymd(Date),
      Scraped = ymd(Scraped),
      days = as.numeric(Date - Scraped)
    ) %>%
    
    # remove dupes, keep the cheapest one found
    group_by(From, To, Date) %>%
    slice_min(price, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    
    # filter out weird stuff
    filter(!is.na(price), days > 0)
}

# clean both
clean1 <- fix_stuff(d1)
clean2 <- fix_stuff(d2)

# stack them together
# make month and day factors for the regression
all_data <- bind_rows(clean1, clean2) %>%
  mutate(
    mon = factor(month(Date, label = TRUE)),
    wd = factor(wday(Date, label = TRUE))
  ) %>%
  # remove flights way too far in the future
  filter(days < 330)

print(paste("rows:", nrow(all_data)))

# split into train and test
# set seed so results dont change
set.seed(123)
idx <- sample(seq_len(nrow(all_data)), size = 0.8 * nrow(all_data))

train <- all_data[idx, ]
test  <- all_data[-idx, ]

# train the linear model
# log(days) fixes the curve shape
print("training lm...")
mod <- lm(price ~ From + To * mon + wd + log(days), data = train)

# predict on test set
# interval prediction gives us the confidence range
preds <- predict(mod, newdata = test, interval = "prediction", level = 0.90)

# put actuals and preds together
res <- cbind(test, preds)

# calculate errors
rmse <- sqrt(mean((res$price - res$fit)^2))
mae <- mean(abs(res$price - res$fit))

print("--- stats ---")
print(paste("rmse:", round(rmse, 2)))
print(paste("mae:", round(mae, 2)))

# --- validation on new file ---
print("--- validating ---")

# load extra file
val_raw <- read_csv("../../data/flightDataValidation.csv")
val_clean <- fix_stuff(val_raw)

# fix factors same as before
val_data <- val_clean %>%
  mutate(
    mon = factor(month(Date, label = TRUE)),
    wd = factor(wday(Date, label = TRUE))
  ) %>%
  filter(days < 330)

# predict on validation
# using tryCatch cos sometimes levels mismatch
val_preds <- tryCatch({
  predict(mod, newdata = val_data, interval = "prediction", level = 0.90)
}, error = function(e) {
  stop("error predicting validation data")
})

# merge and calc stats
val_res <- cbind(val_data, val_preds)

v_rmse <- sqrt(mean((val_res$price - val_res$fit)^2))
v_mae <- mean(abs(val_res$price - val_res$fit))

print("=== validation results ===")
print(paste("val rmse:", round(v_rmse, 2)))
print(paste("val mae:", round(v_mae, 2)))

# check worst errors
val_res <- val_res %>%
  mutate(err = price - fit, abs_err = abs(err))

print("biggest mistakes:")
print(head(arrange(val_res, desc(abs_err))))