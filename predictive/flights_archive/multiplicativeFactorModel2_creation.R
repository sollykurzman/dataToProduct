library(tidyverse)
library(lubridate)

# clean the data columns
# just copy pasting this function because i use it 3 times
fix_data <- function(d) {
  d$Date <- ymd(d$Date)
  d$Scraped <- ymd(d$Scraped)
  # remove the pounds and commas
  d$Price <- as.numeric(str_remove(str_remove(d$Price, "£"), ","))
  d$days_ahead <- as.numeric(d$Date - d$Scraped)
  d$flight_id <- paste(d$From, d$To, d$Date, sep = "_")
  d$route <- paste(d$From, d$To, sep = "-")
  return(d)
}

# load files
print("loading data...")
df1 <- read_csv("data/flightData.csv")
df1 <- fix_data(df1)

df2 <- read_csv("data/flightData2.csv")
df2 <- fix_data(df2)

# BOOKING CURVE
# match the flights to see price change
combined <- inner_join(df1, df2, by = "flight_id")

# calc difference
combined$ratio <- combined$Price.y / combined$Price.x
combined$time_diff <- combined$days_ahead.x - combined$days_ahead.y
combined$daily_change <- combined$ratio ^ (1 / combined$time_diff)

# get median growth per day
growth_rates <- combined %>%
  group_by(days_ahead = days_ahead.y) %>%
  summarise(growth = median(daily_change, na.rm = TRUE))

# fill in missing days 0 to 365
all_days <- tibble(days_ahead = 0:365)
curve <- right_join(growth_rates, all_days, by = "days_ahead")
curve$growth[is.na(curve$growth)] <- 1 # fix nas

# make the curve from 365 down to 0
curve <- curve %>% arrange(desc(days_ahead))
curve$multiplier <- cumprod(curve$growth)

print("got booking curve")

# CLEAN PRICE
# join curve to new data
train <- left_join(df2, curve, by = "days_ahead")
train$multiplier[is.na(train$multiplier)] <- 1

# remove time effect
train$norm_price <- train$Price / train$multiplier

# add date info
train$month <- month(train$Date, label = TRUE)
train$wday <- wday(train$Date, label = TRUE)

# BASE PRICE
base_prices <- train %>%
  group_by(route) %>%
  summarise(base = median(norm_price, na.rm = TRUE))

print("got base prices")

# SEASONS
# join base price back
train <- left_join(train, base_prices, by = "route")

# see how much higher/lower month is vs base
train$month_ratio <- train$norm_price / train$base
season_factors <- train %>%
  group_by(route, month) %>%
  summarise(factor = median(month_ratio, na.rm = TRUE))

print("got seasons")

# WEEKDAYS
# join season factors back
train <- left_join(train, season_factors, by = c("route", "month"))

# remove base AND season to see just day effect
train$day_ratio <- train$norm_price / (train$base * train$factor)

day_factors <- train %>%
  group_by(route, wday) %>%
  summarise(factor = median(day_ratio, na.rm = TRUE))

print("got weekdays")

# save everything in a list
model_output <- list(
  curve = curve,
  base = base_prices,
  season = season_factors,
  day = day_factors,
  avg_price = median(train$norm_price) # fallback
)

saveRDS(model_output, "multiplicativeFactor2_model.rds")
print("done")