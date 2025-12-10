library(tidyverse)
library(lubridate)

# load model
# make sure file is in same folder
model <- readRDS("multiplicativeFactor2_model.rds")

# function to predict
get_flight_prediction <- function(origin, dest, date, scraped_date = Sys.Date()) {
  
  # upper case just in case
  origin <- toupper(origin)
  dest <- toupper(dest)
  route_name <- paste(origin, dest, sep = "-")
  
  # dates
  date_trip <- ymd(date)
  date_scraped <- ymd(scraped_date)
  
  days <- as.numeric(date_trip - date_scraped)
  mon <- month(date_trip, label = TRUE)
  wd <- wday(date_trip, label = TRUE)
  
  if (days < 0) return(NA) # cant predict past
  
  # BASE PRICE
  # find the route in the table
  b <- model$base %>% filter(route == route_name)
  
  if (nrow(b) == 0) {
    base_val <- model$avg_price # default
  } else {
    base_val <- b$base
  }
  
  # TIME FACTOR
  # if days is huge just use 365
  if (days > 365) days <- 365
  
  t <- model$curve %>% filter(days_ahead == days)
  if (nrow(t) == 0) {
    time_val <- 1
  } else {
    time_val <- t$multiplier
  }
  
  # SEASON FACTOR
  s <- model$season %>% filter(route == route_name, month == mon)
  if (nrow(s) == 0) {
    season_val <- 1
  } else {
    season_val <- s$factor
  }
  
  # DAY FACTOR
  d <- model$day %>% filter(route == route_name, wday == wd)
  if (nrow(d) == 0) {
    day_val <- 1
  } else {
    day_val <- d$factor
  }
  
  # calculate
  final <- base_val * time_val * season_val * day_val
  
  return(round(final, 2))
}

# test it
print(get_flight_prediction("LON", "STO", "2026-01-8"))