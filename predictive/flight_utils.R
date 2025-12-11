# load the model file
model <- readRDS("predictive/flight_model.rds")

# main function to get a single price prediction
get_flight_prediction <- function(origin, dest, trip_date, scraped_date = Sys.Date()) {
  
  # check if model exists
  if (is.null(model)) stop("Model not loaded!")
  
  # clean up inputs
  origin <- str_trim(toupper(origin))
  dest   <- str_trim(toupper(dest))
  
  # define possible routes (A->B and B->A)
  route_direct  <- paste(origin, dest, sep = "-")
  route_flipped <- paste(dest, origin, sep = "-")
  
  # process dates
  date_trip <- ymd(trip_date)
  date_scraped <- ymd(scraped_date)
  
  # get time features
  days <- as.numeric(date_trip - date_scraped)
  mon <- month(date_trip, label = TRUE)
  wd <- wday(date_trip, label = TRUE)
  
  # basic error check
  if (days < 0) return(NA) 
  
  # 1. BASE PRICE
  # try to find the direct route first
  b <- model$base %>% filter(route == route_direct)
  
  if (nrow(b) > 0) {
    # found it directly
    base_val   <- b$base
    route_name <- route_direct
    
  } else {
    # try the return route if direct is missing
    b_flipped <- model$base %>% filter(route == route_flipped)
    
    if (nrow(b_flipped) > 0) {
      # found the flipped version, use that for everything
      base_val   <- b_flipped$base
      route_name <- route_flipped
    } else {
      # route doesn't exist, use global average
      base_val   <- model$avg_price
      route_name <- route_direct # fallback
    }
  }
  
  # 2. TIME FACTOR (how far in advance)
  # cap at 365 days
  if (days > 365) days <- 365
  
  t <- model$curve %>% filter(days_ahead == days)
  if (nrow(t) == 0) {
    time_val <- 1
  } else {
    time_val <- t$multiplier
  }
  
  # 3. SEASON FACTOR (month of year)
  # uses the route_name we found earlier
  s <- model$season %>% filter(route == route_name, month == mon)
  if (nrow(s) == 0) {
    season_val <- 1
  } else {
    season_val <- s$factor
  }
  
  # 4. DAY FACTOR (day of week)
  d <- model$day %>% filter(route == route_name, wday == wd)
  if (nrow(d) == 0) {
    day_val <- 1
  } else {
    day_val <- d$factor
  }
  
  # calculate final price
  final <- base_val * time_val * season_val * day_val
  
  return(final)
}

# returns a cdf function for probability plotting
get_flight_cdf <- function(origin, dest, trip_date, scraped_date = Sys.Date()) {
  
  # get the estimated price using the function above
  point_pred <- get_flight_prediction(origin, dest, trip_date, scraped_date)
  
  if (is.na(point_pred)) return(NA)
  
  # simulate possible prices based on historical errors
  simulated_prices <- point_pred * model$residuals
  
  # create the cumulative distribution function
  my_cdf <- ecdf(simulated_prices)
  
  return(my_cdf)
}

# gets trend data for the graphs
get_route_trends <- function(origin, dest, model = .global_model) {
  origin <- str_trim(toupper(origin))
  dest   <- str_trim(toupper(dest))
  
  if (is.null(model)) stop("Model not loaded!")

  # 1. SEASONALITY (Monthly trends)
  # try direct route
  l1_subset <- model$l1 %>% filter(From == origin, To == dest)
  
  # if empty, try the flipped route
  if (nrow(l1_subset) == 0) {
    l1_subset <- model$l1 %>% filter(From == dest, To == origin)
  }
  
  # if still empty, return a flat line so it doesn't crash
  if (nrow(l1_subset) == 0) {
    l1_subset <- data.frame(Month = factor(month.abb, levels=month.abb), base_price = 0)
  } else {
    # sort by month order
    l1_subset <- l1_subset %>% arrange(match(Month, month.abb))
  }

  # 2. WEEKDAY TRENDS
  # try direct route
  l2_subset <- model$l2 %>% filter(From == origin, To == dest)
  
  # if empty, try flipped route
  if (nrow(l2_subset) == 0) {
    l2_subset <- model$l2 %>% filter(From == dest, To == origin)
  }
  
  # return list for plotting
  return(list(
    seasonality = l1_subset,
    weekday = l2_subset
  ))
}

# # load model
# # make sure file is in same folder
# model <- readRDS("predictive/flight_model.rds")

# # function to predict point value
# get_flight_prediction <- function(origin, dest, trip_date, scraped_date = Sys.Date()) {
#   if (is.null(model)) stop("Model not loaded!")
#   # print(paste0("Predicting flight from ", origin, " to ", dest, " on ", trip_date, " (scraped on ", scraped_date, ")"))
  
#   # upper case just in case
#   origin <- str_trim(toupper(origin))
#   dest   <- str_trim(toupper(dest))
#   route_name <- paste(origin, dest, sep = "-")
  
#   # dates
#   date_trip <- ymd(trip_date)
#   date_scraped <- ymd(scraped_date)
  
#   days <- as.numeric(date_trip - date_scraped)
#   mon <- month(date_trip, label = TRUE)
#   wd <- wday(date_trip, label = TRUE)
  
#   if (days < 0) return(NA) # cant predict past
  
#   # BASE PRICE
#   # find the route in the table
#   b <- model$base %>% filter(route == route_name)
  
#   if (nrow(b) == 0) {
#     base_val <- model$avg_price # default
#   } else {
#     base_val <- b$base
#   }
  
#   # TIME FACTOR
#   # if days is huge just use 365
#   if (days > 365) days <- 365
  
#   t <- model$curve %>% filter(days_ahead == days)
#   if (nrow(t) == 0) {
#     time_val <- 1
#   } else {
#     time_val <- t$multiplier
#   }

#   # print(paste0("Time factor: ", time_val))
  
#   # SEASON FACTOR
#   s <- model$season %>% filter(route == route_name, month == mon)
#   if (nrow(s) == 0) {
#     season_val <- 1
#   } else {
#     season_val <- s$factor
#   }

#   # print(paste0("Season factor: ", season_val))
  
#   # DAY FACTOR
#   d <- model$day %>% filter(route == route_name, wday == wd)
#   if (nrow(d) == 0) {
#     day_val <- 1
#   } else {
#     day_val <- d$factor
#   }
  
#   # print(paste0("Day factor: ", day_val))

#   # calculate
#   final <- base_val * time_val * season_val * day_val

#   # print(paste0("Predicted price: ", final))
  
#   return(final)
# }

# # NEW FUNCTION: PROBABILISTIC FORECAST
# # returns a CDF function you can query
# get_flight_cdf <- function(origin, dest, trip_date, scraped_date = Sys.Date()) {
  
#   # 1. get the main prediction
#   point_pred <- get_flight_prediction(origin, dest, trip_date, scraped_date)
  
#   if (is.na(point_pred)) return(NA)
  
#   # 2. bootstrap
#   # assume we make the same errors as we did in the past
#   # multiply point prediction by all historical error ratios
#   simulated_prices <- point_pred * model$residuals
  
#   # 3. make cdf
#   # ecdf creates a function that tells you probability X <= x
#   my_cdf <- ecdf(simulated_prices)
  
#   return(my_cdf)
# }


# get_route_trends <- function(origin, dest, model = .global_model) {
#   origin <- str_trim(toupper(origin))
#   dest   <- str_trim(toupper(dest))
  
#   if (is.null(model)) stop("Model not loaded!")

#   # --- 1. Get Seasonality (Month) ---
#   # Try direct route
#   l1_subset <- model$l1 %>% filter(From == origin, To == dest)
  
#   # If empty, try swapped route (assuming seasonality is symmetric)
#   if (nrow(l1_subset) == 0) {
#     l1_subset <- model$l1 %>% filter(From == dest, To == origin)
#   }
  
#   # If still empty, return generic flat line (safety)
#   if (nrow(l1_subset) == 0) {
#     l1_subset <- data.frame(Month = factor(month.abb, levels=month.abb), base_price = 0)
#   } else {
#     # Ensure all months are present for the plot, even if missing in data
#     # (Optional, but makes graphs look better)
#     l1_subset <- l1_subset %>% arrange(match(Month, month.abb))
#   }

#   # --- 2. Get Weekday Trends ---
#   # Try direct route
#   l2_subset <- model$l2 %>% filter(From == origin, To == dest)
  
#   # If empty, try swapped route
#   if (nrow(l2_subset) == 0) {
#     l2_subset <- model$l2 %>% filter(From == dest, To == origin)
#   }
  
#   # Return structured data for plotting
#   return(list(
#     seasonality = l1_subset, # Columns: Month, base_price
#     weekday = l2_subset      # Columns: Weekday, day_modifier
#   ))
# }

# # test it
# # print(round(get_flight_prediction("LON", "TLL", "2026-01-05"), 2))
