
# load the trained model
model <- readRDS("predictive/flight_model.rds")

# Base Price Lookup: Create a vector where names are "ORIGIN-DEST"
base_lookup <- setNames(model$base$base, model$base$route)

# Seasonality Lookup: Key is "ORIGIN-DEST_Month" (e.g., "LON-PAR_Jan")
model$season$lookup_key <- paste(model$season$route, model$season$month, sep = "_")
season_lookup <- setNames(model$season$factor, model$season$lookup_key)

# Day of Week Lookup: Key is "ORIGIN-DEST_Wday" (e.g., "LON-PAR_Mon")
model$day$lookup_key <- paste(model$day$route, model$day$wday, sep = "_")
day_lookup <- setNames(model$day$factor, model$day$lookup_key)

# Curve (Days Ahead) Lookup: Key is the number of days ahead
# Handle duplicates or gaps by ensuring unique mapping
curve_lookup <- setNames(model$curve$multiplier, as.character(model$curve$days_ahead))


get_flight_prediction <- function(origin, dest, trip_date, scraped_date = Sys.Date()) {
  
  if (is.null(model)) stop("Model not loaded!")
  
  origin <- str_trim(toupper(origin))
  dest   <- str_trim(toupper(dest))
  
  # Standardize Inputs
  date_trip <- ymd(trip_date)
  date_scraped <- ymd(scraped_date)
  
  # Calculate Days Ahead
  days <- as.numeric(date_trip - date_scraped)
  if (days < 0) return(NA)
  if (days > 365) days <- 365 # Cap at max curve range
  
  #  Define Routes
  route_direct  <- paste(origin, dest, sep = "-")
  route_flipped <- paste(dest, origin, sep = "-")
  
  # Base Price Lookup
  base_val <- base_lookup[route_direct]
  route_used <- route_direct
  
  # Handle missing direct route
  if (is.na(base_val)) {
    base_val <- base_lookup[route_flipped]
    route_used <- route_flipped
    
    # Fallback to average if both missing
    if (is.na(base_val)) {
      base_val <- model$avg_price
      route_used <- route_direct 
    }
  }
  
  # Time (Days ahead)
  time_val <- curve_lookup[as.character(days)]
  if (is.na(time_val)) time_val <- 1
  
  # Seasonality
  mon <- month(date_trip, label = TRUE, abbr = TRUE) # Ensure "Jan", "Feb" format
  season_key <- paste(route_used, mon, sep = "_")
  season_val <- season_lookup[season_key]
  if (is.na(season_val)) season_val <- 1
  
  # Day of Week
  wd <- wday(date_trip, label = TRUE, abbr = TRUE) # Ensure "Mon", "Tue" format
  day_key <- paste(route_used, wd, sep = "_")
  day_val <- day_lookup[day_key]
  if (is.na(day_val)) day_val <- 1
  
  # Final Calculation
  final <- base_val * time_val * season_val * day_val
  
  return(final)
}

# make the probability curve function
get_flight_cdf <- function(origin, dest, trip_date, scraped_date = Sys.Date()) {
  
  point_pred <- get_flight_prediction(origin, dest, trip_date, scraped_date)
  
  if (is.na(point_pred)) return(NA)
  
  # simulate prices using past errors
  simulated_prices <- point_pred * model$residuals
  
  my_cdf <- ecdf(simulated_prices)
  
  return(my_cdf)
}

# gets trend data for the graphs
get_route_trends <- function(origin, dest, model_obj = model) {
  
  origin <- str_trim(toupper(origin))
  dest   <- str_trim(toupper(dest))
  
  if (is.null(model_obj)) stop("Model not loaded!")

  # 1. Construct Route Keys
  route_direct  <- paste(origin, dest, sep = "-")
  route_flipped <- paste(dest, origin, sep = "-")

  # 2. Get Base Price 
  base_row <- model_obj$base %>% filter(route == route_direct)
  
  if (nrow(base_row) == 0) {
    base_row <- model_obj$base %>% filter(route == route_flipped)
  }
  
  current_base <- if(nrow(base_row) > 0) base_row$base else model_obj$avg_price

  # --- 3. SEASONALITY ---
  s_subset <- model_obj$season %>% filter(route == route_direct)
  
  if (nrow(s_subset) == 0) {
    s_subset <- model_obj$season %>% filter(route == route_flipped)
  }
  
  if (nrow(s_subset) == 0) {
    s_subset <- data.frame(month = factor(month.abb, levels=month.abb), factor = 1)
  } 

  s_final <- s_subset %>%
    ungroup() %>%    # <--- ADDED THIS (Removes grouping by 'route')
    mutate(
      Month = month,
      base_price = round(factor * current_base, 2)
    ) %>%
    select(Month, base_price) %>%
    arrange(match(Month, month.abb))

  # --- 4. WEEKDAY ---
  d_subset <- model_obj$day %>% filter(route == route_direct)
  
  if (nrow(d_subset) == 0) {
    d_subset <- model_obj$day %>% filter(route == route_flipped)
  }
  
  if (nrow(d_subset) == 0) {
    days_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    d_subset <- data.frame(wday = factor(days_order, levels=days_order), factor = 1)
  }
  
  d_final <- d_subset %>%
    ungroup() %>%    # <--- ADDED THIS (Removes grouping by 'route')
    mutate(
      weekday = wday,
      day_modifier = round((factor * current_base) - current_base, 2)
    ) %>%
    select(weekday, day_modifier)
  
  return(list(
    seasonality = s_final,
    weekday = d_final
  ))
}