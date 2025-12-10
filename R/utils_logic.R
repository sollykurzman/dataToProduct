# utils_logic.R
# --- Data Helpers ---

validate_dates <- function(start, end) {
  end > start
}

get_departure_info <- function(airport_code, departures_df) {
  departures_df %>% filter(code == airport_code)
}

get_other_airports <- function(airport_code, airports_df) {
  airports_df %>% filter(code != airport_code)
}

get_trip_duration <- function(start, end) {
  as.numeric(difftime(end, start, units = "days"))
}

# --- Cost Calculation ---

# Placeholder price generator
dummy_cost_function <- function(dep, arr_df) {
    runif(nrow(arr_df), min = 10, max = 50) |> round()
}

# dummy_flight_cost_function <- function(dep, arr_df, date) {
#     runif(nrow(arr_df), min = 10, max = 50) |> round()
# }

flight_cost <- function(dep, arr_df, date) {

  map_dbl(arr_df$code, function(single_dest) {
    prices <- get_flight_prediction(
      origin = dep, 
      dest = single_dest, 
      trip_date = date
    )

    return(prices$price)
  })
}

hotel_cost <- function(arr_df, arrival_date, leaving_date) {

  map_dbl(arr_df$code, function(single_dest) {
    prices <- get_total_hotel_cost(
      shortcode = single_dest,
      arrival_date = arrival_date,
      leaving_date = leaving_date
    )

    return(prices) 
  })
}

living_cost <- function(arr_df, arrival_date, leaving_date) {

  map_dbl(arr_df$code, function(single_dest) {
    prices <- get_total_living_cost(
      shortcode = single_dest,
      arrival_date = arrival_date,
      leaving_date = leaving_date
    )

    return(prices$total)
  })
}

get_trip_costs <- function(departure_code, arrivals_df, departure_date, return_date) {
    
    departure_flight_price_vector <- flight_cost(departure_code, arrivals_df, departure_date)
    return_flight_price_vector <- flight_cost(departure_code, arrivals_df, return_date)
    hotel_price_vector <- hotel_cost(arrivals_df, departure_date, return_date)
    living_cost_vector <- living_cost(arrivals_df, departure_date, return_date)

    trip_duration <- get_trip_duration(departure_date, return_date)

    # index <- 3
    
    arrivals_df$departure_flight_cost <- round(departure_flight_price_vector, 2)
    arrivals_df$return_flight_cost <- round(return_flight_price_vector, 2)
    arrivals_df$flight_cost <- round(arrivals_df$departure_flight_cost + arrivals_df$return_flight_cost, 2)

    # Uncomment for debugging
    # print(paste0("Flight Cost: ", arrivals_df$flight_cost[index]))

    arrivals_df$hotel_cost <- round(hotel_price_vector, 2)

    # Uncomment for debugging
    # print(paste0("Hotel Cost: ", arrivals_df$hotel_cost[index]))

    arrivals_df$living_cost <- round(living_cost_vector, 2)

    # Uncomment for debugging
    # print(paste0("Living Cost: ", arrivals_df$living_cost[index]))

    arrivals_df$total_cost <- round(arrivals_df$flight_cost + arrivals_df$hotel_cost + arrivals_df$living_cost, 2)

    # print(paste0("Total Cost: ", arrivals_df$total_cost))

  
    return(arrivals_df)
}

get_date_window <- function(date, x) {
  date <- as.Date(date)
  seq(from = date - x, to = date + x, by = "day")
}

get_prices_around <- function(departure_code, city_df, departure_date, return_date, window = 3) {
  
  dest_code <- city_df$code
  
  # --- 1. Define the Helper Function ---
  # This encapsulates all the logic for ONE flight leg (getting dates, fetching prices, calculating diffs)
  get_leg_data <- function(origin, dest, center_date, win) {
    print(paste0("Getting leg data for ", origin, " -> ", dest, " around ", center_date))
    
    # A. Build the date window
    date_window <- get_date_window(center_date, win)
    
    # B. Fetch Absolute Prices for these dates
    prices <- sapply(date_window, function(d) {
      if (d < Sys.Date()) return(NA) # Handle past dates
      
      tryCatch({
        # Call the predictive model
        pred <- get_flight_prediction(origin, dest, d)
        return(pred$price)
      }, error = function(e) {
        return(NA) # Handle model failure
      })
    })
    
    # C. Calculate Relative Difference
    center_index <- win + 1
    base_price <- prices[center_index]
    
    # Safety: If the center date itself is invalid (NA), assume baseline is 0 to avoid crashing
    if (is.na(base_price)) base_price <- 0
    
    # Calculate difference (Price - Base)
    price_diff <- prices - base_price
    
    # Clean up NAs (treat invalid neighbor dates as "0 difference" or "0 price")
    price_diff[is.na(price_diff)] <- 0

    
    # Return both the dates (x-axis) and the price differences (y-axis)
    return(list(window = date_window, prices = price_diff))
  }

  # --- 2. Call the Helper Twice ---
  
  # Leg 1: DEPARTURE (Origin -> Destination)
  dep_data <- get_leg_data(departure_code, dest_code, departure_date, window)

  print(paste0("Dep Data Prices: ", paste(dep_data$prices, collapse = ", ")))
  
  # Leg 2: RETURN (Destination -> Origin)
  # *CRITICAL FIX*: We swap the airports here so we check the price of flying BACK.
  ret_data <- get_leg_data(dest_code, departure_code, return_date, window)

  print(paste0("Ret Data Prices: ", paste(ret_data$prices, collapse = ", ")))

  # --- 3. Return Combined Structure ---
  list(
    departure_window = dep_data$window,
    departure_prices = dep_data$prices,
    
    return_window    = ret_data$window,
    return_prices    = ret_data$prices
  )
}


filter_cities_by_budget <- function(budget_range, cities_df) {
  cities_df %>%
    filter(total_cost >= budget_range[1], total_cost <= budget_range[2])
}

# --- Map Geometry ---

# Included this here because it is a calculation helper used in server_main
create_curved_path <- function(lon1, lat1, lon2, lat2, num_points = 50) {
  # Simple Great Circle approximation or bezier curve logic
  # utilizing the 'geosphere' package usually, or simple interpolation
  
  # Using gcIntermediate from geosphere is best, 
  # but here is a self-contained fallback if you don't want extra dependencies:
  
  t <- seq(0, 1, length.out = num_points)
  
  # Linear interpolation for coords (Simple Line)
  # To curve it, we add an offset to the latitude based on a sine wave
  lons <- lon1 + (lon2 - lon1) * t
  lats <- lat1 + (lat2 - lat1) * t
  
  # Add "arch" effect
  dist_factor <- sqrt((lon2 - lon1)^2 + (lat2 - lat1)^2)
  arch_height <- dist_factor * 0.1 # 10% arch
  
  lats <- lats + sin(t * pi) * arch_height
  
  data.frame(lon = lons, lat = lats)
}

# --- HTML Generation ---

# make_destination_popup <- function(city_data, trip_duration, departure_code) {
#   flight_percentage <- round((city_data$flight_cost / city_data$total_cost) * 100, 1)
#   hotel_percentage  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 1)
#   living_percentage <- round((city_data$living_cost / city_data$total_cost) * 100, 1)
  
#   paste0(
#     '<div>',
#     '<div class="popup-header">', city_data$city, '</div>',
#     '<div class="popup-body">',
#     '<div class="popup-row">',
#     '<span class="popup-label">Country</span>',
#     '<span class="popup-value">', city_data$country, '</span>',
#     '</div>',
#     '<div class="popup-row">',
#     '<span class="popup-label">Duration</span>',
#     '<span class="popup-value">', trip_duration, ' days</span>',
#     '</div>',
#     '<div class="breakdown-section">',
#     '<div class="breakdown-title">Cost Breakdown</div>',
#     '<div class="cost-bar-container">',
#     '<div class="cost-bar-segment flight" style="width:', flight_percentage, '%" title="Flight: £', city_data$flight_cost, '"></div>',
#     '<div class="cost-bar-segment hotel" style="width:', hotel_percentage, '%" title="Hotel: £', city_data$hotel_cost, '"></div>',
#     '<div class="cost-bar-segment living" style="width:', living_percentage, '%" title="Living: £', city_data$living_cost, '"></div>',
#     '</div>',
#     '<div class="cost-legend">',
#     '<span class="legend-item"><span class="dot flight"></span>Flight</span>',
#     '<span class="legend-item"><span class="dot hotel"></span>Hotel</span>',
#     '<span class="legend-item"><span class="dot living"></span>Living</span>',
#     '</div>',
#     '</div>',
#     '<div class="popup-price">',
#     '<div class="popup-price-label">Total Estimate</div>',
#     '<div class="popup-price-value">£', city_data$total_cost, '</div>',
#     '</div>',
#     '</div>',
#     '</div>'
#   )
# }

# make_destination_box <- function(city_data, trip_duration, departure_code) {
#   # If data is empty, return generic message (handled in logic or here)
#   if(nrow(city_data) == 0) return("")

#   # Vectorized paste operation
#   flight_percentage <- round((city_data$flight_cost / city_data$total_cost) * 100, 1)
#   hotel_percentage  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 1)
#   living_percentage <- round((city_data$living_cost / city_data$total_cost) * 100, 1)
  
#   paste0(
#     # Note: data-city attribute is critical for the JS click listener
#     '<div class="sidebaritem" data-city="', city_data$city, '">',
#     '<img src="sidebar_images/', city_data$city, '.png" onerror="this.style.display=\'none\'"/>', # Added error handler
#     '<h3>', city_data$city, '</h3>',
#     '<div class="cost-bar-container">',
#     '<div class="cost-bar-segment flight" style="width:', flight_percentage, '%"></div>',
#     '<div class="cost-bar-segment hotel" style="width:', hotel_percentage, '%"></div>',
#     '<div class="cost-bar-segment living" style="width:', living_percentage, '%"></div>',
#     '</div>',
#     '<div class="cost-legend">',
#     '<span class="legend-item"><span class="dot flight"></span>Flight</span>',
#     '<span class="legend-item"><span class="dot hotel"></span>Hotel</span>',
#     '<span class="legend-item"><span class="dot living"></span>Living</span>',
#     '</div>',
#     '<h4>Total: £', city_data$total_cost, '</h4>',
#     '</div>'
#   )
# }

make_destination_popup <- function(city_data, trip_duration, departure_code) {
  flight_percentage <- round((city_data$flight_cost / city_data$total_cost) * 100, 1)
  hotel_percentage  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 1)
  living_percentage <- round((city_data$living_cost / city_data$total_cost) * 100, 1)
  
  paste0(
    '<div>',
    '<div class="popup-header">', city_data$city, '</div>',
    '<div class="popup-body">',
    '<div class="popup-row">',
    '<span class="popup-label">Country</span>',
    '<span class="popup-value">', city_data$country, '</span>',
    '</div>',
    '<div class="popup-row">',
    '<span class="popup-label">Duration</span>',
    '<span class="popup-value">', trip_duration, ' days</span>',
    '</div>',
    '<div class="breakdown-section">',
    '<div class="breakdown-title">Cost Breakdown</div>',
    '<div class="cost-bar-container">',
    # CHANGED: Added sprintf("%.2f", ...) to hover titles
    '<div class="cost-bar-segment flight" style="width:', flight_percentage, '%" title="Flight: £', sprintf("%.2f", city_data$flight_cost), '"></div>',
    '<div class="cost-bar-segment hotel" style="width:', hotel_percentage, '%" title="Hotel: £', sprintf("%.2f", city_data$hotel_cost), '"></div>',
    '<div class="cost-bar-segment living" style="width:', living_percentage, '%" title="Living: £', sprintf("%.2f", city_data$living_cost), '"></div>',
    '</div>',
    '<div class="cost-legend">',
    '<span class="legend-item"><span class="dot flight"></span>Flight</span>',
    '<span class="legend-item"><span class="dot hotel"></span>Hotel</span>',
    '<span class="legend-item"><span class="dot living"></span>Living</span>',
    '</div>',
    '</div>',
    '<div class="popup-price">',
    '<div class="popup-price-label">Total Estimate</div>',
    # CHANGED: Added sprintf("%.2f", ...) to total price
    '<div class="popup-price-value">£', sprintf("%.2f", city_data$total_cost), '</div>',
    '</div>',
    '</div>',
    '</div>'
  )
}

make_destination_box <- function(city_data, trip_duration, departure_code) {
  # If data is empty, return generic message (handled in logic or here)
  if(nrow(city_data) == 0) return("")

  # Vectorized paste operation
  flight_percentage <- round((city_data$flight_cost / city_data$total_cost) * 100, 1)
  hotel_percentage  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 1)
  living_percentage <- round((city_data$living_cost / city_data$total_cost) * 100, 1)
  
  paste0(
    # Note: data-city attribute is critical for the JS click listener
    '<div class="sidebaritem" data-city="', city_data$city, '">',
    '<img src="sidebar_images/', city_data$city, '.png" onerror="this.style.display=\'none\'"/>', # Added error handler
    '<h3>', city_data$city, '</h3>',
    '<div class="cost-bar-container">',
    '<div class="cost-bar-segment flight" style="width:', flight_percentage, '%"></div>',
    '<div class="cost-bar-segment hotel" style="width:', hotel_percentage, '%"></div>',
    '<div class="cost-bar-segment living" style="width:', living_percentage, '%"></div>',
    '</div>',
    '<div class="cost-legend">',
    '<span class="legend-item"><span class="dot flight"></span>Flight</span>',
    '<span class="legend-item"><span class="dot hotel"></span>Hotel</span>',
    '<span class="legend-item"><span class="dot living"></span>Living</span>',
    '</div>',
    # CHANGED: Added sprintf("%.2f", ...) to total price
    '<h4>Total: £', sprintf("%.2f", city_data$total_cost), '</h4>',
    '</div>'
  )
}