
# check if return date is after start date
validate_dates <- function(start, end) {
  end > start
}

# find lat/lon for start airport
get_departure_info <- function(airport_code, departures_df) {
  departures_df %>% filter(code == airport_code)
}

# get list of everywhere else
get_other_airports <- function(airport_code, airports_df) {
  airports_df %>% filter(code != airport_code)
}

# days between dates
get_trip_duration <- function(start, end) {
  as.numeric(difftime(end, start, units = "days"))
}

# get flight prices for all potential destinations
flight_cost <- function(dep, arr_df, date) {

  map_dbl(arr_df$code, function(single_dest) {
    prices <- get_flight_prediction(
      origin = dep, 
      dest = single_dest, 
      trip_date = date
    )

    return(prices)
  })
}

# get hotel prices for the trip duration
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

# get living costs (food, beer etc)
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

# master function to calculate total trip cost
get_trip_costs <- function(departure_code, arrivals_df, departure_date, return_date) {
    
    # get costs for each component
    departure_flight_price_vector <- flight_cost(departure_code, arrivals_df, departure_date)
    return_flight_price_vector <- flight_cost(departure_code, arrivals_df, return_date)
    hotel_price_vector <- hotel_cost(arrivals_df, departure_date, return_date)
    living_cost_vector <- living_cost(arrivals_df, departure_date, return_date)

    trip_duration <- get_trip_duration(departure_date, return_date)

    # store everything in the dataframe
    arrivals_df$departure_flight_cost <- round(departure_flight_price_vector, 2)
    arrivals_df$return_flight_cost <- round(return_flight_price_vector, 2)
    arrivals_df$flight_cost <- round(arrivals_df$departure_flight_cost + arrivals_df$return_flight_cost, 2)

    arrivals_df$hotel_cost <- round(hotel_price_vector, 2)

    arrivals_df$living_cost <- round(living_cost_vector, 2)

    arrivals_df$total_cost <- round(arrivals_df$flight_cost + arrivals_df$hotel_cost + arrivals_df$living_cost, 2)

    return(arrivals_df)
}

# make a list of dates around the target
get_date_window <- function(date, x) {
  date <- as.Date(date)
  seq(from = date - x, to = date + x, by = "day")
}

# fetch price history for the bar graphs
get_prices_around <- function(departure_code, city_df, departure_date, return_date, window = 3) {
  
  dest_code <- city_df$code
  
  # helper to get one leg of the journey
  get_leg_data <- function(origin, dest, center_date, win) {
    
    date_window <- get_date_window(center_date, win)
    
    # get absolute prices
    prices <- sapply(date_window, function(d) {
      if (d < Sys.Date()) return(NA) 
      
      tryCatch({
        price <- get_flight_prediction(origin, dest, d)
        return(price)
      }, error = function(e) {
        return(NA) 
      })
    })
    
    # calculate difference from chosen date
    center_index <- win + 1
    base_price <- prices[center_index]
    
    if (is.na(base_price)) base_price <- 0
    
    price_diff <- prices - base_price
    price_diff[is.na(price_diff)] <- 0

    return(list(window = date_window, prices = price_diff))
  }

  # get departure leg
  dep_data <- get_leg_data(departure_code, dest_code, departure_date, window)

  # get return leg (swapping origin/dest)
  ret_data <- get_leg_data(dest_code, departure_code, return_date, window)

  # return everything
  list(
    departure_window = dep_data$window,
    departure_prices = dep_data$prices,
    return_window    = ret_data$window,
    return_prices    = ret_data$prices
  )
}

# remove cities that are too expensive
filter_cities_by_budget <- function(budget_range, cities_df) {
  cities_df %>%
    filter(total_cost >= budget_range[1], total_cost <= budget_range[2])
}

# math magic for curved flight paths
create_curved_path <- function(lon1, lat1, lon2, lat2, num_points = 50) {
  
  deg2rad <- function(deg) deg * pi / 180
  rad2deg <- function(rad) rad * 180 / pi
  
  # convert to 3D coords
  to_cartesian <- function(lon, lat) {
    phi <- deg2rad(lat)
    lambda <- deg2rad(lon)
    x <- cos(phi) * cos(lambda)
    y <- cos(phi) * sin(lambda)
    z <- sin(phi)
    return(c(x, y, z))
  }
  
  P1 <- to_cartesian(lon1, lat1)
  P2 <- to_cartesian(lon2, lat2)
  
  # angle between points
  dot <- sum(P1 * P2)
  dot <- max(min(dot, 1), -1)
  omega <- acos(dot)
  
  if (omega < 1e-6) {
    return(data.frame(lon = rep(lon1, num_points), lat = rep(lat1, num_points)))
  }
  
  # interpolate points along the curve
  t_seq <- seq(0, 1, length.out = num_points)
  
  path_list <- lapply(t_seq, function(t) {
    term1 <- sin((1 - t) * omega) / sin(omega)
    term2 <- sin(t * omega) / sin(omega)
    
    P_t <- (term1 * P1) + (term2 * P2)
    
    lat_new <- asin(P_t[3])
    lon_new <- atan2(P_t[2], P_t[1])
    
    return(c(rad2deg(lon_new), rad2deg(lat_new)))
  })
  
  path_df <- do.call(rbind, path_list)
  colnames(path_df) <- c("lon", "lat")
  
  return(as.data.frame(path_df))
}

# generate the html for the map popup
make_destination_popup <- function(city_data, trip_duration, departure_code) {
  flight_percentage <- round((city_data$flight_cost / city_data$total_cost) * 100, 2)
  hotel_percentage  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 2)
  living_percentage <- round((city_data$living_cost / city_data$total_cost) * 100, 2)
  
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
    '<div class="popup-price-value">£', sprintf("%.2f", city_data$total_cost), '</div>',
    '</div>',
    '</div>',
    '</div>'
  )
}

# generate the html for the sidebar list
make_destination_box <- function(city_data, trip_duration, departure_code) {
  if(nrow(city_data) == 0) return("")

  flight_percentage <- round((city_data$flight_cost / city_data$total_cost) * 100, 2)
  hotel_percentage  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 2)
  living_percentage <- round((city_data$living_cost / city_data$total_cost) * 100, 2)
  
  paste0(
    '<div class="sidebaritem" data-city="', city_data$city, '">',
    '<img src="sidebar_images/', city_data$city, '.png" onerror="this.style.display=\'none\'"/>', 
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
    '<h4>Total: £', sprintf("%.2f", city_data$total_cost), '</h4>',
    '</div>'
  )
}

# scrape a summary from wikipedia
get_wiki_intro <- function(city_name) {
  safe_title <- URLencode(city_name)
  url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", safe_title)
  
  tryCatch({
    result <- jsonlite::fromJSON(url)
    return(result$extract)
    
  }, error = function(e) {
    return(paste("Could not fetch description for", city_name))
  })
}