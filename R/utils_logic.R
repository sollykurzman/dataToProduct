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

    return(result$price)
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

    # print(paste0("Departure Flight Cost, out:", departure_flight_price_vector, " in:", return_flight_price_vector))

    trip_duration <- get_trip_duration(departure_date, return_date)

    # index <- 3
    
    # Note: Logic preserved from your snippet (multiplying all by duration)
    arrivals_df$departure_flight_cost <- departure_flight_price_vector
    arrivals_df$return_flight_cost <- return_flight_price_vector
    arrivals_df$flight_cost <- departure_flight_price_vector + return_flight_price_vector
    # print(paste0("Flight Cost: ", arrivals_df$flight_cost[index]))
    arrivals_df$hotel_cost <- hotel_price_vector
    # print(paste0("Hotel Cost: ", arrivals_df$hotel_cost[index]))
    arrivals_df$living_cost <- living_cost_vector
    # print(paste0("Living Cost: ", arrivals_df$living_cost[index]))
    arrivals_df$total_cost <- (arrivals_df$flight_cost + arrivals_df$hotel_cost + arrivals_df$living_cost)

    print(paste0("Total Cost: ", arrivals_df$total_cost))
  
    return(arrivals_df)
}

get_date_window <- function(date, x) {
  date <- as.Date(date)
  seq(from = date - x, to = date + x, by = "day")
}

get_prices_around <- function(departure_code, city_df, departure_date, return_date, window = 3) {
  
  # 1) Build date windows
  dep_window <- get_date_window(departure_date, window)
  ret_window <- get_date_window(return_date, window)
  
  # 2) Price generator (placeholder)
  dummy_flight_cost_function <- function(dep, arr_df, date_vec) {
    # Return a vector of prices, one for each date
    runif(length(date_vec), min = 10, max = 50) |> round()
  }
  
  # 3) Generate prices for each date in the windows
  dep_prices <- dummy_flight_cost_function(departure_code, city_df, dep_window)
  ret_prices <- dummy_flight_cost_function(departure_code, city_df, ret_window)
  
  # 4) Return structured result
  list(
    departure_window = dep_window,
    departure_prices = dep_prices,
    
    return_window    = ret_window,
    return_prices    = ret_prices
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
    '<div class="cost-bar-segment flight" style="width:', flight_percentage, '%" title="Flight: £', city_data$flight_cost, '"></div>',
    '<div class="cost-bar-segment hotel" style="width:', hotel_percentage, '%" title="Hotel: £', city_data$hotel_cost, '"></div>',
    '<div class="cost-bar-segment living" style="width:', living_percentage, '%" title="Living: £', city_data$living_cost, '"></div>',
    '</div>',
    '<div class="cost-legend">',
    '<span class="legend-item"><span class="dot flight"></span>Flight</span>',
    '<span class="legend-item"><span class="dot hotel"></span>Hotel</span>',
    '<span class="legend-item"><span class="dot living"></span>Living</span>',
    '</div>',
    '</div>',
    '<div class="popup-price">',
    '<div class="popup-price-label">Total Estimate</div>',
    '<div class="popup-price-value">£', city_data$total_cost, '</div>',
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
    '<h4>Total: £', city_data$total_cost, '</h4>',
    '</div>'
  )
}

make_sidebar_content <- function(city_data, trip_duration, departure_code) {
  # 1. Handle empty data
  if (nrow(city_data) == 0) return(tags$div())

  # 2. Calculate Percentages
  flight_pct <- round((city_data$flight_cost / city_data$total_cost) * 100, 1)
  hotel_pct  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 1)
  living_pct <- round((city_data$living_cost / city_data$total_cost) * 100, 1)

  # Scale relative to the largest bar (so the biggest cost fills the width)
  row_max <- pmax(flight_pct, hotel_pct, living_pct)
  
  flight_scaled <- round((flight_pct / row_max) * 100, 1)
  hotel_scaled  <- round((hotel_pct  / row_max) * 100, 1)
  living_scaled <- round((living_pct / row_max) * 100, 1)

  # --- Helper to DRY (Don't Repeat Yourself) the row creation ---
  cost_row <- function(label, type_class, width_val, cost) {
    div(class = "breakdown-row",
      div(class = "segment-type", label),
      div(class = "breakdown-container",
        div(class = paste("cost-bar-segment", type_class), 
            style = paste0("width: ", width_val, "%"))
      ),
      div(class = "segment-price", paste0("£", cost))
    )
  }

  # 3. Build UI Programmatically
  tagList(
    # Top Image
    tags$img(
      src = paste0("statbar_images/", city_data$city, ".png"),
      class = "statbar-image",
      onerror = "this.style.display='none'"
    ),

    # Main Container
    div(class = "container",
        div(class = "contents",
            # Header
            div( class = "sticky-header",
                div(class = "statbar-header",
                    h1(city_data$city),
                    div(class = "total-price", paste0("£", city_data$total_cost))
                ),

                # Info Sub-header
                div(class = "statbar-info",
                    h2(city_data$country),
                    div(class = "duration", paste0("(", trip_duration, " days)"))
                )
            ),

            # Cost Breakdown Rows (using the helper above)
            cost_row("Flight", "flight", flight_scaled, city_data$flight_cost),
            cost_row("Hotel",  "hotel",  hotel_scaled,  city_data$hotel_cost),
            cost_row("Living", "living", living_scaled, city_data$living_cost),

            # tags$h4("Return Price Trends", 
            #         style = "margin: 15px 15px 5px 15px; font-size: 0.8rem; color: #888; text-transform: uppercase; letter-spacing: 0.5px;"),
            plotlyOutput("return_plot", height = "200px"),

            # tags$h4("Departure Price Trends", 
            #         style = "margin: 15px 15px 5px 15px; font-size: 0.8rem; color: #888; text-transform: uppercase; letter-spacing: 0.5px;"),
            plotlyOutput("departure_plot", height = "200px")
        )
    )
  )
}

# make_sidebar_content <- function(city_data, trip_duration, departure_code) {
#     if(nrow(city_data) == 0) return("")

#     flight_percentage <- round((city_data$flight_cost / city_data$total_cost) * 100, 1)
#     hotel_percentage  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 1)
#     living_percentage <- round((city_data$living_cost / city_data$total_cost) * 100, 1)

#     row_max <- pmax(flight_percentage, hotel_percentage, living_percentage)

#     flight_scaled <- round((flight_percentage / row_max) * 100, 1)
#     hotel_scaled  <- round((hotel_percentage  / row_max) * 100, 1)
#     living_scaled <- round((living_percentage / row_max) * 100, 1)

#     paste0(
#         '<img src="statbar_images/', city_data$city, '.png" class="statbar-image" onerror="this.style.display=\'none\'"/>',

#         '<div class="container">',

#             '<div class="statbar-header">',
#             '<h1>', city_data$city, '</h1>',
#             '<div class="total-price">£', city_data$total_cost, '</div>',
#             '</div>',

#             '<div class="statbar-info">',
#             '<h2>', city_data$country, '</h2>',
#             '<div class="duration">(', trip_duration, ' days)</div>',
#             '</div>',

#             '<div class="breakdown-row">',
#                 '<div class="segment-type">Flight</div>',
#             '<div class="breakdown-container">',
#                 '<div class="cost-bar-segment flight" style="width:', flight_scaled, '%"></div>',
#             '</div>',
#             '<div class="segment-price">£', city_data$flight_cost, '</div>',
#             '</div>',

#             '<div class="breakdown-row">',
#             '<div class="segment-type">Hotel</div>',
#             '<div class="breakdown-container">',
#                 '<div class="cost-bar-segment hotel" style="width:', hotel_scaled, '%"></div>',
#             '</div>',
#             '<div class="segment-price">£', city_data$hotel_cost, '</div>',
#             '</div>',

#             '<div class="breakdown-row">',
#             '<div class="segment-type">Living</div>',
#             '<div class="breakdown-container">',
#                 '<div class="cost-bar-segment living" style="width:', living_scaled, '%"></div>',
#             '</div>',
#             '<div class="segment-price">£', city_data$living_cost, '</div>',
#             '</div>',

#         '</div>'
#     )
# }