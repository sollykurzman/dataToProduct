validate_dates <- function(start, end) {
    end > start
}

get_departure_info <- function(airport_code, departures_df) {
    departures_df %>% filter(code == airport_code)
}

get_other_airports <- function(airport_code, airports_df) {
    airports_df %>% 
        filter(code != airport_code)
}



get_trip_costs <- function(departure_code, arrivals_df, trip_duration) {

    # Placeholder price generator
    dummy_cost_function <- function(dep, arr_df) {
        runif(nrow(arr_df), min = 10, max = 200) |> round()
    }

    message("--- Structure of arrivals_df ---")
    str(arrivals_df) # Prints the structure (type, length, preview)
    # print(flight_price_vector) # Uncomment this if you want to see all values
    message("--------------------------------------")
  
    flight_price_vector <- dummy_cost_function(departure_code, arrivals_df)
    hotel_price_vector <- dummy_cost_function(departure_code, arrivals_df)
    living_cost_vector <- dummy_cost_function(departure_code, arrivals_df)

    message("--- Structure of flight_price_vector ---")
    str(flight_price_vector) # Prints the structure (type, length, preview)
    # print(flight_price_vector) # Uncomment this if you want to see all values
    message("--------------------------------------")

    arrivals_df$flight_cost <- flight_price_vector*trip_duration
    arrivals_df$hotel_cost <- hotel_price_vector*trip_duration
    arrivals_df$living_cost <- living_cost_vector*trip_duration
    arrivals_df$total_cost <- flight_price_vector + hotel_price_vector + living_cost_vector

    message("--- Structure of arrivals_df now ---")
    str(arrivals_df) # Prints the structure (type, length, preview)
    # print(flight_price_vector) # Uncomment this if you want to see all values
    message("--------------------------------------")
  
    return(arrivals_df)
}

get_trip_duration <- function(start, end) {
  as.numeric(difftime(end, start, units = "days"))
}

filter_cities_by_budget <- function(budget_range, cities_df) {
  cities_df %>%
    filter(total_cost >= budget_range[1], total_cost <= budget_range[2])
}

make_destination_popup <- function(city_data, trip_duration, departure_code) {
    pct_flight <- round((city_data$flight_cost / city_data$total_cost) * 100, 1)
  pct_hotel  <- round((city_data$hotel_cost / city_data$total_cost) * 100, 1)
  pct_living <- round((city_data$living_cost / city_data$total_cost) * 100, 1)
  paste0(
    '<div>',
        '<div class="popup-header">', city_data$city, '</div>',
        '<div class="popup-body">',
            # -- Info Rows --
            '<div class="popup-row">',
                '<span class="popup-label">Country</span>',
                '<span class="popup-value">', city_data$country, '</span>',
            '</div>',
            '<div class="popup-row">',
                '<span class="popup-label">Duration</span>',
                '<span class="popup-value">', trip_duration, ' days</span>',
            '</div>',
            
            # -- The Cost Visualization --
            '<div class="breakdown-section">',
                '<div class="breakdown-title">Cost Breakdown</div>',
                
                # The Stacked Bar
                '<div class="cost-bar-container">',
                    '<div class="cost-bar-segment flight" style="width:', pct_flight, '%" title="Flight: £', city_data$flight_cost, '"></div>',
                    '<div class="cost-bar-segment hotel" style="width:', pct_hotel, '%" title="Hotel: £', city_data$hotel_cost, '"></div>',
                    '<div class="cost-bar-segment living" style="width:', pct_living, '%" title="Living: £', city_data$living_cost, '"></div>',
                '</div>',
                
                # The Legend
                '<div class="cost-legend">',
                    '<span class="legend-item"><span class="dot flight"></span>Flight</span>',
                    '<span class="legend-item"><span class="dot hotel"></span>Hotel</span>',
                    '<span class="legend-item"><span class="dot living"></span>Living</span>',
                '</div>',
            '</div>',

            # -- Total Price --
            '<div class="popup-price">',
                '<div class="popup-price-label">Total Estimate</div>',
                '<div class="popup-price-value">£', city_data$total_cost, '</div>',
            '</div>',
        '</div>',
    '</div>'
  )
}
