library(shiny)
library(leaflet)
library(lubridate)

server_main <- function(input, output, session) {
  
search_triggered <- reactiveVal(FALSE)

    airport_choices <- setNames(
        departure_airports$code, 
        paste0(departure_airports$city)
    )

    updateSelectizeInput(
        session,
        "departure_airport",
        choices = airport_choices,
        selected = character(0),
        options = list(
            placeholder = "Type to search..."
        )
    )

    updateDateInput(
        session,
        "return_date",
        value = Sys.Date() + 14,
        min = Sys.Date()
    )

    updateDateInput(
        session,
        "departure_date",
        value = Sys.Date() + 7,
        min = Sys.Date()
    )
    
    output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% #addTiles() %>%
      setView(lng = 0, lat = 40, zoom = 3)
    })
    
    observeEvent(input$departure_date, {
    new_min_return <- input$departure_date %m+% days(1)
    
    # Only update the actual VALUE if the current return date is invalid
    current_return <- input$return_date
    new_value <- if(current_return <= input$departure_date) new_min_return else current_return
    
    updateDateInput(
      session,
      "return_date",
      min = new_min_return,
      value = new_value 
    )
  }, ignoreInit = TRUE)

  observeEvent(input$search_btn, {
    if (is.null(input$departure_airport) || input$departure_airport == "") {
        showNotification("Please select a departure airport.", type = "error")
        return()
    }

    search_triggered(TRUE)
    
    departure_info <- get_departure_info(input$departure_airport, departure_airports)
    connections <- get_other_airports(input$departure_airport, departure_airports)
    
    # FIXED: Logic to include duration (optional, depends if you update function)
    trip_duration <- get_trip_duration(input$departure_date, input$return_date)
    trip_costs <- get_trip_costs(input$departure_airport, connections, trip_duration) 
    
    # FIXED: Ensure column names match. 
    # Assuming trip_costs has 'total_cost', we map it here or fix the function.
    # Let's assume you fixed the function to create a 'price' column.
    filtered_cities <- filter_cities_by_budget(input$budget, trip_costs)
    
    destination_icon <- makeIcon(
            iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-violet.png",
            iconWidth = 25, iconHeight = 41, iconAnchorX = 12, iconAnchorY = 41
        )

        departure_icon <- makeIcon(
            iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
            iconWidth = 30, iconHeight = 49, iconAnchorX = 15, iconAnchorY = 49
        )
    
    map <- leafletProxy("map") %>% 
        clearMarkers() %>% 
        clearShapes() %>%
        addMarkers(
            lng = departure_info$lon, lat = departure_info$lat,
            icon = departure_icon,
            label = paste0("🛫 ", departure_info$city)
        )

    # 3. FALLBACK CHECK
    if (nrow(filtered_cities) == 0) {
        showNotification("No destinations found within your budget.", type = "warning")
        return() # Stop execution here
    }

    path_matrices <- lapply(seq_len(nrow(filtered_cities)), function(i) {
        city <- filtered_cities[i, ]
        df <- create_curved_path(
            departure_info$lon, departure_info$lat,
            city$lon, city$lat,
            num_points = 50
        )
        as.matrix(df[, c("lon", "lat")])
    })

    popup_content <- make_destination_popup(
        city_data = filtered_cities, 
        trip_duration = trip_duration, 
        departure_code = departure_info$code
    )

    # Convert list of matrices to an sf MULTILINESTRING
    # This creates one single spatial object containing all lines
    paths_sf <- sf::st_multilinestring(path_matrices) %>% 
                sf::st_sfc(crs = 4326)

    # 5. VECTORIZED PLOTTING
    map %>%
        addPolylines(
            data = paths_sf, # Now 'data' works because it's an sf object
            color = "#667eea", 
            weight = 4, 
            opacity = 0.8
        ) %>%
        addMarkers(
            data = filtered_cities,
            lng = ~lon, 
            lat = ~lat,
            icon = destination_icon,
            label = ~city,
            popup = popup_content
        )
    # map <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()
    
    # # Add departure marker
    # map <- map %>%
    #   addMarkers(
    #     lng = departure_info$lon, lat = departure_info$lat,
    #     icon = departure_icon,
    #     label = paste0("🛫 ", departure_info$city)
    #   )
    
    # # FIXED: Vectorized Marker Addition (No Loop)
    # if (nrow(filtered_cities) > 0) {
    #   map %>%
    #     addMarkers(
    #       data = filtered_cities,
    #       lng = ~lon, 
    #       lat = ~lat,
    #       icon = destination_icon,
    #       label = ~city # Assumes 'city' column exists
    #     )
    # } else {
    #     showNotification("No cities found within budget", type = "warning")
    # }
  })
}

# validate_dates <- function(start, end) {
#   end > start
# }

# server_main <- function(input, output, session) {

#     search_triggered <- reactiveVal(FALSE)

#     airport_choices <- setNames(
#         departure_airports$code, 
#         paste0(departure_airports$city)
#     )

#     updateSelectizeInput(
#         session,
#         "departure_airport",
#         choices = airport_choices,
#         selected = character(0),
#         options = list(
#             placeholder = "Type to search..."
#         )
#     )

#     updateDateInput(
#         session,
#         "return_date",
#         value = Sys.Date() + 14,
#         min = Sys.Date()
#     )

#     updateDateInput(
#         session,
#         "departure_date",
#         value = Sys.Date() + 7,
#         min = Sys.Date()
#     )
    
#     output$map <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$CartoDB.Positron) %>% #addTiles() %>%
#       setView(lng = 0, lat = 40, zoom = 3)
#     })

#     observeEvent(input$departure_date, {
#         new_return_date <- input$departure_date %m+% days(1)
#         if (!validate_dates(input$departure_date, input$return_date)) {
#             showNotification("Return date was automatically updated", type = "error")
#         }

#         updateDateInput(
#             session,
#             "return_date",
#             min = new_return_date,
#             value = new_return_date
#         )
#     }, ignoreInit = TRUE)

#     observeEvent(input$search_btn, {
#         search_triggered(TRUE)

#         departure_info <- get_departure_info(input$departure_airport, departure_airports)

#         connections <- get_other_airports(input$departure_airport, departure_airports)

#         trip_costs <- get_trip_costs(input$departure_airport, connections)

#         trip_duration <- get_trip_duration(input$departure_date, input$return_date)

#         filtered_cities <- filter_cities_by_budget(input$budget, trip_costs)

#         destination_icon <- makeIcon(
#             iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-violet.png",
#             iconWidth = 25, iconHeight = 41, iconAnchorX = 12, iconAnchorY = 41
#         )

#         departure_icon <- makeIcon(
#             iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
#             iconWidth = 30, iconHeight = 49, iconAnchorX = 15, iconAnchorY = 49
#         )

    #     map <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()

    #     # Add departure marker
    #     map <- map %>%
    #     addMarkers(
    #         lng = departure_info$lon, lat = departure_info$lat,
    #         icon = departure_icon,
    #         label = paste0("🛫 ", departure_info$city)
    #     )

    #     for (i in 1:nrow(filtered_cities)) {

    #         city <- filtered_cities[i, ]

    #         path <- create_curved_path(
    #             departure_info$lon, departure_info$lat,
    #             city$lon, city$lat,
    #             num_points = 100
    #         )

    #         map <- map %>%
    #             addPolylines(lng = path$lon, lat = path$lat,
    #                         color="#667eea", weight=4, opacity=0.8)

    #         # popup <- make_destination_popup(city, trip_duration, departure_info$code)

    #         map <- map %>%
    #             addMarkers(
    #             lng = city$lon, lat = city$lat,
    #             icon = destination_icon,
    #             # popup = popup,
    #             label = city$city
    #         )
    #     }
    # })

    
# }