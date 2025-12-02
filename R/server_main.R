library(shiny)
library(leaflet)
library(lubridate)

server_main <- function(input, output, session) {

  search_triggered <- reactiveVal(FALSE)

  airport_choices <- setNames(
    departure_airports$code,
    paste0(departure_airports$city)
  )

  updateSelectizeInput(session, "departure_airport",
    choices = airport_choices,
    selected = character(0),
    options = list(placeholder = "Type to search...")
  )

  updateDateInput(session, "return_date",
    value = Sys.Date() + 14,
    min = Sys.Date()
  )

  updateDateInput(session, "departure_date",
    value = Sys.Date() + 7,
    min = Sys.Date()
  )

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 40, zoom = 3)
  })

  observeEvent(input$departure_date, {
    new_min_return <- input$departure_date %m+% days(1)
    current_return <- input$return_date

    new_value <- if (current_return <= input$departure_date) new_min_return else current_return

    updateDateInput(session, "return_date",
      min = new_min_return,
      value = new_value
    )
  }, ignoreInit = TRUE)

  observeEvent(input$sidebar_clicked, {
    print("The sidebar icon was clicked!")
    # all_lons <- c(departure_info$lon, filtered_cities$lon)
    # all_lats <- c(departure_info$lat, filtered_cities$lat)
    # map %>%
    #   fitBounds(
    #     lng1 = min(all_lons), lat1 = min(all_lats),
    #     lng2 = max(all_lons), lat2 = max(all_lats)
    #   )
  })

  observeEvent(input$search_btn, {
    
    if (is.null(input$departure_airport) || input$departure_airport == "") {
      showNotification("Please select a departure airport.", type = "error")
      return()
    }

    search_triggered(TRUE)

    departure_info <- get_departure_info(input$departure_airport, departure_airports)
    connections    <- get_other_airports(input$departure_airport, departure_airports)
    trip_duration  <- get_trip_duration(input$departure_date, input$return_date)
    trip_costs     <- get_trip_costs(input$departure_airport, connections, trip_duration)
    
    filtered_cities <- filter_cities_by_budget(input$budget, trip_costs)

    all_lons <- c(departure_info$lon, filtered_cities$lon)
    all_lats <- c(departure_info$lat, filtered_cities$lat)

    departure_icon   <- HTML('<div class="departure_icon"></div>')
    destination_icon <- HTML('<div class="destination_icon"></div>')

    map <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()

    map %>%
      addLabelOnlyMarkers(
        lng = departure_info$lon, lat = departure_info$lat,
        label = departure_icon,
        labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
      ) %>%
      addCircleMarkers(
        data = departure_info,
        lng = ~lon, lat = ~lat,
        radius = 10,
        fillOpacity = 0, opacity = 0,
        label = ~city
      )

    # E. Stop if no results
    if (nrow(filtered_cities) == 0) {
      showNotification("No destinations found within your budget.", type = "warning")
      return()
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

    paths_sf <- sf::st_multilinestring(path_matrices) %>%
      sf::st_sfc(crs = 4326)

    popup_content <- make_destination_popup(
      city_data = filtered_cities,
      trip_duration = trip_duration,
      departure_code = departure_info$code
    )

    icon_list <- lapply(seq_len(nrow(filtered_cities)), function(x) destination_icon)

    map %>%
      addPolylines(
        data = paths_sf,
        color = "#667eea",
        weight = 4,
        opacity = 0.8
      ) %>%
      addLabelOnlyMarkers(
        data = filtered_cities,
        lng = ~lon, lat = ~lat,
        label = icon_list,
        labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)
      ) %>%
      addCircleMarkers(
        data = filtered_cities,
        lng = ~lon, lat = ~lat,
        radius = 10,
        fillOpacity = 0, opacity = 0,
        popup = popup_content,
        label = ~city
      )

    map %>%
      fitBounds(
        lng1 = min(all_lons), lat1 = min(all_lats),
        lng2 = max(all_lons), lat2 = max(all_lats)
      )
  })
}