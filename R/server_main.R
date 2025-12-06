library(shiny)
library(leaflet)
library(lubridate)
library(htmltools)
library(sf)
library(shinyjs)
library(plotly)

# Source logic if not using global.R
# source("R/utils_logic.R") 

server_main <- function(input, output, session) {

  # --- 1. State Management ---
  rv <- reactiveValues(
    destinations = NULL,
    trip_duration = NULL,
    departure_code = NULL,
    return_date = NULL,
    departure_date = NULL
  )

  selected_destination <- reactiveVal(NULL)
  search_triggered <- reactiveVal(FALSE)

  # --- 2. Input Initialization ---
  
  # Populate Airport Dropdown
  airport_choices <- setNames(departure_airports$code, departure_airports$city)
  updateSelectizeInput(session, "departure_airport",
    choices = airport_choices,
    selected = character(0),
    options = list(placeholder = "Type to search...")
  )

  # Initialize Dates
  updateDateInput(session, "return_date", value = Sys.Date() + 14, min = Sys.Date())
  updateDateInput(session, "departure_date", value = Sys.Date() + 7, min = Sys.Date())

  # Ensure Return Date > Departure Date
  observeEvent(input$departure_date, {
    new_min_return <- input$departure_date %m+% days(1)
    current_return <- input$return_date
    new_value <- if (current_return <= input$departure_date) new_min_return else current_return
    updateDateInput(session, "return_date", min = new_min_return, value = new_value)
  }, ignoreInit = TRUE)

  # --- 3. UI Interactions (Statbar) ---

  open_statbar_for <- function(city_row) {
    selected_destination(city_row)
    shinyjs::removeClass(id = "statbar", class = "closed")
    shinyjs::addClass(id = "map_container", class = "statbar")
  }

  # Handle Map Marker Click
  observeEvent(input$map_marker_click, {
    req(rv$destinations)
    clicked_id <- input$map_marker_click$id
    req(clicked_id)
    
    city_row <- rv$destinations[rv$destinations$city == clicked_id, , drop = FALSE]
    if (nrow(city_row) == 1) open_statbar_for(city_row)
  })

  # Handle Sidebar Item Click (Requires JS listener in UI)
  observeEvent(input$sidebar_item_clicked, {
    req(rv$destinations)

    city_row <- rv$destinations[rv$destinations$city == input$sidebar_item_clicked, , drop = FALSE]
    if (nrow(city_row) == 1) open_statbar_for(city_row)
  })

  observeEvent(selected_destination(), {
    req(selected_destination())

    city <- selected_destination()
    departure_date <- rv$departure_date
    return_date <- rv$return_date

    message("Updating return plot for city: ", city)

    prices = get_prices_around(rv$departure_code, city, departure_date, return_date)

    return_days <- format(as.Date(prices$return_window), "%d")
    departure_days <- format(as.Date(prices$departure_window), "%d")

    # output$return_plot <- plotly::renderPlotly({
    #   plotly::plot_ly(
    #     x = return_days,
    #     y = prices$return_prices,
    #     type = "bar"
    #   )
    # })
    output$return_plot <- plotly::renderPlotly({
      
      # Color Logic
      bar_colors <- rep("#D3E4FA", length(prices$return_prices)) 
      min_index <- which.min(prices$return_prices)
      bar_colors[min_index] <- "#5B97EC"
      
      plotly::plot_ly(
        x = return_days,
        y = prices$return_prices,
        type = "bar",
        hovertemplate = "<b>%{x}</b><br>Price: £%{y}<extra></extra>",
        marker = list(color = bar_colors, line = list(width = 0), cornerradius = 5)
      ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
          xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(title = "", fixedrange = TRUE, showgrid = TRUE, gridcolor = "#f7f7f7", zeroline = FALSE, tickprefix = "£"),
          margin = list(l = 35, r = 0, t = 10, b = 20)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # output$departure_plot <- plotly::renderPlotly({
    #   plotly::plot_ly(
    #     x = departure_days,
    #     y = prices$departure_prices,
    #     type = "bar"
    #   )
    # })
    output$departure_plot <- plotly::renderPlotly({
      
      # Color Logic
      bar_colors <- rep("#D3E4FA", length(prices$departure_prices)) 
      min_index <- which.min(prices$departure_prices)
      bar_colors[min_index] <- "#5B97EC"
      
      plotly::plot_ly(
        x = departure_days,
        y = prices$departure_prices,
        type = "bar",
        hovertemplate = "<b>%{x}</b><br>Price: £%{y}<extra></extra>",
        marker = list(color = bar_colors, line = list(width = 0), cornerradius = 5)
      ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
          xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
          yaxis = list(title = "", fixedrange = TRUE, showgrid = TRUE, gridcolor = "#f7f7f7", zeroline = FALSE, tickprefix = "£"),
          margin = list(l = 35, r = 0, t = 10, b = 20)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
  })

  # --- 4. Map & Search Logic ---

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 40, zoom = 3)
  })

  observeEvent(input$search_btn, {
    
    # A. Validate Input
    if (is.null(input$departure_airport) || input$departure_airport == "") {
      showNotification("Please select a departure airport.", type = "error")
      return()
    }

    search_triggered(TRUE)

    # B. Perform Calculations (Delegated to utils_logic.R)
    dep_info      <- get_departure_info(input$departure_airport, departure_airports)
    connections   <- get_other_airports(input$departure_airport, departure_airports)
    duration      <- get_trip_duration(input$departure_date, input$return_date)
    costs         <- get_trip_costs(input$departure_airport, connections, input$departure_date, input$return_date)
    filtered_data <- filter_cities_by_budget(input$budget, costs)

    # C. Update Global State
    rv$destinations   <- filtered_data
    rv$trip_duration  <- duration
    rv$departure_code <- dep_info$code
    rv$departure_date <- input$departure_date
    rv$return_date    <- input$return_date
    selected_destination(NULL) # Reset selection on new search

    # D. Prepare Map Visuals
    map <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()

    # Draw Departure City
    dep_icon <- HTML('<div class="departure_icon"></div>')
    map %>%
      addLabelOnlyMarkers(lng = dep_info$lon, lat = dep_info$lat, label = dep_icon,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>%
      addCircleMarkers(data = dep_info, lng = ~lon, lat = ~lat, radius = 10, fillOpacity = 0, opacity = 0, label = ~city)

    # E. Handle Results
    if (nrow(filtered_data) == 0) {
      showNotification("No destinations found within your budget.", type = "warning")
      # Sidebar UI handles the "No Data" visual update automatically via renderUI below
    } else {
      
      # Generate Curved Paths
      # We loop here because sf creation involves lists, but the heavy math is in utils
      path_matrices <- lapply(seq_len(nrow(filtered_data)), function(i) {
        city <- filtered_data[i, ]
        df <- create_curved_path(dep_info$lon, dep_info$lat, city$lon, city$lat, num_points = 50)
        as.matrix(df[, c("lon", "lat")])
      })
      
      paths_sf <- sf::st_multilinestring(path_matrices) %>% sf::st_sfc(crs = 4326)

      # Generate Popups
      popups <- make_destination_popup(filtered_data, rv$trip_duration, dep_info$code)
      dest_icon <- HTML('<div class="destination_icon"></div>')
      icon_list <- lapply(seq_len(nrow(filtered_data)), function(x) dest_icon)

      # Draw Flights and Destinations
      map %>%
        addPolylines(data = paths_sf, color = "#667eea", weight = 4, opacity = 0.8,
                     dashArray = "50000", options = pathOptions(className = "flight-path-anim", dashOffset = "50000")) %>%
        addLabelOnlyMarkers(data = filtered_data, lng = ~lon, lat = ~lat, label = icon_list,
                            labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>%
        addCircleMarkers(data = filtered_data, lng = ~lon, lat = ~lat, radius = 10,
                         fillOpacity = 0, opacity = 0, label = lapply(popups, HTML), layerId = ~city,
                         labelOptions = labelOptions(noHide = FALSE, direction = "auto", sticky = TRUE,
                                                     style = list("border-radius" = "10px", "width" = "200px"))) %>%
        fitBounds(lng1 = min(c(dep_info$lon, filtered_data$lon)), lat1 = min(c(dep_info$lat, filtered_data$lat)),
                  lng2 = max(c(dep_info$lon, filtered_data$lon)), lat2 = max(c(dep_info$lat, filtered_data$lat)),
                  options = list(padding = c(20, 20), animate = TRUE, duration = 1.0))
    }

    # Open Sidebar
    shinyjs::removeClass(id = "sidebar", class = "closed")
    shinyjs::addClass(id = "map_container", class = "sidebar")

    # Trigger CSS Animation
    shinyjs::delay(100, shinyjs::runjs("
      document.querySelectorAll('.flight-path-anim').forEach(function(path) {
        path.style.animation = 'none'; path.offsetHeight; 
        path.style.animation = 'drawLine 2.5s linear forwards';
      });
    "))
  })

  # --- 5. Sidebar Result Renderer ---
  output$sidebar_results <- renderUI({
    # If no data, show empty state
    if (is.null(rv$destinations) || nrow(rv$destinations) == 0) {
      return(tags$div(class="no-results",
        tags$h4("No destinations found"),
        tags$p("Try adjusting your filters.")
      ))
    }

    sorted_data <- rv$destinations[order(rv$destinations$total_cost, rv$destinations$city), ]
    
    # Use utility function to generate HTML
    html_content <- make_destination_box(sorted_data, rv$trip_duration, rv$departure_code)
    HTML(html_content)
  })

  # Render Statbar Details
  # output$statbar_info <- renderUI({
  #   city <- selected_destination()
  #   req(city)

  #   # message("Selected city: ", city)
    
  #   html_content <- make_sidebar_content(city, rv$trip_duration, rv$departure_code)
  #   HTML(html_content)
  # })
  output$statbar_info <- renderUI({
    city <- selected_destination()
    req(city)
    
    make_sidebar_content(city, rv$trip_duration, rv$departure_code)
  })
}
