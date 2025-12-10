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
  # updateSelectizeInput(session, "departure_airport",
  #   choices = airport_choices,
  #   selected = character(0),
  #   options = list(placeholder = "Type to search...")

  #ON DEPLOYMENT REMOVE FROM HERE 
  updateSelectizeInput(session, "departure_airport",
    choices = airport_choices,
    selected = airport_choices[1],
    options = list(placeholder = "Type to search...")
  )

  shinyjs::delay(300, {
      # Optional: Update budget manually if you want a specific test case
      updateNumericInput(session, "budget_min", value = 0)
      updateNumericInput(session, "budget_max", value = 1500)
      
      # Simulate the click
      shinyjs::click("search_btn") 
  })
  #TO HERE

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

  # --- GRAPH INITIALIZATION (The Empty Canvas) ---
  
  # Define a helper for the empty layout so we don't repeat code
  empty_plot_layout <- function(p) {
    p %>% plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
      xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
      yaxis = list(title = "", fixedrange = TRUE, showgrid = TRUE, gridcolor = "#f7f7f7", zeroline = FALSE, tickprefix = "£"),
      margin = list(l = 35, r = 0, t = 10, b = 20)
    ) %>%
    plotly::config(displayModeBar = FALSE)
  }

  output$return_plot <- plotly::renderPlotly({
    # Create an empty plot with just the layout
    plotly::plot_ly(x = c(), y = c(), type = "bar") %>% empty_plot_layout()
  })

  output$departure_plot <- plotly::renderPlotly({
    plotly::plot_ly(x = c(), y = c(), type = "bar") %>% empty_plot_layout()
  })

  # --- 3. UI Interactions (Statbar) ---

  open_statbar_for <- function(city_row) {
    selected_destination(city_row)
    shinyjs::removeClass(id = "statbar", class = "closed")
    shinyjs::addClass(id = "map_container", class = "statbar")
  }

  observeEvent(input$map_marker_click, {
    req(rv$destinations)
    clicked_id <- input$map_marker_click$id
    req(clicked_id)
    
    # Find the city data for the clicked ID
    city_row <- rv$destinations[rv$destinations$city == clicked_id, , drop = FALSE]
    
    # If found, open the statbar
    if (nrow(city_row) == 1) open_statbar_for(city_row)
  })

  # --- MISSING PART 2: Handle Sidebar Item Click ---
  # (This relies on the Javascript you added in ui_main.R)
  observeEvent(input$sidebar_item_clicked, {
    req(rv$destinations)

    city_row <- rv$destinations[rv$destinations$city == input$sidebar_item_clicked, , drop = FALSE]
    if (nrow(city_row) == 1) open_statbar_for(city_row)
  })

  # --- STATBAR UPDATE: Handle City Selection ---
  observeEvent(selected_destination(), {
    req(selected_destination())
    city_data <- selected_destination()

    # 1. Calculate Percentages
    total <- city_data$total_cost
    f_pct <- round((city_data$flight_cost / total) * 100, 1)
    h_pct <- round((city_data$hotel_cost / total) * 100, 1)
    l_pct <- round((city_data$living_cost / total) * 100, 1)
    
    row_max <- max(c(f_pct, h_pct, l_pct), na.rm = TRUE)
    if(row_max == 0) row_max <- 1
    
    f_scaled <- round((f_pct / row_max) * 100, 1)
    h_scaled <- round((h_pct / row_max) * 100, 1)
    l_scaled <- round((l_pct / row_max) * 100, 1)

    # 2. Send ONLY the HTML data to JS
    session$sendCustomMessage("update_statbar", list(
      city = city_data$city,
      country = city_data$country,
      total_cost = city_data$total_cost,
      duration = rv$trip_duration,
      flight_cost = city_data$flight_cost,
      hotel_cost = city_data$hotel_cost,
      living_cost = city_data$living_cost,
      flight_pct = f_scaled*0.9,
      hotel_pct = h_scaled*0.9,
      living_pct = l_scaled*0.9
    ))
    
    # NOTE: We do NOT touch the graphs here.
  })

  output$return_plot <- plotly::renderPlotly({
    req(selected_destination())
    city_data <- selected_destination()
    
    prices <- get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
    
    # Highlight logic
    bar_colors <- rep("#D3E4FA", length(prices$return_prices)) 
    bar_colors[which.min(prices$return_prices)] <- "#5B97EC"
    
    plotly::plot_ly(
      x = format(as.Date(prices$return_window), "%d %b"),
      y = prices$return_prices,
      type = "bar",
      hovertemplate = "<b>%{x}</b><br>Diff: %{y}<extra></extra>",
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

  output$departure_plot <- plotly::renderPlotly({
    req(selected_destination())
    city_data <- selected_destination()
    
    prices <- get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
    
    bar_colors <- rep("#D3E4FA", length(prices$departure_prices)) 
    bar_colors[which.min(prices$departure_prices)] <- "#5B97EC"
    
    plotly::plot_ly(
      x = format(as.Date(prices$departure_window), "%d %b"),
      y = prices$departure_prices,
      type = "bar",
      hovertemplate = "<b>%{x}</b><br>Diff: %{y}<extra></extra>",
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

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 40, zoom = 3)
  })

  # --- 1. Create a "Debounced" Version of the Inputs ---
  # This waits 500 milliseconds (half a second) after the last keystroke 
  # before notifying the rest of the app.
  debounced_budget_inputs <- reactive({
    list(min = input$budget_min, max = input$budget_max)
  }) %>% debounce(500)

  # --- Sync: Text Inputs Control Slider Handles & Range ---
  observeEvent(debounced_budget_inputs(), {
    
    # 1. Get values from the debounced text inputs
    d_min <- debounced_budget_inputs()$min
    d_max <- debounced_budget_inputs()$max
    
    # 2. Basic Checks
    req(d_min, d_max)
    if (d_min < 0 || d_max < 0) return()
    
    # 3. Handle Logical Errors (Auto-correct if Min > Max)
    if (d_min >= d_max) d_max <- d_min + 100
    
    # 4. Determine the Slider Track (Background Scale)
    # The track should always be at least as big as the user's input,
    # but never smaller than 2000 (to keep the UI looking consistent).
    new_track_min <- 0  # Ideally, keep the track start at 0
    new_track_max <- max(d_max, 2000) 
    
    # 5. Update the Slider
    # - min/max: Sets the "Playing Field"
    # - value:   Moves the "Handles" to exactly what you typed
    updateSliderInput(session, "budget", 
                      min = new_track_min, 
                      max = new_track_max,
                      value = c(d_min, d_max)) 
  })

  observeEvent(input$search_btn, {
    
    # A. Validate Input
    if (is.null(input$departure_airport) || input$departure_airport == "") {
      showNotification("Please select a departure airport.", type = "error")
      return()
    }

    # user_budget <- input$budget
    # if (is.na(user_budget[1])) user_budget[1] <- 0
    # if (is.na(user_budget[2])) user_budget[2] <- 1000 

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

    shinyjs::addClass(id = "statbar", class = "closed")
    shinyjs::removeClass(id = "map_container", class = "statbar")

    # D. Prepare Map Visuals
    map <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()

    # Draw Departure City
    dep_icon <- HTML('<div class="departure_icon"></div>')
    map %>%
      addLabelOnlyMarkers(lng = dep_info$lon, lat = dep_info$lat, label = dep_icon,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>%
      addCircleMarkers(data = dep_info, lng = ~lon, lat = ~lat, radius = 10, fillOpacity = 0, opacity = 0, label = ~city)

    # E. Handle Results
    if (nrow(filtered_data) > 0) {
      
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
