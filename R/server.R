server <- function(input, output, session) {

  # global state stuff
  rv <- reactiveValues(
    destinations = NULL,
    trip_duration = NULL,
    departure_code = NULL,
    return_date = NULL,
    departure_date = NULL
  )

  selected_destination <- reactiveVal(NULL)
  search_triggered <- reactiveVal(FALSE)

  # fill airport list
  airport_choices <- setNames(departure_airports$code, departure_airports$city)
  updateSelectizeInput(session, "departure_airport",
    choices = airport_choices,
    selected = character(0),
    options = list(placeholder = "Enter Departure Airport...")
  )

  # defaults
  updateDateInput(session, "return_date", value = Sys.Date() + 12, min = Sys.Date())
  updateDateInput(session, "departure_date", value = Sys.Date() + 7, min = Sys.Date())

  # Load Benchmarks
  flight_benchmarks <- readRDS("data/flight_thresholds.rds")
  flight_lookup <- split(flight_benchmarks, flight_benchmarks$code)

  hotel_benchmarks <- readRDS("data/hotel_thresholds.rds")
  hotel_lookup <- split(hotel_benchmarks, hotel_benchmarks$code)

  # auto fix return date if user picks impossible range
  observeEvent(input$departure_date, {
    new_min_return <- input$departure_date %m+% days(1)
    current_return <- input$return_date
    new_value <- if (current_return <= input$departure_date) new_min_return else current_return
    updateDateInput(session, "return_date", min = new_min_return, value = new_value)
  }, ignoreInit = TRUE)

  # prevents ugly errors before data loads
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

  # 1. Price Window Data (For Departure & Return Plots)
  price_window_reactive <- reactive({
    req(selected_destination())
    city_data <- selected_destination()
    get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
  })

  # 2. Trends Data (For Seasonality & Weekday Plots)
  season_data_reactive <- reactive({
    req(selected_destination())
    city_data <- selected_destination()
    get_route_trends(rv$departure_code, city_data$code)
  })

  # PLOTS AND DESCRIPTIONS

  # 1. Return Flight Price Comparison
  output$return_plot <- plotly::renderPlotly({
    prices <- price_window_reactive() # <--- Shared Data
    
    # highlight the cheapest bar
    bar_colors <- rep("#D3E4FA", length(prices$return_prices)) 
    bar_colors[which.min(prices$return_prices)] <- "#5B97EC"
    
    hover_text <- sprintf("£%.2f", prices$return_prices)
    
    plotly::plot_ly(
      x = format(as.Date(prices$return_window), "%d %b"),
      y = prices$return_prices,
      text = hover_text,
      type = "bar",
      hovertemplate = "<b>%{x}</b><br>Diff: %{text}<extra></extra>",
      marker = list(color = bar_colors, line = list(width = 0), cornerradius = 5)
    ) %>%
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
        xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
        yaxis = list(
          title = "", fixedrange = TRUE, showgrid = TRUE, 
          gridcolor = "#f7f7f7", zeroline = TRUE, zerolinecolor = "#333333", 
          zerolinewidth = 1.5, tickprefix = "£"
        ),
        margin = list(l = 35, r = 0, t = 10, b = 20)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$return_desc <- renderText({
    prices <- price_window_reactive()
    
    min_price <- min(prices$return_prices)
    min_index <- which.min(prices$return_prices)
    user_price <- prices$return_prices[4] # Index 4 is the middle (selected) date
    
    if (user_price == min_price) {
      return("✅ You have selected the cheapest return date in this range.")
    } else {
      best_date <- format(as.Date(prices$return_window[min_index]), "%A, %d %b")
      save_amt <- user_price - min_price
      return(paste0("💡 Tip: Returning on ", best_date, " would save you £", sprintf("%.2f", save_amt), "."))
    }
  })

  # Departure Flight Price Comparison
  output$departure_plot <- plotly::renderPlotly({
    prices <- price_window_reactive()
    
    bar_colors <- rep("#D3E4FA", length(prices$departure_prices)) 
    bar_colors[which.min(prices$departure_prices)] <- "#5B97EC"
    
    hover_text <- sprintf("£%.2f", prices$departure_prices)
    
    plotly::plot_ly(
      x = format(as.Date(prices$departure_window), "%d %b"),
      y = prices$departure_prices,
      text = hover_text,
      type = "bar",
      hovertemplate = "<b>%{x}</b><br>Diff: %{text}<extra></extra>",
      marker = list(color = bar_colors, line = list(width = 0), cornerradius = 5)
    ) %>%
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
        xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
        yaxis = list(
          title = "", fixedrange = TRUE, showgrid = TRUE, 
          gridcolor = "#f7f7f7", zeroline = TRUE, zerolinecolor = "#333333", 
          zerolinewidth = 1.5, tickprefix = "£"
        ),
        margin = list(l = 35, r = 0, t = 10, b = 20)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$departure_desc <- renderText({
    prices <- price_window_reactive() # <--- Shared Data
    
    min_price <- min(prices$departure_prices)
    min_index <- which.min(prices$departure_prices)
    user_price <- prices$departure_prices[4] 
    
    if (user_price == min_price) {
      return("✅ You have selected the cheapest departure date in this range.")
    } else {
      best_date <- format(as.Date(prices$departure_window[min_index]), "%A, %d %b")
      save_amt <- user_price - min_price
      return(paste0("💡 Tip: Leaving on ", best_date, " would save you £", sprintf("%.2f", save_amt), "."))
    }
  })

  # CDF Plot
  output$cfd_plot <- plotly::renderPlotly({
    city_data <- selected_destination()
    req(city_data)

    cdf_function <- get_flight_cdf(
      origin = rv$departure_code, 
      dest = city_data$code, 
      trip_date = rv$departure_date
    )

    price <- get_flight_prediction(
      origin = rv$departure_code, 
      dest = city_data$code, 
      trip_date = rv$departure_date
    )

    req(is.function(cdf_function), !is.na(price))
    
    sorted_prices <- knots(cdf_function)
    probs <- cdf_function(sorted_prices)
    current_price <- price
    
    x_limit <- as.numeric(quantile(sorted_prices, 0.99))
    x_max <- max(x_limit, current_price * 1.1)

    plotly::plot_ly() %>%
      plotly::add_lines(
        x = sorted_prices,
        y = probs,
        name = "CDF",
        line = list(color = "#888", width = 3),
        hovertemplate = "<b>Price: £%{x:.2f}</b><br>Chance: %{y:.1%}<extra></extra>"
      ) %>%
      plotly::add_segments(
        x = current_price, xend = current_price, 
        y = 0, yend = 1,
        line = list(color = "#5B97EC", width = 2, dash = "dash"),
        name = "Est. Price"
      ) %>%
      plotly::layout(
        title = "",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
        xaxis = list(title = "Predicted Price (£)", showgrid = FALSE, zeroline = FALSE, range = c(0, x_max)),
        yaxis = list(title = "CDF(x)", showgrid = TRUE, gridcolor = "#eee", zeroline = FALSE, range = c(0, 1.05), tickformat = ".0%"),
        margin = list(l = 50, r = 20, t = 10, b = 40),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$cfd_desc <- renderText({
     req(selected_destination())
     # Placeholder text - logic can be added if needed, or left generic
     "This curve shows the probability of different flight prices. The dashed line is your estimated price."
  })

  # 4. Seasonality (Monthly Trends)
  output$season_plot <- plotly::renderPlotly({
    trends <- season_data_reactive() # <--- Shared Data
    data <- trends$seasonality
    
    data$Month <- factor(data$Month, levels = month.abb)
    data <- data[order(data$Month), ]

    plotly::plot_ly(data, x = ~Month, y = ~base_price, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#5B97EC", width = 2),
            marker = list(color = "#5B97EC", size = 6)) %>%
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        xaxis = list(title = "", fixedrange=TRUE),
        yaxis = list(title = "Avg Price", fixedrange=TRUE, tickprefix="£"),
        margin = list(l=35, r=10, t=10, b=30)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$season_desc <- renderText({
    trends <- season_data_reactive() # <--- Shared Data
    data <- trends$seasonality
    
    cheapest_row <- data[which.min(data$base_price), ]
    
    paste0("For this route, the cheapest month to travel is usually ", 
           cheapest_row$Month, 
           " (approx. £", round(cheapest_row$base_price), ").")
  })

  # 5. Weekday Variance
  output$weekday_plot <- plotly::renderPlotly({
    trends <- season_data_reactive() # <--- Shared Data
    data <- trends$weekday
    
    days_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    data <- data[match(data$weekday, days_order), ] 
    data <- na.omit(data) 

    colors <- rep("#D3E4FA", nrow(data))
    if(nrow(data) > 0) {
      colors[which.min(data$day_modifier)] <- "#48bb78" 
      colors[which.max(data$day_modifier)] <- "#e53e3e" 
    }

    plotly::plot_ly(data, x = ~weekday, y = ~day_modifier, type = 'bar',
            marker = list(color = colors)) %>%
      plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        xaxis = list(title = "", fixedrange=TRUE),
        yaxis = list(title = "Price Impact", fixedrange=TRUE, tickprefix="£"),
        margin = list(l=35, r=10, t=10, b=30)
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$weekday_desc <- renderText({
    trends <- season_data_reactive() # <--- Shared Data
    data <- trends$weekday
    
    best_day_row <- data[which.min(data$day_modifier), ]
    
    paste0("For this route, the best day to travel is normally ", 
           best_day_row$weekday, ".")
  })

  # 6. Living Cost Pie Chart
  output$living_breakdown_plot <- plotly::renderPlotly({
    req(selected_destination())
    city_data <- selected_destination()
    
    living_details <- get_total_living_cost(
      shortcode = city_data$code,
      arrival_date = rv$departure_date,
      leaving_date = rv$return_date
    )
    
    bd <- living_details$breakdown
    plot_data <- data.frame(
      Category = c("Food", "Drinks", "Transport", "Activities"),
      Cost = c(bd$food, bd$drink, bd$transport, bd$activities)
    )
    
    plotly::plot_ly(plot_data, labels = ~Category, values = ~Cost, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = c("#4299e1", "#9f7aea", "#48bb78", "#ed8936"), 
                          line = list(color = '#FFFFFF', width = 1))) %>%
      plotly::layout(
        title = "",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
        margin = list(l = 20, r = 20, t = 20, b = 20),
        showlegend = FALSE
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  output$living_desc <- renderText({
    req(selected_destination())
    city_data <- selected_destination()
    
    living_details <- get_total_living_cost(
      shortcode = city_data$code,
      arrival_date = rv$departure_date,
      leaving_date = rv$return_date
    )
    
    bd <- unlist(living_details$breakdown)
    
    # Find most expensive category
    top_cat <- names(bd)[which.max(bd)]
    top_cat_clean <- paste0(toupper(substr(top_cat, 1, 1)), substr(top_cat, 2, nchar(top_cat)))
    
    paste0("Your biggest daily expense in ", city_data$city, " is likely to be ", top_cat_clean, ".")
  })


  # --- CORE APP LOGIC ---

  # show the sidebar
  open_statbar_for <- function(city_row) {
    selected_destination(city_row)
    shinyjs::removeClass(id = "statbar", class = "closed")
    shinyjs::addClass(id = "map_container", class = "statbar")
  }

  # map click handler
  observeEvent(input$map_marker_click, {
    req(rv$destinations)
    clicked_id <- input$map_marker_click$id
    req(clicked_id)
    
    city_row <- rv$destinations[rv$destinations$city == clicked_id, , drop = FALSE]
    
    if (nrow(city_row) == 1) open_statbar_for(city_row)
  })

  # sidebar click handler
  observeEvent(input$sidebar_item_clicked, {
    req(rv$destinations)

    city_row <- rv$destinations[rv$destinations$city == input$sidebar_item_clicked, , drop = FALSE]
    if (nrow(city_row) == 1) open_statbar_for(city_row)
  })

  # logic when a city is picked
  observeEvent(selected_destination(), {
    req(selected_destination())
    city_data <- selected_destination()

    wiki_description <- get_wiki_intro(city_data$city)

    # calc breakdown percentages
    total <- city_data$total_cost
    f_pct <- round((city_data$flight_cost / total) * 100, 1)
    h_pct <- round((city_data$hotel_cost / total) * 100, 1)
    l_pct <- round((city_data$living_cost / total) * 100, 1)
    
    row_max <- max(c(f_pct, h_pct, l_pct), na.rm = TRUE)
    if(row_max == 0) row_max <- 1
    
    f_scaled <- round((f_pct / row_max) * 100, 1)
    h_scaled <- round((h_pct / row_max) * 100, 1)
    l_scaled <- round((l_pct / row_max) * 100, 1)

    f_marks <- flight_lookup[[city_data$code]]
    h_marks <- hotel_lookup[[city_data$code]]
    
    # Defaults
    verdict_text <- ""
    verdict_colour <- "#e2e8f0" 
    
    # Only calculate if we have data for BOTH flight and hotel
    if (!is.null(f_marks) && !is.null(h_marks)) {
      
      # Formula: Flight_Benchmark + (Hotel_Nightly_Benchmark * Days)
      days <- rv$trip_duration
      
      # Determine what constitutes "Low", "Average", and "High" for a trip of this specific length
      total_low      <- (f_marks$low * 2)      + (h_marks$h_low * days)
      total_med_low  <- (f_marks$med_low * 2)  + (h_marks$h_med_low * days)
      total_med_high <- (f_marks$med_high * 2) + (h_marks$h_med_high * days)
      total_high     <- (f_marks$high * 2)     + (h_marks$h_high * days)

      # 3. Compare User's ACTUAL Total Core Cost (Flight + Hotel)
      user_core_cost <- city_data$flight_cost + city_data$hotel_cost
      
      # 4. Generate Text Verdict
      verdict_text <- case_when(
        user_core_cost < total_low      ~ "a great",
        user_core_cost < total_med_low  ~ "a good",
        user_core_cost < total_med_high ~ "an average",
        user_core_cost < total_high     ~ "a high",
        TRUE                            ~ "an expensive" 
      )

      # 5. Generate Colour Verdict
      verdict_colour <- case_when(
        user_core_cost < total_low      ~ "#7bff7b",       
        user_core_cost < total_med_low  ~ "#c4ffc4",       
        user_core_cost < total_med_high ~ "",              
        user_core_cost < total_high     ~ "#ffa6a6",       
        TRUE                            ~ "#ff5a5a"        
      )
    }

    # send pure data to js to handle the fancy rendering
    session$sendCustomMessage("update_statbar", list(
      city = city_data$city,
      country = city_data$country,
      total_cost = sprintf("%.2f", round(city_data$total_cost, 2)),
      duration = rv$trip_duration,
      flight_cost = sprintf("%.2f", round(city_data$flight_cost, 2)),
      hotel_cost = sprintf("%.2f", round(city_data$hotel_cost, 2)),
      living_cost = sprintf("%.2f", round(city_data$living_cost, 2)),
      flight_pct = f_scaled*0.9,
      hotel_pct = h_scaled*0.9,
      living_pct = l_scaled*0.9,
      wiki_intro = wiki_description,
      verdict = verdict_text,
      verdict_colour = verdict_colour
    ))
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 40, zoom = 3)
  })

  # dont spam updates while typing
  debounced_budget_inputs <- reactive({
    list(min = input$budget_min, max = input$budget_max)
  }) %>% debounce(500)

  # sync text inputs with slider handles
  observeEvent(debounced_budget_inputs(), {
    
    d_min <- debounced_budget_inputs()$min
    d_max <- debounced_budget_inputs()$max
    
    req(d_min, d_max)
    if (d_min < 0 || d_max < 0) return()
    
    if (d_min >= d_max) d_max <- d_min + 100
    
    new_track_min <- 0  
    new_track_max <- max(d_max, 2000) 
    
    updateSliderInput(session, "budget", 
                      min = new_track_min, 
                      max = new_track_max,
                      value = c(d_min, d_max)) 
  })

  observeEvent(input$search_btn, {
    
    if (is.null(input$departure_airport) || input$departure_airport == "") {
      showNotification("Please select a departure airport.", type = "error")
      return()
    }

    search_triggered(TRUE)

    # heavy lifting done in utils
    dep_info      <- get_departure_info(input$departure_airport, departure_airports)
    connections   <- get_other_airports(input$departure_airport, departure_airports)
    duration      <- get_trip_duration(input$departure_date, input$return_date)
    costs         <- get_trip_costs(input$departure_airport, connections, input$departure_date, input$return_date)
    filtered_data <- filter_cities_by_budget(input$budget, costs)

    rv$destinations   <- filtered_data
    rv$trip_duration  <- duration
    rv$departure_code <- dep_info$code
    rv$departure_date <- input$departure_date
    rv$return_date    <- input$return_date
    selected_destination(NULL) 

    map <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()

    dep_icon <- HTML('<div class="departure_icon"></div>')
    map %>%
      addLabelOnlyMarkers(lng = dep_info$lon, lat = dep_info$lat, label = dep_icon,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>%
      addCircleMarkers(data = dep_info, lng = ~lon, lat = ~lat, radius = 10, fillOpacity = 0, opacity = 0, label = ~city)

    if (nrow(filtered_data) > 0) {
      
      # draw curved lines
      path_matrices <- lapply(seq_len(nrow(filtered_data)), function(i) {
        city <- filtered_data[i, ]
        df <- create_curved_path(dep_info$lon, dep_info$lat, city$lon, city$lat, num_points = 50)
        as.matrix(df[, c("lon", "lat")])
      })
      
      paths_sf <- sf::st_multilinestring(path_matrices) %>% sf::st_sfc(crs = 4326)

      popups <- make_destination_popup(filtered_data, rv$trip_duration, dep_info$code)
      dest_icon <- HTML('<div class="destination_icon"></div>')
      icon_list <- lapply(seq_len(nrow(filtered_data)), function(x) dest_icon)

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

    shinyjs::removeClass(id = "sidebar", class = "closed")
    shinyjs::addClass(id = "map_container", class = "sidebar")
    shinyjs::removeClass(id = "statbar", class = "maximised")
    shinyjs::addClass(id = "statbar", class = "closed")
    shinyjs::removeClass(id = "map_container", class = "statbar")
    shinyjs::removeClass(id = "map_container", class = "compressed")

    shinyjs::delay(100, shinyjs::runjs("
      document.querySelectorAll('.flight-path-anim').forEach(function(path) {
        path.style.animation = 'none'; path.offsetHeight; 
        path.style.animation = 'drawLine 2.5s linear forwards';
      });
    "))
  })

  # display list of cities
  output$sidebar_results <- renderUI({
    
    if (is.null(rv$destinations) || nrow(rv$destinations) == 0) {
      return(tags$div(class="no-results",
        tags$h4("No destinations found"),
        tags$p("Try adjusting your filters.")
      ))
    }

    data_to_sort <- rv$destinations
    
    # math for distance sorting
    dep_lat <- departure_airports$lat[departure_airports$code == rv$departure_code]
    dep_lon <- departure_airports$lon[departure_airports$code == rv$departure_code]
    
    data_to_sort$dist_calc <- sqrt((data_to_sort$lat - dep_lat)^2 + (data_to_sort$lon - dep_lon)^2)
    
    # user sorting logic
    if (!is.null(input$sort_by)) {
      
      if (input$sort_by == "price_asc") {
        sorted_data <- data_to_sort[order(data_to_sort$total_cost), ]
        
      } else if (input$sort_by == "price_desc") {
        sorted_data <- data_to_sort[order(data_to_sort$total_cost, decreasing = TRUE), ]
        
      } else if (input$sort_by == "dist_asc") {
        sorted_data <- data_to_sort[order(data_to_sort$dist_calc), ]
        
      } else if (input$sort_by == "dist_desc") {
        sorted_data <- data_to_sort[order(data_to_sort$dist_calc, decreasing = TRUE), ]
        
      } else {
        sorted_data <- data_to_sort[order(data_to_sort$total_cost), ]
      }
      
    } else {
      sorted_data <- data_to_sort 
    }
    
    html_content <- make_destination_box(sorted_data, rv$trip_duration, rv$departure_code)
    HTML(html_content)
  })

  output$statbar_info <- renderUI({
    city <- selected_destination()
    req(city)
    
    make_sidebar_content(city, rv$trip_duration, rv$departure_code)
  })
}
# server <- function(input, output, session) {

#   # global state stuff
#   rv <- reactiveValues(
#     destinations = NULL,
#     trip_duration = NULL,
#     departure_code = NULL,
#     return_date = NULL,
#     departure_date = NULL
#   )

#   selected_destination <- reactiveVal(NULL)
#   search_triggered <- reactiveVal(FALSE)

#   # fill airport list
#   airport_choices <- setNames(departure_airports$code, departure_airports$city)
#   updateSelectizeInput(session, "departure_airport",
#     choices = airport_choices,
#     selected = character(0),
#     options = list(placeholder = "Enter Departure Airport...")
#   )

#   # updateSelectizeInput(session, "departure_airport",
#   #   choices = airport_choices,
#   #   selected = airport_choices[1],
#   #   options = list(placeholder = "Type to search...")
#   # )

#   # shinyjs::delay(300, {
#   #     # Optional: Update budget manually if you want a specific test case
#   #     updateNumericInput(session, "budget_min", value = 0)
#   #     updateNumericInput(session, "budget_max", value = 1500)
      
#   #     # Simulate the click
#   #     shinyjs::click("search_btn") 
#   # })

#   # defaults
#   updateDateInput(session, "return_date", value = Sys.Date() + 12, min = Sys.Date())
#   updateDateInput(session, "departure_date", value = Sys.Date() + 7, min = Sys.Date())

#   flght_benchmarks <- readRDS("data/flight_thresholds.rds")
#   flight_lookup <- split(flght_benchmarks, flght_benchmarks$code)

#   hotel_benchmarks <- readRDS("data/hotel_thresholds.rds")
#   hotel_lookup <- split(hotel_benchmarks, hotel_benchmarks$code)

#   # auto fix return date if user picks impossible range
#   observeEvent(input$departure_date, {
#     new_min_return <- input$departure_date %m+% days(1)
#     current_return <- input$return_date
#     new_value <- if (current_return <= input$departure_date) new_min_return else current_return
#     updateDateInput(session, "return_date", min = new_min_return, value = new_value)
#   }, ignoreInit = TRUE)

#   # prevents ugly errors before data loads
#   empty_plot_layout <- function(p) {
#     p %>% plotly::layout(
#       paper_bgcolor = "rgba(0,0,0,0)",
#       plot_bgcolor  = "rgba(0,0,0,0)",
#       font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
#       xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
#       yaxis = list(title = "", fixedrange = TRUE, showgrid = TRUE, gridcolor = "#f7f7f7", zeroline = FALSE, tickprefix = "£"),
#       margin = list(l = 35, r = 0, t = 10, b = 20)
#     ) %>%
#     plotly::config(displayModeBar = FALSE)
#   }

#   output$return_plot <- plotly::renderPlotly({
#     plotly::plot_ly(x = c(), y = c(), type = "bar") %>% empty_plot_layout()
#   })

#   output$departure_plot <- plotly::renderPlotly({
#     plotly::plot_ly(x = c(), y = c(), type = "bar") %>% empty_plot_layout()
#   })

#   output$cfd_plot <- plotly::renderPlotly({
#     plotly::plot_ly(x = c(), type = "histogram") %>% empty_plot_layout()
#   })

#   output$season_plot <- plotly::renderPlotly({
#     plotly::plot_ly(x = c(), y = c(), type = "scatter", mode = "lines") %>% 
#       empty_plot_layout() %>%
#       plotly::layout(yaxis = list(title = "Avg Price"))
#   })

#   output$weekday_plot <- plotly::renderPlotly({
#     plotly::plot_ly(x = c(), y = c(), type = "bar") %>% 
#       empty_plot_layout() %>%
#       plotly::layout(yaxis = list(title = "Price Impact"))
#   })

#   # show the sidebar
#   open_statbar_for <- function(city_row) {
#     selected_destination(city_row)
#     shinyjs::removeClass(id = "statbar", class = "closed")
#     shinyjs::addClass(id = "map_container", class = "statbar")
#   }

#   # map click handler
#   observeEvent(input$map_marker_click, {
#     req(rv$destinations)
#     clicked_id <- input$map_marker_click$id
#     req(clicked_id)
    
#     city_row <- rv$destinations[rv$destinations$city == clicked_id, , drop = FALSE]
    
#     if (nrow(city_row) == 1) open_statbar_for(city_row)
#   })

#   # sidebar click handler
#   observeEvent(input$sidebar_item_clicked, {
#     req(rv$destinations)

#     city_row <- rv$destinations[rv$destinations$city == input$sidebar_item_clicked, , drop = FALSE]
#     if (nrow(city_row) == 1) open_statbar_for(city_row)
#   })

#   # logic when a city is picked
#   observeEvent(selected_destination(), {
#     req(selected_destination())
#     city_data <- selected_destination()

#     wiki_description <- get_wiki_intro(city_data$city)

#     # calc breakdown percentages
#     total <- city_data$total_cost
#     f_pct <- round((city_data$flight_cost / total) * 100, 1)
#     h_pct <- round((city_data$hotel_cost / total) * 100, 1)
#     l_pct <- round((city_data$living_cost / total) * 100, 1)
    
#     row_max <- max(c(f_pct, h_pct, l_pct), na.rm = TRUE)
#     if(row_max == 0) row_max <- 1
    
#     f_scaled <- round((f_pct / row_max) * 100, 1)
#     h_scaled <- round((h_pct / row_max) * 100, 1)
#     l_scaled <- round((l_pct / row_max) * 100, 1)

#     trip_avg_leg_cost <- city_data$flight_cost / 2

#     f_marks <- flight_lookup[[city_data$code]]
#     h_marks <- hotel_lookup[[city_data$code]]
    
#     # Defaults
#     verdict_text <- ""
#     verdict_colour <- "#e2e8f0" # A neutral default grey if data is missing
    
#     # Only calculate if we have data for BOTH flight and hotel
#     if (!is.null(f_marks) && !is.null(h_marks)) {
      
#       # 2. Calculate "Expected Total Cost" for THIS trip duration
#       # Formula: Flight_Benchmark + (Hotel_Nightly_Benchmark * Days)
#       days <- rv$trip_duration
      
#       # Determine what constitutes "Low", "Average", and "High" for a trip of this specific length
#       total_low      <- (f_marks$low * 2)      + (h_marks$h_low * days)
#       total_med_low  <- (f_marks$med_low * 2)  + (h_marks$h_med_low * days)
#       total_med_high <- (f_marks$med_high * 2) + (h_marks$h_med_high * days)
#       total_high     <- (f_marks$high * 2)     + (h_marks$h_high * days)

#       # 3. Compare User's ACTUAL Total Core Cost (Flight + Hotel)
#       # We sum the actual costs to see if the overall package is a deal
#       user_core_cost <- city_data$flight_cost + city_data$hotel_cost
      
#       # 4. Generate Text Verdict
#       verdict_text <- case_when(
#         user_core_cost < total_low      ~ "a great",
#         user_core_cost < total_med_low  ~ "a good",
#         user_core_cost < total_med_high ~ "an average",
#         user_core_cost < total_high     ~ "a high",
#         TRUE                            ~ "an expensive" 
#       )

#       # 5. Generate Colour Verdict
#       verdict_colour <- case_when(
#         user_core_cost < total_low      ~ "#7bff7b",       # Bright Green
#         user_core_cost < total_med_low  ~ "#c4ffc4",       # Light Green
#         user_core_cost < total_med_high ~ "",              # Default (No colour/White)
#         user_core_cost < total_high     ~ "#ffa6a6",       # Light Red
#         TRUE                            ~ "#ff5a5a"        # Bright Red
#       )
#     }

#     # b_marks <- flight_lookup[[city_data$code]]

#     # verdict_text <- "Unknown"

#     # if (!is.null(b_marks)) {
#     #   verdict_text <- case_when(
#     #     trip_avg_leg_cost < b_marks$low      ~ "a great",
#     #     trip_avg_leg_cost < b_marks$med_low  ~ "a good",
#     #     trip_avg_leg_cost < b_marks$med_high ~ "an average",
#     #     trip_avg_leg_cost < b_marks$high     ~ "a high",
#     #     TRUE                                 ~ "an expensive" 
#     #   )
#     # }

#     # if (!is.null(b_marks)) {
#     #   verdict_colour <- case_when(
#     #     trip_avg_leg_cost < b_marks$low      ~ "#7bff7b",
#     #     trip_avg_leg_cost < b_marks$med_low  ~ "#c4ffc4",
#     #     trip_avg_leg_cost < b_marks$med_high ~ "",
#     #     trip_avg_leg_cost < b_marks$high     ~ "#ffa6a6",
#     #     TRUE                                 ~ "#ff5a5a" 
#     #   )
#     # }

#     # send pure data to js to handle the fancy rendering
#     session$sendCustomMessage("update_statbar", list(
#       city = city_data$city,
#       country = city_data$country,
#       total_cost = sprintf("%.2f", round(city_data$total_cost, 2)),
#       duration = rv$trip_duration,
#       flight_cost = sprintf("%.2f", round(city_data$flight_cost, 2)),
#       hotel_cost = sprintf("%.2f", round(city_data$hotel_cost, 2)),
#       living_cost = sprintf("%.2f", round(city_data$living_cost, 2)),
#       flight_pct = f_scaled*0.9,
#       hotel_pct = h_scaled*0.9,
#       living_pct = l_scaled*0.9,
#       wiki_intro = wiki_description,
#       verdict = verdict_text,
#       verdict_colour = verdict_colour
#     ))
#   })

#   # return flight price comparison
#   output$return_plot <- plotly::renderPlotly({
#     city_data <- selected_destination() 
#     req(city_data) 

#     prices <- get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
    
#     # highlight the cheapest bar
#     bar_colors <- rep("#D3E4FA", length(prices$return_prices)) 
#     bar_colors[which.min(prices$return_prices)] <- "#5B97EC"
    
#     hover_text <- sprintf("£%.2f", prices$return_prices)
    
#     plotly::plot_ly(
#       x = format(as.Date(prices$return_window), "%d %b"),
#       y = prices$return_prices,
#       text = hover_text,
#       type = "bar",
#       hovertemplate = "<b>%{x}</b><br>Diff: %{text}<extra></extra>",
#       marker = list(color = bar_colors, line = list(width = 0), cornerradius = 5)
#     ) %>%
#       plotly::layout(
#         paper_bgcolor = "rgba(0,0,0,0)",
#         plot_bgcolor  = "rgba(0,0,0,0)",
#         font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
#         xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
#         yaxis = list(
#           title = "", fixedrange = TRUE, showgrid = TRUE, 
#           gridcolor = "#f7f7f7", zeroline = TRUE, zerolinecolor = "#333333", 
#           zerolinewidth = 1.5, tickprefix = "£"
#         ),
#         margin = list(l = 35, r = 0, t = 10, b = 20)
#       ) %>%
#       plotly::config(displayModeBar = FALSE)
#   })

#   # same thing but for departure flight
#   output$departure_plot <- plotly::renderPlotly({
#     city_data <- selected_destination()
#     req(city_data)
    
#     prices <- get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
    
#     bar_colors <- rep("#D3E4FA", length(prices$departure_prices)) 
#     bar_colors[which.min(prices$departure_prices)] <- "#5B97EC"
    
#     hover_text <- sprintf("£%.2f", prices$departure_prices)
    
#     plotly::plot_ly(
#       x = format(as.Date(prices$departure_window), "%d %b"),
#       y = prices$departure_prices,
#       text = hover_text,
#       type = "bar",
#       hovertemplate = "<b>%{x}</b><br>Diff: %{text}<extra></extra>",
#       marker = list(color = bar_colors, line = list(width = 0), cornerradius = 5)
#     ) %>%
#       plotly::layout(
#         paper_bgcolor = "rgba(0,0,0,0)",
#         plot_bgcolor  = "rgba(0,0,0,0)",
#         font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
#         xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
#         yaxis = list(
#           title = "", fixedrange = TRUE, showgrid = TRUE, 
#           gridcolor = "#f7f7f7", zeroline = TRUE, zerolinecolor = "#333333", 
#           zerolinewidth = 1.5, tickprefix = "£"
#         ),
#         margin = list(l = 35, r = 0, t = 10, b = 20)
#       ) %>%
#       plotly::config(displayModeBar = FALSE)
#   })

#   # s-curve for probability
#   output$cfd_plot <- plotly::renderPlotly({
#     city_data <- selected_destination()
#     req(city_data)

#     cdf_function <- get_flight_cdf(
#       origin = rv$departure_code, 
#       dest = city_data$code, 
#       trip_date = rv$departure_date
#     )

#     price <- get_flight_prediction(
#       origin = rv$departure_code, 
#       dest = city_data$code, 
#       trip_date = rv$departure_date
#     )

#     req(is.function(cdf_function), !is.na(price))
    
#     sorted_prices <- knots(cdf_function)
#     probs <- cdf_function(sorted_prices)
#     current_price <- price
    
#     # cut off the extreme expensive outliers
#     x_limit <- as.numeric(quantile(sorted_prices, 0.99))
#     x_max <- max(x_limit, current_price * 1.1)

#     plotly::plot_ly() %>%
#       plotly::add_lines(
#         x = sorted_prices,
#         y = probs,
#         name = "CDF",
#         line = list(color = "#888", width = 3),
#         hovertemplate = "<b>Price: £%{x:.2f}</b><br>Chance: %{y:.1%}<extra></extra>"
#       ) %>%
#       plotly::add_segments(
#         x = current_price, xend = current_price, 
#         y = 0, yend = 1,
#         line = list(color = "#5B97EC", width = 2, dash = "dash"),
#         name = "Est. Price"
#       ) %>%
#       plotly::layout(
#         title = "",
#         paper_bgcolor = "rgba(0,0,0,0)",
#         plot_bgcolor  = "rgba(0,0,0,0)",
#         font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
#         xaxis = list(title = "Predicted Price (£)", showgrid = FALSE, zeroline = FALSE, range = c(0, x_max)),
#         yaxis = list(title = "CDF(x)", showgrid = TRUE, gridcolor = "#eee", zeroline = FALSE, range = c(0, 1.05), tickformat = ".0%"),
#         margin = list(l = 50, r = 20, t = 10, b = 40),
#         showlegend = FALSE
#       ) %>%
#       plotly::config(displayModeBar = FALSE)
#   })

#   # pie chart
#   output$living_breakdown_plot <- plotly::renderPlotly({
#     req(selected_destination())
#     city_data <- selected_destination()
    
#     living_details <- get_total_living_cost(
#       shortcode = city_data$code,
#       arrival_date = rv$departure_date,
#       leaving_date = rv$return_date
#     )
    
#     bd <- living_details$breakdown
#     plot_data <- data.frame(
#       Category = c("Food", "Drinks", "Transport", "Activities"),
#       Cost = c(bd$food, bd$drink, bd$transport, bd$activities)
#     )
    
#     plotly::plot_ly(plot_data, labels = ~Category, values = ~Cost, type = 'pie',
#             textposition = 'inside',
#             textinfo = 'label+percent',
#             insidetextorientation = 'radial',
#             marker = list(colors = c("#4299e1", "#9f7aea", "#48bb78", "#ed8936"), 
#                           line = list(color = '#FFFFFF', width = 1))) %>%
#       plotly::layout(
#         title = "",
#         paper_bgcolor = "rgba(0,0,0,0)",
#         plot_bgcolor  = "rgba(0,0,0,0)",
#         font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
#         margin = list(l = 20, r = 20, t = 20, b = 20),
#         showlegend = FALSE
#       ) %>%
#       plotly::config(displayModeBar = FALSE)
#   })

#   # monthly trends line chart
#   output$season_plot <- plotly::renderPlotly({
#     req(selected_destination())
#     city_data <- selected_destination()
    
#     trends <- get_route_trends(rv$departure_code, city_data$code)
#     data <- trends$seasonality
    
#     data$Month <- factor(data$Month, levels = month.abb)
#     data <- data[order(data$Month), ]

#     plotly::plot_ly(data, x = ~Month, y = ~base_price, type = 'scatter', mode = 'lines+markers',
#             line = list(color = "#5B97EC", width = 2),
#             marker = list(color = "#5B97EC", size = 6)) %>%
#       plotly::layout(
#         paper_bgcolor = "rgba(0,0,0,0)",
#         plot_bgcolor  = "rgba(0,0,0,0)",
#         xaxis = list(title = "", fixedrange=TRUE),
#         yaxis = list(title = "Avg Price", fixedrange=TRUE, tickprefix="£"),
#         margin = list(l=35, r=10, t=10, b=30)
#       ) %>%
#       plotly::config(displayModeBar = FALSE)
#   })

#   # daily variance bar chart
#   output$weekday_plot <- plotly::renderPlotly({
#     req(selected_destination())
#     city_data <- selected_destination()
    
#     trends <- get_route_trends(rv$departure_code, city_data$code)
#     data <- trends$weekday
    
#     days_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
#     data <- data[match(data$weekday, days_order), ] 
#     data <- na.omit(data) 

#     # green for cheap, red for expensive
#     colors <- rep("#D3E4FA", nrow(data))
#     if(nrow(data) > 0) {
#       colors[which.min(data$day_modifier)] <- "#48bb78" 
#       colors[which.max(data$day_modifier)] <- "#e53e3e" 
#     }

#     plotly::plot_ly(data, x = ~weekday, y = ~day_modifier, type = 'bar',
#             marker = list(color = colors)) %>%
#       plotly::layout(
#         paper_bgcolor = "rgba(0,0,0,0)",
#         plot_bgcolor  = "rgba(0,0,0,0)",
#         xaxis = list(title = "", fixedrange=TRUE),
#         yaxis = list(title = "Price Impact", fixedrange=TRUE, tickprefix="£"),
#         margin = list(l=35, r=10, t=10, b=30)
#       ) %>%
#       plotly::config(displayModeBar = FALSE)
#   })

#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addProviderTiles(providers$CartoDB.Positron) %>%
#       setView(lng = 0, lat = 40, zoom = 3)
#   })

#   # dont spam updates while typing
#   debounced_budget_inputs <- reactive({
#     list(min = input$budget_min, max = input$budget_max)
#   }) %>% debounce(500)

#   # sync text inputs with slider handles
#   observeEvent(debounced_budget_inputs(), {
    
#     d_min <- debounced_budget_inputs()$min
#     d_max <- debounced_budget_inputs()$max
    
#     req(d_min, d_max)
#     if (d_min < 0 || d_max < 0) return()
    
#     if (d_min >= d_max) d_max <- d_min + 100
    
#     new_track_min <- 0  
#     new_track_max <- max(d_max, 2000) 
    
#     updateSliderInput(session, "budget", 
#                       min = new_track_min, 
#                       max = new_track_max,
#                       value = c(d_min, d_max)) 
#   })

#   observeEvent(input$search_btn, {
    
#     if (is.null(input$departure_airport) || input$departure_airport == "") {
#       showNotification("Please select a departure airport.", type = "error")
#       return()
#     }

#     search_triggered(TRUE)

#     # heavy lifting done in utils
#     dep_info      <- get_departure_info(input$departure_airport, departure_airports)
#     connections   <- get_other_airports(input$departure_airport, departure_airports)
#     duration      <- get_trip_duration(input$departure_date, input$return_date)
#     costs         <- get_trip_costs(input$departure_airport, connections, input$departure_date, input$return_date)
#     filtered_data <- filter_cities_by_budget(input$budget, costs)

#     # print(head(filtered_data))

#     rv$destinations   <- filtered_data
#     rv$trip_duration  <- duration
#     rv$departure_code <- dep_info$code
#     rv$departure_date <- input$departure_date
#     rv$return_date    <- input$return_date
#     selected_destination(NULL) 

#     map <- leafletProxy("map") %>% clearMarkers() %>% clearShapes()

#     dep_icon <- HTML('<div class="departure_icon"></div>')
#     map %>%
#       addLabelOnlyMarkers(lng = dep_info$lon, lat = dep_info$lat, label = dep_icon,
#                           labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>%
#       addCircleMarkers(data = dep_info, lng = ~lon, lat = ~lat, radius = 10, fillOpacity = 0, opacity = 0, label = ~city)

#     if (nrow(filtered_data) > 0) {
      
#       # draw curved lines
#       path_matrices <- lapply(seq_len(nrow(filtered_data)), function(i) {
#         city <- filtered_data[i, ]
#         df <- create_curved_path(dep_info$lon, dep_info$lat, city$lon, city$lat, num_points = 50)
#         as.matrix(df[, c("lon", "lat")])
#       })
      
#       paths_sf <- sf::st_multilinestring(path_matrices) %>% sf::st_sfc(crs = 4326)

#       popups <- make_destination_popup(filtered_data, rv$trip_duration, dep_info$code)
#       dest_icon <- HTML('<div class="destination_icon"></div>')
#       icon_list <- lapply(seq_len(nrow(filtered_data)), function(x) dest_icon)

#       map %>%
#         addPolylines(data = paths_sf, color = "#667eea", weight = 4, opacity = 0.8,
#                      dashArray = "50000", options = pathOptions(className = "flight-path-anim", dashOffset = "50000")) %>%
#         addLabelOnlyMarkers(data = filtered_data, lng = ~lon, lat = ~lat, label = icon_list,
#                             labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE)) %>%
#         addCircleMarkers(data = filtered_data, lng = ~lon, lat = ~lat, radius = 10,
#                          fillOpacity = 0, opacity = 0, label = lapply(popups, HTML), layerId = ~city,
#                          labelOptions = labelOptions(noHide = FALSE, direction = "auto", sticky = TRUE,
#                                                      style = list("border-radius" = "10px", "width" = "200px"))) %>%
#         fitBounds(lng1 = min(c(dep_info$lon, filtered_data$lon)), lat1 = min(c(dep_info$lat, filtered_data$lat)),
#                   lng2 = max(c(dep_info$lon, filtered_data$lon)), lat2 = max(c(dep_info$lat, filtered_data$lat)),
#                   options = list(padding = c(20, 20), animate = TRUE, duration = 1.0))
#     }

#     shinyjs::removeClass(id = "sidebar", class = "closed")
#     shinyjs::addClass(id = "map_container", class = "sidebar")
#     shinyjs::removeClass(id = "statbar", class = "maximised")
#     shinyjs::addClass(id = "statbar", class = "closed")
#     shinyjs::removeClass(id = "map_container", class = "statbar")
#     shinyjs::removeClass(id = "map_container", class = "compressed")

#     # restart animation
#     shinyjs::delay(100, shinyjs::runjs("
#       document.querySelectorAll('.flight-path-anim').forEach(function(path) {
#         path.style.animation = 'none'; path.offsetHeight; 
#         path.style.animation = 'drawLine 2.5s linear forwards';
#       });
#     "))
#   })

#   # display list of cities
#   output$sidebar_results <- renderUI({
    
#     if (is.null(rv$destinations) || nrow(rv$destinations) == 0) {
#       return(tags$div(class="no-results",
#         tags$h4("No destinations found"),
#         tags$p("Try adjusting your filters.")
#       ))
#     }

#     data_to_sort <- rv$destinations
    
#     # math for distance sorting
#     dep_lat <- departure_airports$lat[departure_airports$code == rv$departure_code]
#     dep_lon <- departure_airports$lon[departure_airports$code == rv$departure_code]
    
#     data_to_sort$dist_calc <- sqrt((data_to_sort$lat - dep_lat)^2 + (data_to_sort$lon - dep_lon)^2)
    
#     # user sorting logic
#     if (!is.null(input$sort_by)) {
      
#       if (input$sort_by == "price_asc") {
#         sorted_data <- data_to_sort[order(data_to_sort$total_cost), ]
        
#       } else if (input$sort_by == "price_desc") {
#         sorted_data <- data_to_sort[order(data_to_sort$total_cost, decreasing = TRUE), ]
        
#       } else if (input$sort_by == "dist_asc") {
#         sorted_data <- data_to_sort[order(data_to_sort$dist_calc), ]
        
#       } else if (input$sort_by == "dist_desc") {
#         sorted_data <- data_to_sort[order(data_to_sort$dist_calc, decreasing = TRUE), ]
        
#       } else {
#         sorted_data <- data_to_sort[order(data_to_sort$total_cost), ]
#       }
      
#     } else {
#       sorted_data <- data_to_sort 
#     }
    
#     html_content <- make_destination_box(sorted_data, rv$trip_duration, rv$departure_code)
#     HTML(html_content)
#   })

#   output$statbar_info <- renderUI({
#     city <- selected_destination()
#     req(city)
    
#     make_sidebar_content(city, rv$trip_duration, rv$departure_code)
#   })
# }