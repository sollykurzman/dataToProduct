# Source logic if not using global.R
# source("R/utils_logic.R") 

server <- function(input, output, session) {

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
    options = list(placeholder = "Enter Departure Airport...")
  )

  #ON DEPLOYMENT REMOVE FROM HERE 
  # updateSelectizeInput(session, "departure_airport",
  #   choices = airport_choices,
  #   selected = airport_choices[1],
  #   options = list(placeholder = "Type to search...")
  # )

  # shinyjs::delay(300, {
  #     # Optional: Update budget manually if you want a specific test case
  #     updateNumericInput(session, "budget_min", value = 0)
  #     updateNumericInput(session, "budget_max", value = 1500)
      
  #     # Simulate the click
  #     shinyjs::click("search_btn") 
  # })
  #TO HERE

  # Initialize Dates
  updateDateInput(session, "return_date", value = Sys.Date() + 12, min = Sys.Date())
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

  output$cfd_plot <- plotly::renderPlotly({
    plotly::plot_ly(x = c(), type = "histogram") %>% empty_plot_layout()
  })

  output$season_plot <- plotly::renderPlotly({
    plotly::plot_ly(x = c(), y = c(), type = "scatter", mode = "lines") %>% 
      empty_plot_layout() %>%
      plotly::layout(yaxis = list(title = "Avg Price"))
  })

  # 2. Empty Weekday Plot (Bar Chart)
  output$weekday_plot <- plotly::renderPlotly({
    plotly::plot_ly(x = c(), y = c(), type = "bar") %>% 
      empty_plot_layout() %>%
      plotly::layout(yaxis = list(title = "Price Impact"))
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

    wiki_description <- get_wiki_intro(city_data$city)

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

    cdf_func <- get_flight_cdf(
      origin = rv$departure_code, 
      dest = city_data$code, 
      trip_date = rv$departure_date
    )
    percentile <- cdf_func(city_data$flight_cost)
    verdict_text <- case_when(
      percentile < 0.2 ~ "Great Deal! 💎",       # Bottom 20%
      percentile < 0.4 ~ "Good Price",           # 20% - 40%
      percentile < 0.6 ~ "Average Price",        # 40% - 60%
      percentile < 0.8 ~ "High Price",           # 60% - 80%
      TRUE             ~ "Very Expensive 💸"     # Top 20%
    )

    # 2. Send ONLY the HTML data to JS
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
      verdict = verdict_text
    ))
    
    # NOTE: We do NOT touch the graphs here.
  })

  # --- 1. Return Price Plot ---
  output$return_plot <- plotly::renderPlotly({
    # Explicitly depend on the selection
    city_data <- selected_destination() 
    req(city_data) # Stop if nothing is selected

    # Recalculate prices for the NEW city
    prices <- get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
    
    # Highlight logic
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

  # --- 2. Departure Price Plot ---
  output$departure_plot <- plotly::renderPlotly({
    city_data <- selected_destination()
    req(city_data)
    
    prices <- get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
    
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

  # --- 3. CDF (S-Curve) Plot ---
  output$cfd_plot <- plotly::renderPlotly({
    city_data <- selected_destination()
    req(city_data)

    # print(paste0("Generating CDF for ", city_data$city))
    
    # Re-run prediction for the NEW city
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

    # print(paste0("Predicted price: ", price))
    
    req(is.function(cdf_function), !is.na(price))
    
    sorted_prices <- knots(cdf_function)
    probs <- cdf_function(sorted_prices)
    current_price <- price
    
    # Dynamic Axis Scaling
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

  # --- Living Cost Breakdown Plot (Pie Chart) ---
  output$living_breakdown_plot <- plotly::renderPlotly({
    req(selected_destination())
    city_data <- selected_destination()
    
    # 1. Fetch detailed living cost data for this specific city
    # We recall the utility function to get the 'breakdown' list
    living_details <- get_total_living_cost(
      shortcode = city_data$code,
      arrival_date = rv$departure_date,
      leaving_date = rv$return_date
    )
    
    # 2. Prepare Data for Plotting
    # Convert the named list (food, drink, etc.) into a data frame
    bd <- living_details$breakdown
    plot_data <- data.frame(
      Category = c("Food", "Drinks", "Transport", "Activities"),
      Cost = c(bd$food, bd$drink, bd$transport, bd$activities)
    )
    
    # 3. Create Pie Chart
    plotly::plot_ly(plot_data, labels = ~Category, values = ~Cost, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = c("#4299e1", "#9f7aea", "#48bb78", "#ed8936"), # Custom colors to match your theme
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

  # output$return_plot <- plotly::renderPlotly({
  #   req(selected_destination())
  #   city_data <- selected_destination()
    
  #   prices <- get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
    
  #   # Highlight logic
  #   bar_colors <- rep("#D3E4FA", length(prices$return_prices)) 
  #   bar_colors[which.min(prices$return_prices)] <- "#5B97EC"
    
  #   # Pre-format text for the hover tooltip (so we don't break the y-axis math)
  #   hover_text <- sprintf("£%.2f", prices$return_prices)
    
  #   plotly::plot_ly(
  #     x = format(as.Date(prices$return_window), "%d %b"),
  #     y = prices$return_prices, # <--- FIX: Keep this NUMERIC
  #     text = hover_text,        # <--- FIX: Pass text separately
  #     type = "bar",
  #     # Update hovertemplate to use the custom text
  #     hovertemplate = "<b>%{x}</b><br>Diff: %{text}<extra></extra>",
  #     marker = list(color = bar_colors, line = list(width = 0), cornerradius = 5)
  #   ) %>%
  #     plotly::layout(
  #       paper_bgcolor = "rgba(0,0,0,0)",
  #       plot_bgcolor  = "rgba(0,0,0,0)",
  #       font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
  #       xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
  #       yaxis = list(
  #         title = "", 
  #         fixedrange = TRUE, 
  #         showgrid = TRUE, 
  #         gridcolor = "#f7f7f7", 
          
  #         # --- FIX: explicitly draw the zero line ---
  #         zeroline = TRUE, 
  #         zerolinecolor = "#333333", 
  #         zerolinewidth = 1.5,
          
  #         tickprefix = "£"
  #       ),
  #       margin = list(l = 35, r = 0, t = 10, b = 20)
  #     ) %>%
  #     plotly::config(displayModeBar = FALSE)
  # })

  # output$departure_plot <- plotly::renderPlotly({
  #   req(selected_destination())
  #   city_data <- selected_destination()
    
  #   prices <- get_prices_around(rv$departure_code, city_data, rv$departure_date, rv$return_date)
    
  #   bar_colors <- rep("#D3E4FA", length(prices$departure_prices)) 
  #   bar_colors[which.min(prices$departure_prices)] <- "#5B97EC"
    
  #   # Pre-format text
  #   hover_text <- sprintf("£%.2f", prices$departure_prices)
    
  #   plotly::plot_ly(
  #     x = format(as.Date(prices$departure_window), "%d %b"),
  #     y = prices$departure_prices, # <--- FIX: Keep this NUMERIC
  #     text = hover_text,           # <--- FIX: Pass text separately
  #     type = "bar",
  #     hovertemplate = "<b>%{x}</b><br>Diff: %{text}<extra></extra>",
  #     marker = list(color = bar_colors, line = list(width = 0), cornerradius = 5)
  #   ) %>%
  #     plotly::layout(
  #       paper_bgcolor = "rgba(0,0,0,0)",
  #       plot_bgcolor  = "rgba(0,0,0,0)",
  #       font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
  #       xaxis = list(title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
  #       yaxis = list(
  #         title = "", 
  #         fixedrange = TRUE, 
  #         showgrid = TRUE, 
  #         gridcolor = "#f7f7f7", 
          
  #         # --- FIX: explicitly draw the zero line ---
  #         zeroline = TRUE, 
  #         zerolinecolor = "#333333", 
  #         zerolinewidth = 1.5,
          
  #         tickprefix = "£"
  #       ),
  #       margin = list(l = 35, r = 0, t = 10, b = 20)
  #     ) %>%
  #     plotly::config(displayModeBar = FALSE)
  # })

  # # --- Render the CDF (S-Curve) Plot ---
  # output$cfd_plot <- plotly::renderPlotly({
  #   req(selected_destination())
  #   city_data <- selected_destination()
    
  #   # 1. Get Prediction Data
  #   cdf_function <- get_flight_cdf(
  #     origin = rv$departure_code, 
  #     dest = city_data$code, 
  #     trip_date = rv$departure_date
  #   )

  #   price <- get_flight_prediction(
  #     origin = rv$departure_code, 
  #     dest = city_data$code, 
  #     trip_date = rv$departure_date
  #   )
    
  #   req(is.function(cdf_function), !is.na(price))
    
  #   # 2. Process Data
  #   sorted_prices <- knots(cdf_function)
  #   probs <- cdf_function(sorted_prices)
  #   current_price <- price
    
  #   # --- FIX: Calculate Axis Limits ---
  #   # We grab the 99th percentile. This cuts off the top 1% of extreme 
  #   # expensive outliers so the curve fills the chart nicely.
  #   x_limit <- as.numeric(quantile(sorted_prices, 0.99))
    
  #   # Optional: Ensure the current price is definitely included in the view
  #   x_max <- max(x_limit, current_price * 1.1)

  #   # 3. Create the S-Curve Plot
  #   plotly::plot_ly() %>%
      
  #     plotly::add_lines(
  #       x = sorted_prices,
  #       y = probs,
  #       name = "CDF",
  #       line = list(color = "#888", width = 3),
  #       hovertemplate = "<b>Price: £%{x:.2f}</b><br>Chance: %{y:.1%}<extra></extra>"
  #     ) %>%
      
  #     plotly::add_segments(
  #       x = current_price, xend = current_price, 
  #       y = 0, yend = 1,
  #       line = list(color = "#5B97EC", width = 2, dash = "dash"),
  #       name = "Est. Price"
  #     ) %>%
      
  #     plotly::layout(
  #       title = "",
  #       paper_bgcolor = "rgba(0,0,0,0)",
  #       plot_bgcolor  = "rgba(0,0,0,0)",
  #       font = list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", size = 11, color = "#888"),
        
  #       # --- FIX: Apply the Range Limit ---
  #       xaxis = list(
  #         title = "Predicted Price (£)", 
  #         showgrid = FALSE,
  #         zeroline = FALSE,
  #         range = c(0, x_max) # <--- This forces the zoom
  #       ),
        
  #       yaxis = list(
  #         title = "CDF(x)", 
  #         showgrid = TRUE, 
  #         gridcolor = "#eee", 
  #         zeroline = FALSE,
  #         range = c(0, 1.05), 
  #         tickformat = ".0%"
  #       ),
        
  #       margin = list(l = 50, r = 20, t = 10, b = 40),
  #       showlegend = FALSE
  #     ) %>%
  #     plotly::config(displayModeBar = FALSE)
  # })

  # --- Render Trend Plots ---
  
  # 1. Seasonality Plot (Line Chart)
  output$season_plot <- plotly::renderPlotly({
    req(selected_destination())
    city_data <- selected_destination()
    
    # Get the trends
    trends <- get_route_trends(rv$departure_code, city_data$code)
    data <- trends$seasonality
    
    # Ensure correct month order for plotting
    # (Assuming 'Month' column is "Jan", "Feb" etc.)
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

  # 2. Weekday Plot (Bar Chart)
  output$weekday_plot <- plotly::renderPlotly({
    req(selected_destination())
    city_data <- selected_destination()
    
    trends <- get_route_trends(rv$departure_code, city_data$code)
    data <- trends$weekday
    
    # Sort Weekdays (Mon -> Sun)
    days_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    data <- data[match(data$Weekday, days_order), ] # Reorder rows
    data <- na.omit(data) # Safety

    # Color logic: Highlight the cheapest day green, expensive red
    colors <- rep("#D3E4FA", nrow(data))
    if(nrow(data) > 0) {
      colors[which.min(data$day_modifier)] <- "#48bb78" # Green
      colors[which.max(data$day_modifier)] <- "#e53e3e" # Red
    }

    plotly::plot_ly(data, x = ~Weekday, y = ~day_modifier, type = 'bar',
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

    # print(head(filtered_data))

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
    shinyjs::removeClass(id = "statbar", class = "maximised")
    shinyjs::addClass(id = "statbar", class = "closed")
    shinyjs::removeClass(id = "map_container", class = "statbar")
    shinyjs::removeClass(id = "map_container", class = "compressed")

    # Trigger CSS Animation
    shinyjs::delay(100, shinyjs::runjs("
      document.querySelectorAll('.flight-path-anim').forEach(function(path) {
        path.style.animation = 'none'; path.offsetHeight; 
        path.style.animation = 'drawLine 2.5s linear forwards';
      });
    "))
  })

  # --- 5. Sidebar Result Renderer ---
  # output$sidebar_results <- renderUI({
  #   # If no data, show empty state
  #   if (is.null(rv$destinations) || nrow(rv$destinations) == 0) {
  #     return(tags$div(class="no-results",
  #       tags$h4("No destinations found"),
  #       tags$p("Try adjusting your filters.")
  #     ))
  #   }

  #   sorted_data <- rv$destinations[order(rv$destinations$total_cost, rv$destinations$city), ]
    
  #   # Use utility function to generate HTML
  #   html_content <- make_destination_box(sorted_data, rv$trip_duration, rv$departure_code)
  #   HTML(html_content)
  # })

  # --- 5. Sidebar Result Renderer ---
  output$sidebar_results <- renderUI({
    
    # 1. Safety Check: If no results, show the "No Results" message
    if (is.null(rv$destinations) || nrow(rv$destinations) == 0) {
      return(tags$div(class="no-results",
        tags$h4("No destinations found"),
        tags$p("Try adjusting your filters.")
      ))
    }

    # 2. Create a working copy of the data
    data_to_sort <- rv$destinations
    
    # 3. Calculate Distance (if needed for sorting)
    # We fetch the lat/lon of the CURRENT departure airport using the global dataframe
    dep_lat <- departure_airports$lat[departure_airports$code == rv$departure_code]
    dep_lon <- departure_airports$lon[departure_airports$code == rv$departure_code]
    
    # Calculate simple distance (Euclidean is fine for sorting purposes)
    data_to_sort$dist_calc <- sqrt((data_to_sort$lat - dep_lat)^2 + (data_to_sort$lon - dep_lon)^2)

    # print(head(data_to_sort))
    
    # 4. SORTING LOGIC
    # We check input$sort_by to see what the user picked
    if (!is.null(input$sort_by)) {
      
      if (input$sort_by == "price_asc") {
        # Cheapest First (Default)
        sorted_data <- data_to_sort[order(data_to_sort$total_cost), ]
        
      } else if (input$sort_by == "price_desc") {
        # Most Expensive First
        sorted_data <- data_to_sort[order(data_to_sort$total_cost, decreasing = TRUE), ]
        
      } else if (input$sort_by == "dist_asc") {
        # Nearest First
        sorted_data <- data_to_sort[order(data_to_sort$dist_calc), ]
        
      } else if (input$sort_by == "dist_desc") {
        # Furthest First
        sorted_data <- data_to_sort[order(data_to_sort$dist_calc, decreasing = TRUE), ]
        
      } else {
        # Fallback
        sorted_data <- data_to_sort[order(data_to_sort$total_cost), ]
      }
      
    } else {
      sorted_data <- data_to_sort # Fallback if input is null
    }
    
    # 5. Generate HTML
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
