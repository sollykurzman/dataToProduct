# Load required libraries
library(shiny)
library(leaflet)
library(shinyWidgets)
library(bslib)
library(htmltools)
library(dplyr)

# Define major European cities with coordinates
european_cities <- data.frame(
  city = c("London", "Paris", "Rome", "Barcelona", "Amsterdam", 
           "Berlin", "Vienna", "Prague", "Copenhagen", "Lisbon"),
  country = c("United Kingdom", "France", "Italy", "Spain", "Netherlands",
              "Germany", "Austria", "Czech Republic", "Denmark", "Portugal"),
  lat = c(51.5074, 48.8566, 41.9028, 41.3851, 52.3676,
          52.5200, 48.2082, 50.0755, 55.6761, 38.7223),
  lon = c(-0.1278, 2.3522, 12.4964, 2.1734, 4.9041,
          13.4050, 16.3738, 14.4378, 12.5683, -9.1393),
  price = c(450, 380, 420, 350, 400, 370, 360, 320, 440, 340),
  stringsAsFactors = FALSE
)

# Define departure airports (using city center coordinates)
departure_airports <- data.frame(
  airport = c("London Heathrow", "Paris CDG", "Frankfurt", "Amsterdam Schiphol", 
              "Madrid Barajas", "Munich", "Zurich", "Brussels"),
  code = c("LHR", "CDG", "FRA", "AMS", "MAD", "MUC", "ZRH", "BRU"),
  city = c("London", "Paris", "Frankfurt", "Amsterdam", "Madrid", "Munich", "Zurich", "Brussels"),
  lat = c(51.5074, 48.8566, 50.1109, 52.3676, 40.4168, 48.1351, 47.3769, 50.8503),
  lon = c(-0.1278, 2.3522, 8.6821, 4.9041, -3.7038, 11.5820, 8.5417, 4.3517),
  stringsAsFactors = FALSE
)

# Function to create curved flight path between two points
create_curved_path <- function(lon1, lat1, lon2, lat2, num_points = 50) {
  # Calculate the great circle intermediate points
  gcircle <- function(lon1, lat1, lon2, lat2, n) {
    # Convert to radians
    lon1_rad <- lon1 * pi / 180
    lat1_rad <- lat1 * pi / 180
    lon2_rad <- lon2 * pi / 180
    lat2_rad <- lat2 * pi / 180
    
    d <- 2 * asin(sqrt((sin((lat1_rad - lat2_rad) / 2))^2 + 
                         cos(lat1_rad) * cos(lat2_rad) * 
                         (sin((lon1_rad - lon2_rad) / 2))^2))
    
    if (d == 0) return(data.frame(lon = lon1, lat = lat1))
    
    # Generate intermediate points
    f <- seq(0, 1, length.out = n)
    
    A <- sin((1 - f) * d) / sin(d)
    B <- sin(f * d) / sin(d)
    
    x <- A * cos(lat1_rad) * cos(lon1_rad) + B * cos(lat2_rad) * cos(lon2_rad)
    y <- A * cos(lat1_rad) * sin(lon1_rad) + B * cos(lat2_rad) * sin(lon2_rad)
    z <- A * sin(lat1_rad) + B * sin(lat2_rad)
    
    lat <- atan2(z, sqrt(x^2 + y^2))
    lon <- atan2(y, x)
    
    data.frame(
      lon = lon * 180 / pi,
      lat = lat * 180 / pi
    )
  }
  
  gcircle(lon1, lat1, lon2, lat2, num_points)
}

# Define UI
ui <- page_sidebar(
  title = "",
  theme = bs_theme(
    version = 5,
    bg = "#fafbfc",
    fg = "#0f1419",
    primary = "#6366f1",
    secondary = "#f1f5f9",
    base_font = font_google("Plus Jakarta Sans"),
    heading_font = font_google("Plus Jakarta Sans"),
    font_scale = 0.95
  ),
  
  # Custom CSS for ultra-modern design with gradients
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@300;400;500;600;700;800&display=swap');
      
      :root {
        --gradient-primary: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        --gradient-secondary: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        --gradient-success: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
        --gradient-warm: linear-gradient(135deg, #fa709a 0%, #fee140 100%);
        --shadow-sm: 0 2px 8px rgba(0,0,0,0.04);
        --shadow-md: 0 4px 16px rgba(0,0,0,0.08);
        --shadow-lg: 0 8px 32px rgba(0,0,0,0.12);
        --shadow-xl: 0 16px 48px rgba(0,0,0,0.16);
      }
      
      body {
        font-family: 'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, sans-serif;
        background: #f8f9fa;
      }
      
      .bslib-sidebar-layout {
        --bs-border-color: transparent;
      }
      
      .bslib-gap-spacing {
        gap: 4px !important;
      }
      
      .sidebar {
        background: rgba(255, 255, 255, 0.98);
        backdrop-filter: blur(20px);
        -webkit-backdrop-filter: blur(20px);
        border-right: 1px solid rgba(255,255,255,0.3);
        padding: 24px 20px !important;
        box-shadow: 4px 0 24px rgba(0,0,0,0.08);
        border-radius: 0 24px 24px 0;
      }
      
      .app-title {
        font-size: 28px;
        font-weight: 800;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        margin-bottom: 4px;
        letter-spacing: -1px;
      }
      
      .app-subtitle {
        font-size: 14px;
        color: #64748b;
        margin-bottom: 24px;
        font-weight: 500;
      }
      
      .filter-section {
        margin-bottom: 18px;
        animation: fadeInUp 0.5s ease-out backwards;
      }
      
      .filter-section:nth-child(3) { animation-delay: 0.1s; }
      .filter-section:nth-child(4) { animation-delay: 0.2s; }
      .filter-section:nth-child(5) { animation-delay: 0.3s; }
      .filter-section:nth-child(6) { animation-delay: 0.4s; }
      
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .filter-label {
        font-size: 11px;
        font-weight: 700;
        color: #1e293b;
        margin-bottom: 8px;
        display: block;
        letter-spacing: 0.3px;
        text-transform: uppercase;
      }
      
      .form-control, .form-select {
        border: 2px solid #e2e8f0;
        border-radius: 12px;
        padding: 12px 16px;
        font-size: 15px;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        background: white;
        font-weight: 500;
        box-shadow: var(--shadow-sm);
      }
      
      .form-control:hover, .form-select:hover {
        border-color: #cbd5e1;
        box-shadow: var(--shadow-md);
      }
      
      .form-control:focus, .form-select:focus {
        border-color: #667eea;
        box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1), var(--shadow-md);
        outline: none;
        transform: translateY(-1px);
      }
      
      .irs--shiny {
        margin-top: 20px;
        margin-bottom: 8px;
      }
      
      .irs-bar {
        background: linear-gradient(90deg, #667eea 0%, #764ba2 100%) !important;
        border: none !important;
        height: 6px !important;
      }
      
      .irs-from, .irs-to, .irs-single {
        background: #667eea !important;
        font-weight: 600 !important;
        border-radius: 6px !important;
        padding: 2px 8px !important;
        font-size: 11px !important;
        color: white !important;
      }
      
      .irs-from:before, .irs-to:before, .irs-single:before {
        border-top-color: #667eea !important;
      }
      
      .irs-handle > i:first-child {
        background: #667eea !important;
        border: 3px solid white !important;
        box-shadow: 0 2px 8px rgba(102, 126, 234, 0.3) !important;
        width: 18px !important;
        height: 18px !important;
      }
      
      .irs-line {
        background: #e2e8f0 !important;
        border: none !important;
        height: 6px !important;
      }
      
      .irs-grid-text {
        font-size: 10px !important;
        color: #94a3b8 !important;
      }
      
      .irs-min, .irs-max {
        display: none !important;
      }
      
      .search-button {
        width: 100%;
        padding: 14px 24px;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        border-radius: 14px;
        font-size: 16px;
        font-weight: 700;
        cursor: pointer;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        margin-top: 24px;
        box-shadow: 0 6px 20px rgba(102, 126, 234, 0.35);
        letter-spacing: -0.3px;
        position: relative;
        overflow: hidden;
      }
      
      .search-button:before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
        transition: left 0.5s;
      }
      
      .search-button:hover {
        transform: translateY(-3px);
        box-shadow: 0 12px 32px rgba(102, 126, 234, 0.5);
      }
      
      .search-button:hover:before {
        left: 100%;
      }
      
      .search-button:active {
        transform: translateY(-1px);
        box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
      }
      
      #map {
        border-radius: 24px;
        box-shadow: var(--shadow-xl);
        height: calc(100vh - 48px) !important;
        border: 4px solid rgba(255,255,255,0.5);
      }
      
      .leaflet-popup-content-wrapper {
        border-radius: 20px;
        box-shadow: var(--shadow-xl);
        padding: 0;
        overflow: hidden;
        border: 3px solid rgba(255,255,255,0.8);
      }
      
      .leaflet-popup-close-button {
        top: 8px !important;
        right: 8px !important;
        width: 28px !important;
        height: 28px !important;
        font-size: 24px !important;
        padding: 0 !important;
        color: white !important;
        opacity: 0.8 !important;
      }
      
      .leaflet-popup-close-button:hover {
        opacity: 1 !important;
        color: white !important;
      }
      
      .leaflet-popup-content {
        margin: 0;
        min-width: 260px;
      }
      
      .popup-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px 24px;
        font-weight: 700;
        font-size: 20px;
        letter-spacing: -0.5px;
      }
      
      .popup-body {
        padding: 20px 24px;
        background: white;
      }
      
      .popup-row {
        display: flex;
        justify-content: space-between;
        margin-bottom: 12px;
        font-size: 15px;
        align-items: center;
      }
      
      .popup-label {
        color: #64748b;
        font-weight: 600;
      }
      
      .popup-value {
        color: #1e293b;
        font-weight: 700;
      }
      
      .popup-price {
        background: linear-gradient(135deg, #f1f5f9 0%, #e2e8f0 100%);
        padding: 16px;
        border-radius: 12px;
        text-align: center;
        margin-top: 16px;
        box-shadow: var(--shadow-sm);
      }
      
      .popup-price-label {
        font-size: 12px;
        color: #64748b;
        margin-bottom: 6px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .popup-price-value {
        font-size: 28px;
        font-weight: 800;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      
      .leaflet-popup-tip {
        background: white;
      }
      
      .leaflet-container {
        font-family: 'Plus Jakarta Sans', -apple-system, BlinkMacSystemFont, sans-serif;
      }
      
      /* Flight path animation */
      @keyframes flowingGlow {
        0% {
          stroke-dashoffset: 25;
          opacity: 0.6;
        }
        50% {
          opacity: 0.9;
        }
        100% {
          stroke-dashoffset: 0;
          opacity: 0.6;
        }
      }
      
      .flight-path {
        animation: flowingGlow 1s linear infinite;
      }
      
      /* Custom departure marker */
      .departure-marker {
        background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        border: 4px solid white;
        border-radius: 50%;
        width: 20px;
        height: 20px;
        box-shadow: 0 4px 12px rgba(245, 87, 108, 0.4);
        animation: pulse 2s infinite;
      }
      
      @keyframes pulse {
        0%, 100% {
          box-shadow: 0 4px 12px rgba(245, 87, 108, 0.4);
        }
        50% {
          box-shadow: 0 4px 20px rgba(245, 87, 108, 0.7), 0 0 0 8px rgba(245, 87, 108, 0.1);
        }
      }
      
      /* Notification styling */
      .shiny-notification {
        border-radius: 16px !important;
        box-shadow: var(--shadow-lg) !important;
        border: none !important;
        padding: 16px 24px !important;
        font-weight: 600 !important;
        backdrop-filter: blur(10px);
      }
      
      .shiny-notification-message {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        color: white !important;
      }
      
      .shiny-notification-warning {
        background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%) !important;
        color: white !important;
      }
      
      /* Smooth animations */
      * {
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
      }
    ")),
    
    # JavaScript for enhanced flight path animation
    tags$script(HTML("
      // Wait for map to load and add animation enhancements
      $(document).on('shiny:connected', function() {
        setTimeout(function() {
          // Function to add staggered animations to flight paths
          function animateFlightPaths() {
            // Target all SVG paths in the overlay pane
            var paths = document.querySelectorAll('.leaflet-overlay-pane svg path');
            var flightPaths = [];
            
            // Filter to get only the flight path polylines (not markers)
            paths.forEach(function(path) {
              if (path.getAttribute('stroke') && 
                  path.getAttribute('stroke-dasharray') && 
                  path.getAttribute('stroke-dasharray').includes('10')) {
                flightPaths.push(path);
              }
            });
            
            // Apply animation to each flight path
            flightPaths.forEach(function(path, index) {
              var delay = index * 0;
              path.style.animation = 'flowingGlow 1s linear infinite';
              path.style.animationDelay = delay + 's';
              path.style.filter = 'drop-shadow(0 0 6px rgba(102, 126, 234, 0.5))';
            });
          }
          
          // Run animation function
          animateFlightPaths();
          
          // Re-run when map updates
          setTimeout(animateFlightPaths, 1000);
          setTimeout(animateFlightPaths, 2000);
        }, 500);
      });
      
      // Listen for search button clicks to re-animate paths
      Shiny.addCustomMessageHandler('animate_paths', function(message) {
        setTimeout(function() {
          var paths = document.querySelectorAll('.leaflet-overlay-pane svg path');
          var flightPaths = [];
          
          paths.forEach(function(path) {
            if (path.getAttribute('stroke') && 
                path.getAttribute('stroke-dasharray') && 
                path.getAttribute('stroke-dasharray').includes('10')) {
              flightPaths.push(path);
            }
          });
          
          flightPaths.forEach(function(path, index) {
            var delay = index * 0;
            path.style.animation = 'none';
            setTimeout(function() {
              path.style.animation = 'flowingGlow 1s linear infinite';
              path.style.animationDelay = delay + 's';
              path.style.filter = 'drop-shadow(0 0 6px rgba(102, 126, 234, 0.5))';
            }, 50);
          });
        }, 800);
      });
    "))
  ),
  
  # Sidebar
  sidebar = sidebar(
    width = 340,
    
    # App Title
    div(class = "app-title", "✈️ Travel Europe"),
    div(class = "app-subtitle", "Discover your next adventure"),
    
    # Departure Airport
    div(class = "filter-section",
        tags$label(class = "filter-label", "🛫 Departure Airport"),
        selectInput("departure_airport",
                    label = NULL,
                    choices = setNames(
                      departure_airports$code,
                      paste0(departure_airports$airport, " (", departure_airports$code, ")")
                    ),
                    selected = "LHR")
    ),
    
    # Departure Date
    div(class = "filter-section",
        tags$label(class = "filter-label", "📅 Departure Date"),
        dateInput("departure_date", 
                  label = NULL,
                  value = Sys.Date() + 7,
                  min = Sys.Date(),
                  format = "dd M yyyy")
    ),
    
    # Return Date
    div(class = "filter-section",
        tags$label(class = "filter-label", "🏠 Return Date"),
        dateInput("return_date",
                  label = NULL,
                  value = Sys.Date() + 14,
                  min = Sys.Date() + 1,
                  format = "dd M yyyy")
    ),
    
    # Budget Slider
    div(class = "filter-section",
        tags$label(class = "filter-label", "💰 Budget (per person)"),
        sliderInput("budget",
                    label = NULL,
                    min = 200,
                    max = 1000,
                    value = c(300, 600),
                    step = 50,
                    pre = "€",
                    sep = ",")
    ),
    
    # Search Button
    actionButton("search_btn",
                 "🔍 Search Destinations",
                 class = "search-button")
  ),
  
  # Main content - Map
  leafletOutput("map", height = "100%")
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to track search state
  search_triggered <- reactiveVal(FALSE)
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(opacity = 0.9)) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                       options = providerTileOptions(opacity = 0.7)) %>%
      setView(lng = 10, lat = 50, zoom = 4) %>%
      addControl(
        html = tags$div(
          style = "background: linear-gradient(135deg, rgba(255,255,255,0.98) 0%, rgba(255,255,255,0.95) 100%); 
                   padding: 14px 20px; 
                   border-radius: 14px; 
                   box-shadow: 0 6px 20px rgba(0,0,0,0.1);
                   backdrop-filter: blur(10px);
                   border: 2px solid rgba(255,255,255,0.5);",
          tags$div(style = "font-size: 10px; color: #64748b; font-weight: 700; letter-spacing: 1px; text-transform: uppercase;", "EXPLORE"),
          tags$div(style = "font-size: 16px; 
                            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                            -webkit-background-clip: text;
                            -webkit-text-fill-color: transparent;
                            background-clip: text;
                            font-weight: 800; 
                            margin-top: 3px; 
                            letter-spacing: -0.5px;", 
                   "European Destinations")
        ),
        position = "topright"
      )
  })
  
  # Handle search button click
  observeEvent(input$search_btn, {
    
    # Validate dates
    if (input$return_date <= input$departure_date) {
      showNotification(
        "Return date must be after departure date",
        type = "error",
        duration = 3
      )
      return()
    }
    
    # Get departure airport info
    departure_info <- departure_airports %>%
      filter(code == input$departure_airport)
    
    # Filter cities based on budget
    filtered_cities <- european_cities %>%
      filter(price >= input$budget[1] & price <= input$budget[2])
    
    # If no cities match, show all but with notification
    if (nrow(filtered_cities) == 0) {
      showNotification(
        "No destinations found in this budget range. Showing all destinations.",
        type = "warning",
        duration = 4
      )
      filtered_cities <- european_cities
    }
    
    # Calculate trip duration
    trip_duration <- as.numeric(difftime(input$return_date, input$departure_date, units = "days"))
    
    # Create custom destination icon
    destination_icon <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-violet.png",
      iconWidth = 25,
      iconHeight = 41,
      iconAnchorX = 12,
      iconAnchorY = 41,
      popupAnchorX = 0,
      popupAnchorY = -35
    )
    
    # Create departure airport icon
    departure_icon <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
      iconWidth = 30,
      iconHeight = 49,
      iconAnchorX = 15,
      iconAnchorY = 49,
      popupAnchorX = 0,
      popupAnchorY = -40
    )
    
    # Update map with markers and flight paths
    leaflet_map <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
    
    # Add departure airport marker
    leaflet_map <- leaflet_map %>%
      addMarkers(
        lng = departure_info$lon,
        lat = departure_info$lat,
        icon = departure_icon,
        popup = paste0(
          '<div>',
          '<div class="popup-header">🛫 ', departure_info$city, '</div>',
          '<div class="popup-body">',
          '<div class="popup-row">',
          '<span class="popup-label">Airport</span>',
          '<span class="popup-value">', departure_info$code, '</span>',
          '</div>',
          '<div class="popup-row">',
          '<span class="popup-label">Type</span>',
          '<span class="popup-value">Departure City</span>',
          '</div>',
          '</div>',
          '</div>'
        ),
        label = paste0("🛫 ", departure_info$city),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "Plus Jakarta Sans, sans-serif",
            "font-size" = "13px",
            "font-weight" = "700",
            "padding" = "8px 14px",
            "border-radius" = "10px",
            "background" = "linear-gradient(135deg, #f093fb 0%, #f5576c 100%)",
            "color" = "white",
            "border" = "2px solid white",
            "box-shadow" = "0 4px 12px rgba(245, 87, 108, 0.3)"
          )
        )
      )
    
    # Add markers and flight paths for each destination
    for (i in 1:nrow(filtered_cities)) {
      city_data <- filtered_cities[i, ]
      
      # Create flight path
      flight_path <- create_curved_path(
        departure_info$lon, 
        departure_info$lat,
        city_data$lon, 
        city_data$lat,
        num_points = 100
      )
      
      # Add animated curved flight path with flowing glow effect
      # Create gradient effect with multiple overlapping lines
      
      # Base path (lighter, wider)
      leaflet_map <- leaflet_map %>%
        addPolylines(
          lng = flight_path$lon,
          lat = flight_path$lat,
          color = "#a78bfa",
          weight = 8,
          opacity = 0.3,
          smoothFactor = 1,
          group = "flight_paths"
        )
      
      # Middle glow layer
      leaflet_map <- leaflet_map %>%
        addPolylines(
          lng = flight_path$lon,
          lat = flight_path$lat,
          color = "#8b5cf6",
          weight = 7,
          opacity = 0.5,
          smoothFactor = 1,
          group = "flight_paths"
        )
      
      # Top animated dashed line (flowing effect)
      leaflet_map <- leaflet_map %>%
        addPolylines(
          lng = flight_path$lon,
          lat = flight_path$lat,
          color = "#667eea",
          weight = 6,
          opacity = 0.8,
          dashArray = "10, 15",
          smoothFactor = 1,
          layerId = paste0("flight_path_", i),
          group = "flight_paths",
          options = pathOptions(
            pane = "overlayPane"
          )
        )
      
      # Create custom popup content
      popup_content <- paste0(
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
        '<div class="popup-row">',
        '<span class="popup-label">From</span>',
        '<span class="popup-value">', departure_info$code, '</span>',
        '</div>',
        '<div class="popup-price">',
        '<div class="popup-price-label">Flight + Hotel</div>',
        '<div class="popup-price-value">€', city_data$price, '</div>',
        '</div>',
        '</div>',
        '</div>'
      )
      
      # Add destination marker
      leaflet_map <- leaflet_map %>%
        addMarkers(
          lng = city_data$lon,
          lat = city_data$lat,
          icon = destination_icon,
          popup = popup_content,
          label = city_data$city,
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Plus Jakarta Sans, sans-serif",
              "font-size" = "13px",
              "font-weight" = "700",
              "padding" = "8px 14px",
              "border-radius" = "10px",
              "background" = "rgba(255,255,255,0.95)",
              "backdrop-filter" = "blur(10px)",
              "border" = "2px solid #e2e8f0",
              "box-shadow" = "0 4px 12px rgba(0,0,0,0.1)",
              "color" = "#1e293b"
            )
          )
        )
    }
    
    # Fit bounds to show all markers
    all_lons <- c(departure_info$lon, filtered_cities$lon)
    all_lats <- c(departure_info$lat, filtered_cities$lat)
    
    leaflet_map %>%
      fitBounds(
        lng1 = min(all_lons) - 2,
        lat1 = min(all_lats) - 2,
        lng2 = max(all_lons) + 2,
        lat2 = max(all_lats) + 2
      )
    
    # Show success notification
    showNotification(
      paste("✈️ Found", nrow(filtered_cities), "destinations from", departure_info$city, "!"),
      type = "message",
      duration = 3
    )
    
    # Trigger animation of flight paths
    session$sendCustomMessage("animate_paths", list(timestamp = Sys.time()))
    
    search_triggered(TRUE)
  })
}

# Run the application
shinyApp(
  ui = ui, 
  server = server,
  options = list(
    launch.browser = TRUE,
    display.mode = "normal"
  )
)