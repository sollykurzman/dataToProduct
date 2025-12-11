
ui <- function() {
  fluidPage(
    useShinyjs(),
    tags$script(HTML("
    $(document).on('click', '#close_sidebar_icon', function() {
      
      // 1. Toggle the class
      $('#sidebar').removeClass('closed');
      $('#map_container').removeClass('sidebar');
      
      // 2. Notify Shiny (Optional, keep if you have server logic)
      Shiny.setInputValue('sidebar_clicked', new Date().getTime());
    });

    $(document).on('click', '#close_statbar_icon', function() {
      
      // 1. Toggle the class
      $('#statbar').addClass('closed');
      $('#statbar').removeClass('maximised');
      $('#map_container').removeClass('compressed');
      $('#map_container').removeClass('statbar');
      
      // 2. Notify Shiny (Optional, keep if you have server logic)
      Shiny.setInputValue('statbar_clicked', new Date().getTime());
    });

    $(document).on('click', '#maximise_statbar_icon', function() {
      
      // 1. Toggle the class
      $('#statbar').addClass('maximised');
      $('#map_container').addClass('compressed');
      
      // 2. Notify Shiny (Optional, keep if you have server logic)
      Shiny.setInputValue('maximise_statbar_clicked', new Date().getTime());
    });

    $(document).on('click', '#minimise_statbar_icon', function() {
      
      // 1. Toggle the class
      $('#statbar').removeClass('maximised');
      $('#map_container').removeClass('compressed');
      
      // 2. Notify Shiny (Optional, keep if you have server logic)
      Shiny.setInputValue('minimise_statbar_clicked', new Date().getTime());
    });

    $(document).on('click', '#map_open', function() {
      
      // 1. Toggle the class
      $('#statbar').removeClass('maximised');
      $('#map_container').removeClass('compressed');
      
      // 2. Notify Shiny (Optional, keep if you have server logic)
      Shiny.setInputValue('statbar_clicked', new Date().getTime());
    });

    // NEW: when a sidebar card is clicked
    $(document).on('click', '.sidebaritem', function() {
      var cityId = $(this).data('city');
      Shiny.setInputValue('sidebar_item_clicked', cityId, {priority: 'event'});
    });

    Shiny.addCustomMessageHandler('update_statbar', function(data) {
      // 1. Update Text & Images
      $('#sb_city').text(data.city);
      $('#sb_country').text(data.country);
      $('#sb_total').text('£' + data.total_cost);
      $('#sb_duration').text('(' + data.duration + ' days)');
      $('#introtext').text(data.wiki_intro);
      $('#sb_verdict').text(data.verdict);
      
      // Update Image (Reset display in case it was hidden by error previously)
      $('#sb_img').attr('src', 'statbar_images/' + data.city + '.png').css('display', 'block');

      // 2. Update Costs
      $('#val_flight').text('£' + data.flight_cost);
      $('#val_hotel').text('£' + data.hotel_cost);
      $('#val_living').text('£' + data.living_cost);

      // 3. Update Bar Widths (CSS Transition handles the animation!)
      $('#width_flight').css('width', data.flight_pct + '%');
      $('#width_hotel').css('width', data.hotel_pct + '%');
      $('#width_living').css('width', data.living_pct + '%');
    });
")),
    includeCSS("www/styles.css"),
    tagList(
      div(id="menu_bar",

        div(class="container",
          
          div(id="header",
            class="filter-section",
            img(src="icons/airplane.svg"),
            div(id="title",
              h1("GoLow"),
              h2("Adventure by budget")
            )
          ),

          div(id="search_filters",
            div(id = "search_bar", class = "filter-section",
              img(src="icons/airplane_departure.svg", id="airport_selection_icon"),
              selectizeInput(inputId = "departure_airport", label = NULL, choices = NULL, selected = NULL, multiple = FALSE, options = list(placeholder = "Enter Departure Airport..."))
            ),

            # h3("Select Dates"),
            div(id = "date_selection", class = "filter-section",
              dateInput(inputId = "departure_date", label=NULL),
              dateInput(inputId = "return_date", label=NULL)
            ),

            # h3("Set Budget"),
            # div(id = "budget_selection", class = "filter-section",
            #   sliderInput("budget", label=NULL, min=000, max=1000, value=c(0,600), step=50, pre="£")
            # ),

            # div(id = "budget_selection", class = "filter-section",
            #     shinyWidgets::numericRangeInput(
            #       inputId = "budget",
            #       label = NULL,
            #       value = c(0, 600),
            #       min = 0, 
            #       max = 2000,
            #       separator = " to "#,
            #       # icon = icon("pound-sign")
            #     )
            # ),

            div(id = "budget_selection", class = "filter-section",
    
              # 1. Min Input (Text Box)
              div(class = "budget-input-box",
                numericInput("budget_min", label = NULL, value = 0, width = "100%")
              ),

              # 2. The Slider (Hidden labels to save space)
              div(class = "budget-slider-container",
                sliderInput("budget", label = NULL, min = 0, max = 2000, value = c(1000, 1500), step = 50, ticks = FALSE, pre = "£")
              ),

              # 3. Max Input (Text Box)
              div(class = "budget-input-box",
                numericInput("budget_max", label = NULL, value = 1500, width = "100%")
              )
            ),

            div(id = "search_button_container", class = "filter-section",
              actionButton("search_btn", "Search", class="search-button")
            )
          )
          

        )
      ),


      div(id = "map_container", #class= "sidebar statbar",
        img(src="icons/map.svg", id="map_open"),
        div(class="container",
          leafletOutput("map", width = "100%", height = "100%")
        )
      ),
      
      div(id = "sidebar", class = "closed",
        # p("Top Destinations"),
        div(class = "sidebar-header-container",
          p("Top Destinations"),
              
              # Expanded Choices
          selectInput("sort_by", label = NULL, 
            choices = c(
              "Price (Low-High)" = "price_asc", 
              "Price (High-Low)" = "price_desc",
              "Distance (Near-Far)" = "dist_asc",
              "Distance (Far-Near)" = "dist_desc"
            ),
            width = "140px")
        ),
        div(class="fade-top"),
        div(class="container",
          uiOutput("sidebar_results")
        ),
        div(class="fade-bottom"),
        img(src="icons/chevron_right.svg", id="close_sidebar_icon")
      ),

      # div(id="statbar", class = "closed",
      #   # div(class="container",
      #     uiOutput("statbar_info"),
      #     # plotlyOutput("return_plot"),
      #     # plotlyOutput("departure_plot"),
      #   # ),
      #   img(src="icons/chevron_right.svg", id="close_statbar_icon")
      # )
      div(id="statbar", class = "closed",
    
        # 2. THE SKELETON (Replaces uiOutput("statbar_info"))
        # We keep the structure but add IDs to the elements we want to change
        tagList(
          tags$img(id="sb_img", src="", class="statbar-image", onerror="this.style.display='none'"),
          div(id="sb_verdict"),
          div(class = "container",
            
            div(id="stickyspacer"), # Spacer to allow for sticky header
            div(class = "sticky-header",
                div(class = "statbar-header",
                    h1(id="sb_city", "City"), # Placeholder text
                    div(id="sb_total", class = "total-price", "£0")
                ),
                div(class = "statbar-info",
                    h2(id="sb_country", "Country"),
                    div(id="sb_duration", class = "duration", "(0 days)")
                )
            ),
              
            div(id = "cost_breakdown",
              # Flight Row
              div(class = "breakdown-row",
                  div(class = "segment-type", "Flight"),
                  div(class = "breakdown-container",
                      div(id="width_flight", class = "cost-bar-segment flight", style = "width: 0%")
                  ),
                  div(id="val_flight", class = "segment-price", "£0")
              ),
              
              # Hotel Row
              div(class = "breakdown-row",
                  div(class = "segment-type", "Hotel"),
                  div(class = "breakdown-container",
                      div(id="width_hotel", class = "cost-bar-segment hotel", style = "width: 0%")
                  ),
                  div(id="val_hotel", class = "segment-price", "£0")
              ),
              
              # Living Row
              div(class = "breakdown-row",
                  div(class = "segment-type", "Living"),
                  div(class = "breakdown-container",
                      div(id="width_living", class = "cost-bar-segment living", style = "width: 0%")
                  ),
                  div(id="val_living", class = "segment-price", "£0")
              )
            ),

            div(id="introtext"),

            div(id = "statbar_plots",
              # Keep the plots here
              div(class="plot",
                h2(class="graphtitles", id="departure_plot_title", "Departure price fluctuations"),
                plotlyOutput("departure_plot", height = "200px"),
              ),
              div(class="plot",
                h2(class="graphtitles", id="return_plot_title", "Return price fluctuations"),
                plotlyOutput("return_plot", height = "200px"),
              ),
              div(class="plot",
                h2(class="graphtitles", id="cfd_plot_title", "Price Probability"),
                plotlyOutput("cfd_plot", height = "200px"),
              ),
              div(class="plot",
                h2(class="graphtitles", id="season_plot_title", "Seasonality (Cheapest Months)"),
                plotlyOutput("season_plot", height = "200px"),
              ),
              div(class="plot",
                h2(class="graphtitles", id="weekday_plot_title", "Best Day to Fly"),
                plotlyOutput("weekday_plot", height = "200px")
              ),
              div(class="plot",
                h2(class="graphtitles", "Living Cost Breakdown"),
                plotlyOutput("living_breakdown_plot", height = "200px"),
              )
            )
          )
        ),
        
        img(src="icons/close.svg", id="close_statbar_icon"),
        div(id="maximise_statbar_icon",
          img(src="icons/maximise.svg"),
        ),
        div(id="minimise_statbar_icon",
          img(src="icons/minimise.svg"),
        )
        
      )
    )
  )
}
