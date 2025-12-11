
ui <- function() {
  fluidPage(
    useShinyjs(),
    # javascript to handle clicks and animations
    tags$script(HTML("
    var dimension = [0, 0];
      $(document).on('shiny:connected', function() {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange('dimension', dimension);
      });
      $(window).resize(function() {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange('dimension', dimension);
      });

    $(document).on('click', '#close_sidebar_icon', function() {
      $('#sidebar').removeClass('closed');
      $('#map_container').removeClass('sidebar');
      Shiny.setInputValue('sidebar_clicked', new Date().getTime());
    });

    $(document).on('click', '#close_statbar_icon', function() {
      $('#statbar').addClass('closed');
      $('#statbar').removeClass('maximised');
      $('#map_container').removeClass('compressed');
      $('#map_container').removeClass('statbar');
      Shiny.setInputValue('statbar_clicked', new Date().getTime());
    });

    $(document).on('click', '#maximise_statbar_icon', function() {
      $('#statbar').addClass('maximised');
      $('#map_container').addClass('compressed');
      Shiny.setInputValue('maximise_statbar_clicked', new Date().getTime());
    });

    $(document).on('click', '#minimise_statbar_icon', function() {
      $('#statbar').removeClass('maximised');
      $('#map_container').removeClass('compressed');
      Shiny.setInputValue('minimise_statbar_clicked', new Date().getTime());
    });

    $(document).on('click', '#map_open', function() {
      $('#statbar').removeClass('maximised');
      $('#map_container').removeClass('compressed');
      Shiny.setInputValue('statbar_clicked', new Date().getTime());
    });

    // detect clicks on specific sidebar cards
    $(document).on('click', '.sidebaritem', function() {
      var cityId = $(this).data('city');
      Shiny.setInputValue('sidebar_item_clicked', cityId, {priority: 'event'});
    });

    // javascript to update text/bars inside the statbar
    Shiny.addCustomMessageHandler('update_statbar', function(data) {
      $('#sb_city').text(data.city.trim());
      $('#verdict_city').text(data.city.trim() + '.');
      $('#sb_country').text(data.country);
      $('#sb_total').text('£' + data.total_cost);
      $('#sb_duration').text('(' + data.duration + ' days)');
      $('#introtext').text(data.wiki_intro);
      if (data.verdict == '') {
          $('#sb_verdict').css('display', 'none');
      } else {
        $('#sb_verdict').css('display', 'block');
          $('#verdict_text').text(data.verdict);
      }
      $('#verdict_text').text(data.verdict);
      
      $('#sb_img').attr('src', 'statbar_images/' + data.city + '.png').css('display', 'block');

      $('#val_flight').text('£' + data.flight_cost);
      $('#val_hotel').text('£' + data.hotel_cost);
      $('#val_living').text('£' + data.living_cost);

      $('#width_flight').css('width', data.flight_pct + '%');
      $('#width_hotel').css('width', data.hotel_pct + '%');
      $('#width_living').css('width', data.living_pct + '%');
      $('#sb_verdict').css('background-color', data.verdict_colour);
    });
")),
    includeCSS("www/styles.css"),
    tagList(
      # top navigation and filters
      div(id="menu_bar",

        div(class="container",
          
          # logo
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

            div(id = "date_selection", class = "filter-section",
              dateInput(inputId = "departure_date", label=NULL),
              dateInput(inputId = "return_date", label=NULL)
            ),

            div(id = "budget_selection", class = "filter-section",
    
              # manual inputs and slider sync
              div(class = "budget-input-box",
                numericInput("budget_min", label = NULL, value = 0, width = "100%")
              ),

              div(class = "budget-slider-container",
                sliderInput("budget", label = NULL, min = 0, max = 2000, value = c(1000, 1500), step = 50, ticks = FALSE, pre = "£")
              ),

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

      # main map area
      div(id = "map_container", 
        img(src="icons/map.svg", id="map_open"),
        div(class="container",
          leafletOutput("map", width = "100%", height = "100%")
        )
      ),
      
      # left sidebar for results
      div(id = "sidebar", class = "closed",
        div(class = "sidebar-header-container",
          p("Top Destinations"),
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

      # right sidebar for detailed stats
      div(id="statbar", class = "closed",
    
        # skeleton structure
        tagList(
          tags$img(id="sb_img", src="", class="statbar-image", onerror="this.style.display='none'"),
          div(id="sb_verdict",
            "This is ", span(id="verdict_text"), " price for", span(id="verdict_city")
          ),
          div(class = "container",
            
            div(id="stickyspacer"), 
            div(class = "sticky-header",
                div(class = "statbar-header",
                    h1(id="sb_city", "City"), 
                    div(id="sb_total", class = "total-price", "£0")
                ),
                div(class = "statbar-info",
                    h2(id="sb_country", "Country"),
                    div(id="sb_duration", class = "duration", "(0 days)")
                )
            ),
              
            # visual cost bars
            div(id = "cost_breakdown",
              div(class = "breakdown-row",
                  div(class = "segment-type", "Flight"),
                  div(class = "breakdown-container",
                      div(id="width_flight", class = "cost-bar-segment flight", style = "width: 0%")
                  ),
                  div(id="val_flight", class = "segment-price", "£0")
              ),
              
              div(class = "breakdown-row",
                  div(class = "segment-type", "Hotel"),
                  div(class = "breakdown-container",
                      div(id="width_hotel", class = "cost-bar-segment hotel", style = "width: 0%")
                  ),
                  div(id="val_hotel", class = "segment-price", "£0")
              ),
              
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
              # placeholders for the graphs
              div(class="plot",
                h2(class="graphtitles", id="departure_plot_title", "Departure price fluctuations"),
                withSpinner(
                  plotlyOutput("departure_plot",
                    height = "200px"
                  ),
                    type = 1,
                    color = "#5B97EC",
                    size = 0.5
                  ),
                  textOutput("departure_desc")
              ),
              div(class="plot",
                h2(class="graphtitles", id="return_plot_title", "Return price fluctuations"),
                withSpinner(
                  plotlyOutput("return_plot",
                    height = "200px"
                  ),
                  type = 1,
                  color = "#5B97EC",
                  size = 0.5
                ),
                textOutput("return_desc")
              ),
              div(class="plot",
                h2(class="graphtitles", id="cfd_plot_title", "Price Probability"),
                withSpinner(
                  plotlyOutput("cfd_plot",
                    height = "200px"
                  ),
                  type = 1,
                  color = "#5B97EC",
                  size = 0.5
                ),
                textOutput("cfd_desc")
              ),
              div(class="plot",
                h2(class="graphtitles", id="season_plot_title", "Seasonality (Cheapest Months)"),
                withSpinner(
                  plotlyOutput("season_plot",
                    height = "200px"
                  ),
                  type = 1,
                  color = "#5B97EC",
                  size = 0.5
                ),
                textOutput("season_desc")
              ),
              div(class="plot",
                h2(class="graphtitles", id="weekday_plot_title", "Best Day to Fly"),
                withSpinner(
                  plotlyOutput("weekday_plot",
                    height = "200px"
                  ),
                  type = 1,
                  color = "#5B97EC",
                  size = 0.5
                ),
                textOutput("weekday_desc")
              ),
              div(class="plot",
                h2(class="graphtitles", "Living Cost Breakdown"),
                withSpinner(
                  plotlyOutput("living_breakdown_plot",
                    height = "200px"
                  ),
                  type = 1,
                  color = "#5B97EC",
                  size = 0.5
                ),
                textOutput("living_desc")
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