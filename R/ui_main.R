
ui_main <- function() {
  fluidPage(
    useShinyjs(),
    tags$script(HTML("
    $(document).on('click', '#close_sidebar_icon', function() {
      
      // 1. Toggle the class
      $('#sidebar').toggleClass('closed');
      $('#map_container').toggleClass('sidebar');
      
      // 2. Notify Shiny (Optional, keep if you have server logic)
      Shiny.setInputValue('sidebar_clicked', new Date().getTime());
    });

    $(document).on('click', '#close_statbar_icon', function() {
      
      // 1. Toggle the class
      $('#statbar').toggleClass('closed');
      $('#map_container').toggleClass('statbar');
      
      // 2. Notify Shiny (Optional, keep if you have server logic)
      Shiny.setInputValue('statbar_clicked', new Date().getTime());
    });

    // NEW: when a sidebar card is clicked
    $(document).on('click', '.sidebaritem', function() {
      var cityId = $(this).data('city');
      Shiny.setInputValue('sidebar_item_clicked', cityId, {priority: 'event'});
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
              selectizeInput(inputId = "departure_airport", label = NULL, choices = NULL, selected = NULL, multiple = FALSE, options = list(placeholder = "Type to search..."))
            ),

            # h3("Select Dates"),
            div(id = "date_selection", class = "filter-section",
              dateInput(inputId = "departure_date", label=NULL),
              dateInput(inputId = "return_date", label=NULL)
            ),

            # h3("Set Budget"),
              div(id = "budget_selection", class = "filter-section",
              sliderInput("budget", label=NULL, min=000, max=1000, value=c(0,600), step=50, pre="£")
            ),

            div(id = "search_button_container", class = "filter-section",
              actionButton("search_btn", "Search", class="search-button")
            )
          )
          

        )
      ),


      div(id = "map_container",
        div(class="container",
          leafletOutput("map", width = "100%", height = "100%")
        )
      ),
      
      div(id = "sidebar", class = "closed",
        p("Top Destinations"),
        div(class="fade-top"),
        div(class="container",
          uiOutput("sidebar_results")
        ),
        div(class="fade-bottom"),
        img(src="icons/chevron_right.svg", id="close_sidebar_icon")
      ),

      div(id="statbar", class = "closed",
        # div(class="container",
          uiOutput("statbar_info"),
          # plotlyOutput("return_plot"),
          # plotlyOutput("departure_plot"),
        # ),
        img(src="icons/chevron_right.svg", id="close_statbar_icon")
      )
    )
  )
}
