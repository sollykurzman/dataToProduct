library(shiny)
library(leaflet)

ui_main <- function() {
  fluidPage(
  #   tags$script(HTML("
  #   $(document).on('click', '#close_sidebar_icon', function() {
  #     $('#sidebar').toggleClass('closed');
  #     $('#map').toggleClass('closed');
  #     Shiny.setInputValue('sidebar_clicked', new Date().getTime());

  #     setTimeout(function() {
  #      // Find the Shiny Leaflet widget by its ID ('map')
  #      var mapWidget = HTMLWidgets.find('#map');
       
  #       if (mapWidget) {
  #         // Access the underlying Leaflet instance and fix the size
  #         mapWidget.getMap().invalidateSize();
  #       }
  #     }, 0); // slightly longer than CSS transition to be safe
  #   });
  # ")),
  tags$script(HTML("
  $(document).on('click', '#close_sidebar_icon', function() {
    
    // 1. Toggle the class
    $('#sidebar').toggleClass('closed');
    $('#map_container').toggleClass('closed');
    
    // 2. Notify Shiny (Optional, keep if you have server logic)
    Shiny.setInputValue('sidebar_clicked', new Date().getTime());
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
              sliderInput("budget", label=NULL, min=200, max=1000, value=c(300,600), step=50, pre="£")
            ),

            div(id = "search_button_container", class = "filter-section",
              actionButton("search_btn", "Search", class="search-button")
            )
          )
          

        )
      ),


      div(id = "map_container", class = "closed",
        div(class="container",
          leafletOutput("map", width = "100%", height = "100%"),
        )
      ),
      
      div(id = "sidebar", class = "closed",
        div(class="container",
          img(src="icons/chevron_right.svg", id="close_sidebar_icon"),
        )
      )
    )
  )
}
