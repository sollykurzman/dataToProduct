library(shiny)
# library(bslib)
library(leaflet)

ui_main <- function() {
  fluidPage(
    includeCSS("www/styles.css"),
    tagList(
      div(id="sidebar",

        div(class="container",

          div(
            id="sidebar-close-button",
            img(src="icons/chevron_right.svg", id="sidebar-toggle"),
          ),
            
          
          div(id="header",
            class="filter-section",
            img(src="icons/airplane.svg"),
            div(id="title",
              h1("GoLow"),
              h2("Adventure by budget")
            )
          ),

          div(class = "filter-section",
            img(src="icons/airplane_departure.svg", id="airport_selection_icon"),
            selectizeInput(inputId = "departure_airport", label = NULL, choices = NULL, selected = NULL, multiple = FALSE, options = list(placeholder = "Type to search..."))
          ),

          h3("Select Dates"),
          div(class = "filter-section",
            dateInput(inputId = "departure_date", label=NULL),
            dateInput(inputId = "return_date", label=NULL)
          ),

          h3("Set Budget"),
            div(class = "filter-section",
            sliderInput("budget", label=NULL, min=200, max=1000, value=c(300,600), step=50, pre="£")
          ),

          div(id = "search_button_container", class = "filter-section",
            actionButton("search_btn", "Search Destinations", class="search-button")
          )
          

        )
      ),
      leafletOutput("map", width = "100%", height = "100%")
    )
  )
}
