library(shiny)
library(leaflet)
library(shinyjs)
library(lubridate)
library(htmltools)
library(sf)
library(plotly)
library(purrr)
library(tidyverse)
library(dplyr)
library(shinyWidgets)

source("R/data.R")
source("predictive_models/flights/flight_utils.R")
source("predictive_models/hotels/hotel_utils.R")
source("predictive_models/living_utils.R")
source("R/utils_logic.R")
source("R/ui_main.R")
source("R/server_main.R")

shinyApp(
  ui = ui_main(),
  server = server_main
)