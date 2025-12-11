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
library(jsonlite)
library(shinycssloaders)

source("R/data.R")
source("predictive/flight_utils.R")
source("predictive/hotel_utils.R")
source("predictive/living_utils.R")
source("R/utils.R")
source("R/ui.R")
source("R/server.R")

shinyApp(
  ui = ui(),
  server = server
)