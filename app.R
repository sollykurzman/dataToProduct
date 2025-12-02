library(shiny)

# Load app components
source("R/data.R")
source("R/utils_paths.R")
source("R/utils_logic.R")
source("R/ui_main.R")
source("R/server_main.R")

# Run the app
shinyApp(
  ui = ui_main(),
  server = server_main
)


# shinyApp(
#   ui = fluidPage(
#     dateInput("test_date", "Test date")
#   ),
#   server = function(input, output, session) {}
# )