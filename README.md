# GoLow: Adventure by Budget

**GoLow** is a budget-first travel discovery tool built with R Shiny.

This project addresses the problems of traditional destination-first holiday searching—such as repetitive visitation habits, herd mentality leaving some locations over-visited while others remain unexplored, and budgetary inflexibility that fails to account for hidden costs like food and activities.

GoLow solves this by **inverting the traditional searching process**. Instead of picking a destination first, users define a departure airport, a total budget, and travel dates. The product returns a curated selection of European destinations that fall within this budget. Crucially, the cost presented combines **predicted flight and hotel prices** with **living costs** (food, drink, transport) for the duration of the trip, allowing users to identify unique destinations with a holistic, budget-first approach.

## Installation & Requirements

This project uses **R** and **Shiny**. To ensure reproducibility and manage package versions, we use `renv`.

### Prerequisites
* [R](https://cran.r-project.org/) (Version 4.0.0 or higher recommended)
* [RStudio](https://posit.co/download/rstudio-desktop/) (Optional, but recommended)

### Setting up the Environment

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/sollykurzman/dataToProduct.git](https://github.com/sollykurzman/dataToProduct.git)
    cd golow
    ```

2.  **Restore the environment using `renv`:**
    Open the project in R/RStudio and run the following command in the console. This will automatically install all the required packages (Shiny, dplyr, plotly, leaflet, etc.) with the correct versions.
    ```r
    if (!require("renv")) install.packages("renv")
    renv::restore()
    ```
    
### Key Dependencies
* **Core:** `shiny`, `shinyjs`, `htmltools`, `shinyWidgets` - App framework, interactivity, and custom widgets.
* **Data Manipulation:** `tidyverse`, `dplyr`, `purrr`, `lubridate` - Data cleaning, piping, functional programming, and date handling.
* **Visualization:**
    * `leaflet` - Interactive maps.
    * `plotly` - Interactive graphs and dashboards.
    * `shinycssloaders` - Loading animations for UI elements.
* **Geospatial:** `sf` - Handling spatial geometry for flight paths.
* **Utilities:** `jsonlite` - JSON parsing for API responses.

## Project Structure

* **`app.R`**: The application entry point.
* **`R/`**: Contains the core application logic and utility scripts.
    * **`ui.R`**: User interface definition.
    * **`server.R`**: Server-side logic and event handling.
    * **`utils.R`**: General helper functions (e.g., map popups, date logic).
    * **`data.R`**: Data loading and cleaning scripts.
    * **`predictive/flight_utils.R`**: Logic for predicting flight prices and calculating probability curves.
    * **`hotel_utils.R`**: Logic for predicting hotel costs.
    * **`living_utils.R`**: Logic for calculating daily living expenses (food, transport).
* **`www/`**: Static assets.
    * **`styles.css`**: Custom CSS styling for the dashboard.
    * **`icons/`**: UI icons (SVG/PNG).
    * **`sidebar_images/`**: Thumbnails for destination results.
    * **`statbar_images/`**: Images for the detailed statistics sidebar.
* **`data/`**: Static datasets (e.g., `locations.csv`) and generated `.rds` benchmarks.
* **`predictive/`**: Contains trained model files (`.rds`) and benchmark generation scripts.
