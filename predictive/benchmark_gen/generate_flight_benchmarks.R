library(dplyr)
library(lubridate)
library(purrr)
library(tidyr) 

# 1. Load your resources
# Ensure your working directory is set to the project root
if(file.exists("flight_utils.R")) {
  source("flight_utils.R")
} else {
  tryCatch(source("predictive/flight_utils.R"), error=function(e) warning("Could not find flight_utils.R. Ensure functions are loaded."))
}

# --- FIX: Read and rename columns based on your specific CSV headers ---
locations <- read.csv("data/locations.csv", stringsAsFactors = FALSE)

# Rename strictly to match what the app expects
locations <- locations %>%
  rename(
    city = CityName,
    code = AirportCode,
    country = Country,
    lat = Latitude,
    lon = Longitude
  )

# Load the model manually if not already in environment
if (!exists("model") || is.null(model)) {
  if(file.exists("predictive/flight_model.rds")) {
    model <- readRDS("predictive/flight_model.rds")
  } else {
    stop("Could not find 'predictive/flight_model.rds'. Check your working directory.")
  }
}

# 2. Setup the simulation parameters
simulation_dates <- seq(from = Sys.Date(), by = "day", length.out = 365)
origin_code <- "LON" # Assuming London is your origin

print("Starting price simulation... (This might take a minute)")

# 3. Calculate thresholds
thresholds <- locations %>%
  filter(code != origin_code) %>% 
  mutate(stats = map(code, function(dest_code) {
    
    # Predict price for every day of the upcoming year
    yearly_prices <- map_dbl(simulation_dates, function(d) {
      tryCatch({
        get_flight_prediction(origin_code, dest_code, d)
      }, error = function(e) NA)
    })
    
    # Remove failures
    yearly_prices <- yearly_prices[!is.na(yearly_prices)]
    
    if(length(yearly_prices) == 0) return(list(low=NA, med_low=NA, med_high=NA, high=NA))

    # Calculate quantiles
    qs <- quantile(yearly_prices, probs = c(0.2, 0.4, 0.6, 0.8))
    
    return(list(
      low      = qs["20%"],
      med_low  = qs["40%"],
      med_high = qs["60%"],
      high     = qs["80%"]
    ))
  })) %>%
  unnest_wider(stats) %>%
  select(code, low, med_low, med_high, high)

# 4. Save the file
saveRDS(thresholds, "data/flight_thresholds.rds")
print("Done! Saved to data/flight_thresholds.rds")