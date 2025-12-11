library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)

# 1. Load resources
if(file.exists("hotel_utils.R")) {
  source("hotel_utils.R")
} else {
  tryCatch(source("predictive/hotel_utils.R"), error=function(e) warning("Could not find hotel_utils.R"))
}

# Load locations and rename consistently
locations <- read.csv("data/locations.csv", stringsAsFactors = FALSE)
names(locations) <- c("city", "code", "country", "lat", "lon")

# 2. Setup simulation (Next 365 days)
simulation_dates <- seq(from = Sys.Date(), by = "day", length.out = 365)

print("Starting hotel price simulation... (This takes a moment)")

# 3. Calculate Nightly Rate Thresholds
hotel_thresholds <- locations %>%
  mutate(stats = map(code, function(city_code) {
    
    # Get the price for a 1-night stay for every single day of the year
    yearly_nightly_rates <- map_dbl(simulation_dates, function(d) {
      tryCatch({
        # Book from Today -> Tomorrow (1 Night)
        cost <- get_total_hotel_cost(city_code, d, d + 1)
        if(is.numeric(cost)) return(cost) else return(NA)
      }, error = function(e) NA)
    })
    
    # Remove failures
    yearly_nightly_rates <- yearly_nightly_rates[!is.na(yearly_nightly_rates)]
    
    if(length(yearly_nightly_rates) == 0) return(list(low=NA, med_low=NA, med_high=NA, high=NA))
    
    # Calculate quantiles for a SINGLE NIGHT
    qs <- quantile(yearly_nightly_rates, probs = c(0.2, 0.4, 0.6, 0.8))
    
    return(list(
      h_low      = qs["20%"],
      h_med_low  = qs["40%"],
      h_med_high = qs["60%"],
      h_high     = qs["80%"]
    ))
  })) %>%
  unnest_wider(stats) %>%
  select(code, h_low, h_med_low, h_med_high, h_high)

# 4. Save
saveRDS(hotel_thresholds, "data/hotel_thresholds.rds")
print("Done! Saved to data/hotel_thresholds.rds")