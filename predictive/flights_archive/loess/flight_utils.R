
get_flight_prediction <- function(origin, dest, trip_date, model = .global_model) {
  origin <- str_trim(toupper(origin))
  dest   <- str_trim(toupper(dest))
  
  # check if model loaded
  if (is.null(model)) stop("Model not loaded! Make sure flight_model.rds is in the folder.")
  
  t_date <- ymd(trip_date)
  d_ahead <- as.numeric(t_date - Sys.Date())
  
  t_month <- as.character(month(t_date, label = TRUE))
  t_day   <- as.character(wday(t_date, label = TRUE))

  # seasonality
  l1 <- model$l1 %>% filter(From == origin, To == dest, as.character(Month) == t_month)
  if (nrow(l1) == 0) {
    l1 <- model$l1 %>% filter(From == dest, To == origin, as.character(Month) == t_month)
    print(paste0("Swapped route for seasonality: ", dest, " -> ", origin))
  }

  if (nrow(l1) == 0) {
    stop("No seasonality data found for this route and month.")
  } else {
    val_1 <- l1$base_price
  }
  
  # weekday
  l2 <- model$l2 %>% filter(From == origin, To == dest, as.character(Weekday) == t_day)
  if (nrow(l2) == 0) {
    l2 <- model$l2 %>% filter(From == dest, To == origin, as.character(Weekday) == t_day)
  }

  val_2 <- ifelse(nrow(l2) == 1, l2$day_modifier, 0)
  
  # bookign curve
  val_3 <- suppressWarnings(predict(model$l3, newdata = data.frame(days_ahead = d_ahead)))
  if(is.na(val_3)) val_3 <- 0

  # results
  final_price <- max(val_1 + val_2 + val_3, 10)

  # Generate CFD data (probabilistic stuff)
  # we simulate 10k futures using historical errors
  sim_prices <- final_price + model$residuals
  sim_prices <- sim_prices[sim_prices >= 10] 
  safe_price <- quantile(sim_prices, 0.90)

  sim_prices <- final_price + model$residuals
  
  return(list(
    price = round(final_price, 2),
    safe_price = round(safe_price, 2),
    cfd_data = data.frame(Price = sim_prices)
  ))
}

get_route_trends <- function(origin, dest, model = .global_model) {
  origin <- str_trim(toupper(origin))
  dest   <- str_trim(toupper(dest))
  
  if (is.null(model)) stop("Model not loaded!")

  # --- 1. Get Seasonality (Month) ---
  # Try direct route
  l1_subset <- model$l1 %>% filter(From == origin, To == dest)
  
  # If empty, try swapped route (assuming seasonality is symmetric)
  if (nrow(l1_subset) == 0) {
    l1_subset <- model$l1 %>% filter(From == dest, To == origin)
  }
  
  # If still empty, return generic flat line (safety)
  if (nrow(l1_subset) == 0) {
    l1_subset <- data.frame(Month = factor(month.abb, levels=month.abb), base_price = 0)
  } else {
    # Ensure all months are present for the plot, even if missing in data
    # (Optional, but makes graphs look better)
    l1_subset <- l1_subset %>% arrange(match(Month, month.abb))
  }

  # --- 2. Get Weekday Trends ---
  # Try direct route
  l2_subset <- model$l2 %>% filter(From == origin, To == dest)
  
  # If empty, try swapped route
  if (nrow(l2_subset) == 0) {
    l2_subset <- model$l2 %>% filter(From == dest, To == origin)
  }
  
  # Return structured data for plotting
  return(list(
    seasonality = l1_subset, # Columns: Month, base_price
    weekday = l2_subset      # Columns: Weekday, day_modifier
  ))
}

path_to_check <- "predictive/flight_model.rds"

if (file.exists(path_to_check)) {
    # Check Project Root
    .global_model <<- readRDS(path_to_check)
} else {
    warning("Model not found.")
}

# test:
# result <- get_flight_prediction("STO", "BCN", "2026-04-15")
# print(result$price)
# print(result$safe_price)
# head(result$cfd_data)   