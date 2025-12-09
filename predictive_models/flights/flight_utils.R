
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
  }

  if (nrow(l1) == 0) {
    stop("No seasonality data found for this route and month.")
  } else {
    val_1 <- l1$base_price
  }
  
  # weekday
  l2 <- model$l2 %>% filter(From == origin, To == dest, as.character(Weekday) == t_day)
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

path_to_check <- "predictive_models/flights/flight_model.rds"

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