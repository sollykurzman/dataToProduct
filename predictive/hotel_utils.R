

# load traine dmodels
if(file.exists("predictive/hotel_models.rds")) {
  model_list <- readRDS("predictive/hotel_models.rds")
} else {
  stop("Error: Please run '01_train_models.R' first.")
}

# lookup for city name conversion
city_lookup <- c(
  "AMS" = "Amsterdam", "BCN" = "Barcelona", "BER" = "Berlin",
  "CPH" = "Copenhagen", "DUB" = "Dublin", "EDI" = "Edinburgh",
  "GLA" = "Glasgow", "LON" = "London", "MAD" = "Madrid",
  "MUC" = "Munich", "PAR" = "Paris", "PRG" = "Prague",
  "STO" = "Stockholm", "TLL" = "Tallinn", "VIE" = "Vienna",
  "ZRH" = "Zurich"
)

# main func
get_total_hotel_cost <- function(shortcode, arrival_date, leaving_date) {

  shortcode   <- str_trim(toupper(shortcode))
  
  # convert city name
  city_name <- city_lookup[shortcode]
  if(is.na(city_name)) return("Error: Invalid City Shortcode")
  
  # prep stay dates
  stay_dates <- seq(from = as.Date(arrival_date), 
                    to = as.Date(leaving_date) - 1, 
                    by = "day")
  
  # create input df for predicitng
  input_data <- data.frame(DateObj = stay_dates) %>%
    mutate(
      Month = factor(month(DateObj)),
      DayOfWeek = factor(wday(DateObj))
    )
  
  # pick model
  specific_model <- model_list[[city_name]]
  
  # predict prices
  predicted_prices <- predict(specific_model, newdata = input_data)
  
  if(any(is.na(predicted_prices))) {
    return("Error: Prediction failed. Model may lack data for this month.")
  }
  
  # sum prices and round
  return(round(sum(predicted_prices), 2))
}


# test
# cost <- get_total_hotel_cost("BCN", "2026-01-14", "2026-12-19")
# print("Cost:")
# print(cost)