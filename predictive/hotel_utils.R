# load trained models
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

# predict total hotel price
get_total_hotel_cost <- function(shortcode, arrival_date, leaving_date) {

  shortcode   <- str_trim(toupper(shortcode))
  
  # convert city name
  city_name <- city_lookup[shortcode]
  if(is.na(city_name)) return("Error: Invalid City Shortcode")
  
  stay_dates <- seq(from = as.Date(arrival_date), 
                    to = as.Date(leaving_date) - 1, 
                    by = "day")
  
  # prepare data for the model
  input_data <- data.frame(DateObj = stay_dates) %>%
    mutate(
      Month = factor(month(DateObj)),
      DayOfWeek = factor(wday(DateObj))
    )
  
  specific_model <- model_list[[city_name]]
  
  # get price from model
  predicted_prices <- predict(specific_model, newdata = input_data)
  
  if(any(is.na(predicted_prices))) {
    return("Error: Prediction failed. Model may lack data for this month.")
  }
  
  return(round(sum(predicted_prices), 2))
}