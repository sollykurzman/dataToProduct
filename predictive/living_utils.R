living_data <- read.csv("data/livingData.csv")

# lookup for city name conversion
city_lookup <- c(
  "AMS" = "Amsterdam", "BCN" = "Barcelona", "BER" = "Berlin",
  "CPH" = "Copenhagen", "DUB" = "Dublin", "EDI" = "Edinburgh",
  "GLA" = "Glasgow", "LON" = "London", "MAD" = "Madrid",
  "MUC" = "Munich", "PAR" = "Paris", "PRG" = "Prague",
  "STO" = "Stockholm", "TLL" = "Tallinn", "VIE" = "Vienna",
  "ZRH" = "Zurich"
)

scrape_date <- as.Date("2025-12-08")  

get_total_living_cost <- function(shortcode, arrival_date, leaving_date) {
  
  shortcode   <- str_trim(toupper(shortcode))
  
  # convert city name
  city_name <- city_lookup[shortcode]
  if(is.na(city_name)) return("Error: Invalid City Shortcode")
  
  stay_dates <- seq(from = as.Date(arrival_date), 
                    to = as.Date(leaving_date) - 1, 
                    by = "day")

  city_data <- living_data %>% 
    filter(trimws(tolower(city)) == trimws(tolower(city_name)))
  
  if (nrow(city_data) == 0) {
    return(paste("Error: City", city_name, "not found."))
  }
  
  days <- as.numeric(difftime(as.Date(leaving_date), as.Date(arrival_date), units = "days"))
  
  if (days < 1) {
    return("Error: End date must be after start date.")
  }
  
  # basket of goods calculation
  
  cost_breakfast <- city_data$`Cappuccino..Regular.Size.` + (city_data$`Meal.at.an.Inexpensive.Restaurant` * 0.5)
  
  cost_lunch <- city_data$`Meal.at.an.Inexpensive.Restaurant`
  
  cost_dinner <- city_data$`Meal.for.Two.at.a.Mid.Range.Restaurant..Three.Courses..Without.Drinks.` / 2
  
  cost_water <- city_data$`Bottled.Water..1.5.Liter.` * 1

  cost_beer <- city_data$`Domestic.Beer..0.5.Liter.Bottle.` * 2
  
  # assume some taxi and transport use
  cost_transport <- (city_data$`One.Way.Ticket..Local.Transport.` * 3) + (city_data$`Taxi.Start..Standard.Tariff.`) + (city_data$`Taxi.1.km..Standard.Tariff.` * 5)
  
  # general activity cost
  cost_activities <- city_data$`Cinema.Ticket..International.Release.`
  
  daily_total <- cost_breakfast + cost_lunch + cost_dinner + cost_water + cost_beer + cost_transport + cost_activities
  
  total_holiday_cost <- daily_total * days

  # adjust for inflation since scrape date
  days_difference <- as.numeric(as.Date(arrival_date) - scrape_date)

  total_holiday_cost <- total_holiday_cost * (1.022^(days_difference/365))
  
  return(list(
    diration = days,
    daily = round(daily_total, 2),
    total = round(total_holiday_cost, 2),
    breakdown = list(
      food = round(cost_breakfast + cost_lunch + cost_dinner, 2),
      drink = round(cost_beer + cost_water, 2),
      transport = round(cost_transport, 2),
      activities = round(cost_activities, 2)
    )
  ))
}