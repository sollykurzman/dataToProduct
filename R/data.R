library(dplyr)

# load up the locations file
locations <- read.csv("data/locations.csv", stringsAsFactors = FALSE)
locations <- locations %>%
  rename(
    city = CityName,
    code = AirportCode,
    country = Country,
    lat = Latitude,
    lon = Longitude
  )

# separate into valid departures
departure_airports <- locations %>%
select(city, code, country, lat, lon)

# generate some fake prices for the cities
cities <- locations %>%
mutate(price = sample(300:600, n(), replace = TRUE)) %>%
select(city, country, lat, lon, price)