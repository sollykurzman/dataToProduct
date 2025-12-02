    library(dplyr)

    locations <- read.csv("data/locations.csv", stringsAsFactors = FALSE)
    names(locations) <- c("city", "code", "country", "lat", "lon")

    departure_airports <- locations %>%
    select(city, code, country, lat, lon)

    cities <- locations %>%
    mutate(price = sample(300:600, n(), replace = TRUE)) %>%
    select(city, country, lat, lon, price)