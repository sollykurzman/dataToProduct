# loading libaries
library(tidyverse)
library(lubridate)

# function to clean the messy data
# basically just removes the pound sign and makes sure dates work
clean_flight_data <- function(df) {
  df %>%
    mutate(
      # get rid of the pound sign and commas or else R crashes lol
      numeric_price = as.numeric(gsub("[£,]", "", Price)), 
      
      # unifyingg/fixing date formats
      Date = ymd(Date),
      Scraped = ymd(Scraped),
      
      # how many days before flight
      days_ahead = as.numeric(Date - Scraped)
    ) %>%
    # filter out weird rows if price is missing
    # also remove flights super far away bc they r inaccurate
    filter(!is.na(numeric_price), days_ahead < 330) %>%
    
    # if there are duplicates take the cheapest one
    group_by(From, To, Date) %>%
    slice_min(numeric_price, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# loading the raw scraped csvs
raw_nov <- read_csv("../../data/flightData.csv")
raw_dec <- read_csv("../../data/flightData2.csv")

# cleaning them separately just to be safe
df_nov_clean <- clean_flight_data(raw_nov)
df_dec_clean <- clean_flight_data(raw_dec)

# stacking them together
master_df <- bind_rows(df_nov_clean, df_dec_clean)

# save to csv
write_csv(master_df, "../../data/master_flight_data.csv")
