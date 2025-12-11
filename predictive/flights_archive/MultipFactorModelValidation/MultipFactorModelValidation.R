# ==============================================================================
# multiplicative_factor_lazy.r
# doing the manual multiplier model
# ==============================================================================

library(tidyverse)
library(lubridate)

# --- step 1: train model ---

# load the training csv
d <- read_csv("../dataToProduct/data/flightData.csv")

# clean up the messy data
d_clean <- d %>%
  mutate(
    price = as.numeric(gsub("[£,]", "", Price)),
    date = ymd(Date),
    scraped = ymd(Scraped),
    route = paste(From, To, sep = "-"),
    days = as.numeric(date - scraped),
    wd = wday(date, label = TRUE, week_start = 1),
    mon = month(date)
  ) %>%
  filter(!is.na(price), !is.na(days)) %>%
  filter(date < as.Date("2026-11-04")) 

# 1. get base price (median of stuff 60+ days out)
base_p <- d_clean %>%
  filter(days >= 60) %>%
  group_by(route) %>%
  summarise(base = median(price, na.rm = TRUE))

# function to split days into groups
get_b <- function(x) {
  cut(x, 
      breaks = c(-1, 7, 14, 21, 30, 60, 400),
      labels = c("0-7", "8-14", "15-21", "22-30", "31-60", "60+"),
      include.lowest = TRUE)
}

# 2. get booking window factors
book_fac <- d_clean %>%
  mutate(buck = get_b(days)) %>%
  left_join(base_p, by = "route") %>%
  mutate(fac = price / base) %>%
  group_by(route, buck) %>%
  summarise(b_mult = mean(fac, na.rm = TRUE), .groups = 'drop')

# normalize prices to find other factors
d_norm <- d_clean %>%
  mutate(buck = get_b(days)) %>%
  left_join(book_fac, by = c("route", "buck")) %>%
  mutate(
    b_mult = replace_na(b_mult, 1.0),
    norm_p = price / b_mult
  )

yearly <- d_norm %>%
  group_by(route) %>%
  summarise(y_mean = mean(norm_p, na.rm = TRUE))

# 3. get seasonal factors
seas_fac <- d_norm %>%
  group_by(route, mon) %>%
  summarise(m_mean = mean(norm_p, na.rm = TRUE), .groups='drop') %>%
  left_join(yearly, by = "route") %>%
  mutate(s_mult = m_mean / y_mean)

# 4. get weekday factors
wd_fac <- d_norm %>%
  group_by(route, wd) %>%
  summarise(w_mean = mean(norm_p, na.rm = TRUE), .groups='drop') %>%
  left_join(yearly, by = "route") %>%
  mutate(w_mult = w_mean / y_mean)


# --- step 2: check if it works (validation) ---

# load validation data
val <- read_csv("../dataToProduct/data/flightDataValidation.csv")

# same cleaning as above
val_c <- val %>%
  mutate(
    actual = as.numeric(gsub("[£,]", "", Price)),
    date = ymd(Date),
    scraped = ymd(Scraped),
    route = paste(From, To, sep = "-"),
    days = as.numeric(date - scraped),
    buck = get_b(days),
    mon = month(date),
    wd = wday(date, label = TRUE, week_start = 1)
  ) %>%
  filter(!is.na(actual))

# predict using the factors we made
preds <- val_c %>%
  left_join(base_p, by = "route") %>%
  left_join(book_fac, by = c("route", "buck")) %>%
  left_join(seas_fac, by = c("route", "mon")) %>%
  left_join(wd_fac, by = c("route", "wd")) %>%
  mutate(
    # fill nas with 1 so math doesnt break
    base = replace_na(base, NA), 
    b_mult = replace_na(b_mult, 1.0),
    s_mult = replace_na(s_mult, 1.0),
    w_mult = replace_na(w_mult, 1.0),
    
    # formula
    pred = base * b_mult * s_mult * w_mult
  ) %>%
  filter(!is.na(pred))

# check error
rmse <- sqrt(mean((preds$pred - preds$actual)^2))
mae <- mean(abs(preds$pred - preds$actual))

# print output
cat("validation results:\n")
cat(sprintf("rmse: %.2f\n", rmse))
cat(sprintf("mae:  %.2f\n", mae))