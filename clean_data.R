library(tidyverse)
library(lubridate)

raw_data <- read_csv(
  file = "Shared_Micromobility_Vehicle_Trips.csv",
  col_names = c("id", "device_id", "vehicle_type", "trip_duration",
                "trip_distance", "start_time", "end_time", "modified_date",
                "month", "hour", "day_of_week", "start_district",
                "end_district", "year", "start_census", "end_census"),
  col_types = cols_only(
    #id = col_character(),
    #device_id = col_character(),
    vehicle_type = col_factor(levels = c("bicycle", "scooter")),
    trip_duration = col_double(),
    trip_distance = col_double(),
    start_time = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
    end_time = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
    #modified_date = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
    month = col_double(),
    hour = col_double(),
    day_of_week = col_double(),
    start_district = col_character(),
    end_district = col_character(),
    year = col_character()
    #start_census = col_character(),
    #end_census = col_character()
  ),
  locale = locale(tz = "US/Central"),
  #n_max = 10000,
  skip = 1
)

glimpse(raw_data)

# First Monday of each year.
start_date <- c(
  "2019" = mdy("01-07-2019"),
  "2020" = mdy("01-06-2020")
)

# Date same number of days after start for each year.
# TODO: May want to do something like start_date[[year]] + days(90) instead.
end_date <- c(
  "2019" = mdy("04-08-2019"),
  "2020" = mdy("04-06-2020")
)


# start_date <- c(
#   "2019" = mdy("02-01-2019"),
#   "2020" = mdy("02-01-2020")
# )
# 
# end_date <- c(
#   "2019" = mdy("02-28-2019"),
#   "2020" = mdy("02-29-2020")
# )


# Check if a given date is in the pertinent date range for the given year.
in_date_range <- function(year, date_time) {
  date_time >= start_date[[year]] & date_time <= end_date[[year]]
}

data <- raw_data %>%
  filter(in_date_range("2019", start_time) | in_date_range("2020", start_time))

# Keep track of how many observations are deleted by the cleaning process.
n_before_cleaning <- nrow(data)
data <- data %>%
  filter(
    # TODO: May want to remove other weird trip distances, durations, etc.
    # TODO: Modify conditions here to get agreement with Bob's numbers.
    trip_distance > 0,
    trip_duration > 0,
    
    # TODO: Needs to be properly justified.
    trip_distance <= 80000,
    trip_duration <= 43200,
    
    #start_district != "0", end_district != "0",
    # TODO: Needs to be properly justified.
    trip_distance / trip_duration * 3.6 <= 50
  ) %>%
  mutate(
    # TODO: Rounding issue?
    #day_offset = floor((start_date[year] %--% start_time) / ddays(1)),
    #week_offset = floor((start_date[year] %--% start_time) / dweeks(1))
    day_offset = (start_date[year] %--% date(start_time)) / ddays(1)
  )

# TODO: Verify that counts correspond with Bob's data.
count_data <- data %>%
  group_by(year) %>%
  #count(date(start_time))
  count(day_offset)

data %>%
  filter(day_offset >= 75, year == "2020") %>%
  count(day_offset)

ggplot(data, aes(x = day_offset, col = year, linetype = vehicle_type)) +
  #geom_histogram(binwidth = 1, alpha = 0.5, position = "identity")
  geom_freqpoly(binwidth = 1)

scooter_data <- data %>%
  filter(vehicle_type == "scooter")

ggplot(scooter_data, aes(x = day_offset, col = year)) +
  geom_freqpoly(binwidth = 1)

bicycle_data <- data %>%
  filter(vehicle_type == "bicycle")

ggplot(bicycle_data, aes(x = day_offset, col = year)) +
  geom_freqpoly(binwidth = 1)


count_data <- data %>%
  group_by(year, vehicle_type) %>%
  #count(date(start_time))
  count(day_offset)


# TODO
ggplot(data, aes(col = factor(week_offset))) +
  geom_freqpoly(binwidth = 1, alpha = 0.5, aes(x = hour), pad = FALSE) +
  facet_wrap(~ year)
  #geom_histogram(binwidth = 1)
