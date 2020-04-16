
# Data loading/initial processing -----------------------------------------

load_data <- function(path) {
  raw_data <- read_csv(
    file = path,
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
}

# start_date <- c(
#   "2019" = sxsw_2019_dates[["end"]],
#   "2020" = (sxsw_2019_dates[["end"]] - mdy("01-01-2019")) + mdy("01-01-2020")
# )

# First Monday of each year.
first_monday_date <- c(
  "2019" = mdy("01-07-2019"),
  "2020" = mdy("01-06-2020")
)

# Date same number of days after start for each year.
# TODO: May want to do something like start_date[[year]] + days(90) instead.
# end_date <- c(
#   "2019" = mdy("04-08-2019"),
#   "2020" = mdy("04-06-2020")
# )

clean_data <- function(raw_data) {
  raw_data %>%
    filter(
      trip_distance > 0,
      trip_duration > 0,
      
      # TODO: Needs to be properly justified.
      trip_distance <= 80000,  # 80 km
      trip_duration <= 43200,  # 12 hours
      trip_distance / trip_duration * 3.6 <= 50,  # 50 km/hour
      
      start_district != "0", end_district != "0",
    ) %>%
    mutate(
      day_offset = (first_monday_date[year] %--% date(start_time)) / ddays(1)
    )
}

restrict_day_offset <- function(data_all_dates,
                                start_offset = 0, end_offset = 366) {
  # Check if a given date is in the pertinent date range for the given year.
  # in_date_range <- function(year, date_time) {
  #   date_time >= start_date[[year]] & date_time <= end_date[[year]]
  # }
  # 
  # data_all_dates %>%
  #   filter(
  #     in_date_range("2019", start_time) | in_date_range("2020", start_time)
  #   )
  
  data_all_dates %>%
    filter(day_offset >= start_offset & day_offset <= end_offset)
}

# Plotting ----------------------------------------------------------------

plot_frequencies <- function(data, years = c("2019", "2020"),
                             vehicle_types = c("scooter", "bicycle"),
                             label_increment = NULL) {
  data <- data %>%
    filter(vehicle_type %in% vehicle_types, year %in% years)
  
  start_offset = min(data$day_offset)
  end_offset = max(data$day_offset)
  
  plot <- ggplot(data, aes(x = day_offset, col = year,
                           linetype = vehicle_type)) +
    geom_freqpoly(binwidth = 1) +
    geom_vline(
      xintercept = seq(0, end_offset, by = 7),
      linetype = "longdash",
      alpha = 0.3
    ) +
    coord_cartesian(xlim = c(start_offset, end_offset))
  
  if (!is.null(label_increment)) {
    plot <- plot +
      scale_x_continuous(
        breaks = seq(start_offset, end_offset, by = label_increment)
      )
  }
  
  plot
}
