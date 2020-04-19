
# Loading the data --------------------------------------------------------

load_data <- function(path) {
  date_time_format <- "%m/%d/%Y %I:%M:%S %p"
  
  raw_data <- read_csv(
    file = path,
    col_names = c("id", "device_id", "vehicle_type", "trip_duration",
                  "trip_distance", "start_time", "end_time", "modified_date",
                  "month", "hour", "day_of_week", "start_district",
                  "end_district", "year", "start_census", "end_census"),
    col_types = cols_only(
      id = col_character(),
      device_id = col_character(),
      # vehicle_type = col_factor(levels = c("bicycle", "scooter")),
      vehicle_type = col_character(),
      trip_duration = col_double(),
      trip_distance = col_double(),
      start_time = col_datetime(format = date_time_format),
      end_time = col_datetime(format = date_time_format),
      # modified_date = col_datetime(format = date_time_format),
      # month = col_double(),
      # hour = col_double(),
      # day_of_week = col_double(),
      start_district = col_character(),
      end_district = col_character(),
      year = col_character()
      # start_census = col_character(),
      # end_census = col_character()
    ),
    locale = locale(tz = "US/Central"),
    skip = 1
  )
  
  raw_data <- raw_data %>%
    mutate(
      trip_duration_hours = trip_duration / 3600,
      trip_distance_km = trip_distance / 1000
    )
  
  raw_data
}

# Initial exploration -----------------------------------------------------

# plot_trip_duration <- function(raw_data) {
#   ggplot(raw_data %>% slice(1:10000), aes(x = trip_duration_hours)) +
#     geom_histogram(binwidth = 1)
# }
# 
# plot_trip_distance <- function(raw_data) {
#   ggplot(raw_data, aes(x = trip_distance_km)) +
#     geom_histogram(binwidth = 10)
# }

# Data cleaning -----------------------------------------------------------

# First Monday of each year.
first_monday_date <- c(
  "2019" = mdy("01-07-2019"),
  "2020" = mdy("01-06-2020")
)

# TODO: May want to make start_district and end_district factors.
clean_data <- function(raw_data) {
  council_districts <- as.character(1:10)
  
  data <- raw_data %>%
    drop_na() %>%
    filter(
      year(start_time) %in% c("2019", "2020"),
      year(end_time) %in% c("2019", "2020"),
      trip_distance > 0,
      trip_duration > 0,
      trip_distance_km <= 80,
      trip_duration_hours <= 12,
      trip_distance_km / trip_duration_hours <= 50,  # Average speed (km/hour)
      start_district %in% council_districts,
      end_district %in% council_districts
    ) %>%
    mutate(
      day_offset = (first_monday_date[year] %--% date(start_time)) / ddays(1)
    )
  
  data
}

restrict_day_offset <- function(data_all_dates, start_offset, end_offset) {
  data_all_dates %>%
    filter(day_offset >= start_offset & day_offset <= end_offset)
}

# Exploration after cleaning ----------------------------------------------

# TODO: For next two functions, could also just change the limits on the x-axis
# rather than filter the data.

plot_trip_distance <- function(data, years = c("2019", "2020"),
                               vehicle_types = c("scooter", "bicycle"),
                               max_distance = Inf) {
  
  data <- data %>%
    filter(trip_distance_km <= max_distance)
  
  ggplot(data, aes(x = trip_distance_km,
                   col = year, linetype = vehicle_type)) +
    geom_freqpoly(binwidth = 0.1)
}

plot_trip_duration <- function(data, years = c("2019", "2020"),
                               vehicle_types = c("scooter", "bicycle"),
                               max_duration = Inf) {
  
  data <- data %>%
    filter(trip_duration_hours <= max_duration)
  
  ggplot(data, aes(x = trip_duration_hours,
                   col = year, linetype = vehicle_type)) +
    geom_freqpoly(binwidth = 1/12)  # 5 minute intervals
}

# TODO: Ensure that council districts with no corresponding observations aren't
# dropped.
plot_council_districts <- function(data, year, vehicle_type) {
  data <- data %>%
    filter(year == !!year, vehicle_type == !!vehicle_type)
  
  data %>%
    count(start_district, end_district) %>%
    ggplot(aes(x = start_district, y = end_district)) +
      geom_tile(aes(fill = n))
}

plot_start_times <- function(data, year, vehicle_type, part_of_week) {
  data <- data %>%
    filter(year == !!year, vehicle_type == !!vehicle_type)
  
  # TODO: Should be done in the data cleaning function.
  data <- data %>%
    mutate(
      day_of_week = wday(start_time, week_start = 1),
      month_label = month(start_time, label = TRUE),
      #day_of_week_name = wday(start_time, week_start = 1, label = TRUE),
      hours_since_midnight = hour(start_time) + minute(start_time) / 60
        + second(start_time) / 3600,
      holiday = FALSE  # TODO: Deal with holidays.
    )
  
  if (part_of_week == "weekdays") {
    data <- data %>%
      filter(day_of_week <= 5)
  }
  else if (part_of_week == "weekends") {
    data <- data %>%
      filter(day_of_week > 5)
  }
  
  ggplot(data, aes(x = hours_since_midnight, y = month_label)) +
    geom_density_ridges(scale = 0.9)
}

compute_count_data <- function(data, years = c("2019", "2020"),
                               vehicle_types = c("scooter", "bicycle")) {
  
  start_offset = min(data$day_offset)
  end_offset = max(data$day_offset)
  
  data <- data %>%
    mutate(
      day_offset = factor(day_offset, levels = start_offset:end_offset)
    ) %>%
    count(year, vehicle_type, day_offset, .drop = FALSE) %>%
    mutate(day_offset = as.integer(levels(day_offset))[day_offset]) %>%
    filter(vehicle_type %in% vehicle_types, year %in% years)
  
  data
}

plot_frequencies <- function(data, years = c("2019", "2020"),
                             vehicle_types = c("scooter", "bicycle"),
                             label_increment = NULL) {
  
  count_data <- compute_count_data(data, years, vehicle_types)
  start_offset = min(count_data$day_offset)
  end_offset = max(count_data$day_offset)
  
  plot <- ggplot(count_data, aes(x = day_offset, y = n,
                                 col = year, linetype = vehicle_type)) +
    geom_line() +
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
