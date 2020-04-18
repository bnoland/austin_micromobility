
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
}

# Initial exploration -----------------------------------------------------

plot_trip_duration <- function(raw_data) {
  ggplot(raw_data, aes(x = trip_duration)) +
    geom_histogram()
}

plot_trip_distance <- function(raw_data) {
  ggplot(raw_data, aes(x = trip_distance)) +
    geom_histogram()
}

# Data cleaning -----------------------------------------------------------

# First Monday of each year.
first_monday_date <- c(
  "2019" = mdy("01-07-2019"),
  "2020" = mdy("01-06-2020")
)

clean_data <- function(raw_data) {
  # TODO: May want more rigorous filtering.
  raw_data %>%
    filter(
      year(start_time) %in% c("2019", "2020"),
      year(end_time) %in% c("2019", "2020"),
      
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

restrict_day_offset <- function(data_all_dates, start_offset, end_offset) {
  data_all_dates %>%
    filter(day_offset >= start_offset & day_offset <= end_offset)
}

# Exploration after cleaning ----------------------------------------------

compute_count_data <- function(data, years = c("2019", "2020"),
                               vehicle_types = c("scooter", "bicycle")) {
  
  start_offset = min(data$day_offset)
  end_offset = max(data$day_offset)
  
  data %>%
    mutate(
      day_offset = factor(day_offset, levels = start_offset:end_offset)
    ) %>%
    count(year, vehicle_type, day_offset, .drop = FALSE) %>%
    mutate(day_offset = as.integer(levels(day_offset))[day_offset]) %>%
    filter(vehicle_type %in% vehicle_types, year %in% years)
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
