
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

# Data cleaning -----------------------------------------------------------

# First Monday of each year.
first_monday_date <- c(
  "2019" = mdy("01-07-2019"),
  "2020" = mdy("01-06-2020")
)

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
      day_offset = (first_monday_date[year] %--% date(start_time)) / ddays(1),
      start_district = factor(start_district, levels = council_districts),
      end_district = factor(end_district, levels = council_districts)
    )
  
  data
}

restrict_day_offset <- function(data_all_dates, start_offset, end_offset) {
  data <- data_all_dates %>%
    filter(day_offset >= start_offset & day_offset <= end_offset)
  
  data
}

# Exploration after cleaning ----------------------------------------------

# TODO: For next two functions, could also just change the limits on the x-axis
# rather than filter the data.

plot_trip_distance <- function(data, years = c("2019", "2020"),
                               vehicle_types = c("scooter", "bicycle"),
                               max_distance = Inf) {
  
  data <- data %>%
    filter(trip_distance_km <= max_distance)
  
  plot <- ggplot(data, aes(x = trip_distance_km, col = year,
                           linetype = vehicle_type)) +
    geom_freqpoly(binwidth = 0.1)
  
  plot
}

plot_trip_duration <- function(data, years = c("2019", "2020"),
                               vehicle_types = c("scooter", "bicycle"),
                               max_duration = Inf) {
  
  data <- data %>%
    filter(trip_duration_hours <= max_duration)
  
  plot <- ggplot(data, aes(x = trip_duration_hours, col = year,
                           linetype = vehicle_type)) +
    geom_freqpoly(binwidth = 1/12)  # 5 minute intervals
  
  plot
}

plot_council_districts <- function(data, year, vehicle_type,
                                   use_proportions = TRUE) {
  data <- data %>%
    filter(year == !!year, vehicle_type == !!vehicle_type)
  
  plot_data <- data %>%
    count(start_district, end_district, name = "fill_value", .drop = FALSE)
  
  if (use_proportions) {
    # Convert counts to proportions, rounded to 2 decimal places.
    total_rides <- nrow(data)
    plot_data <- plot_data %>%
      mutate(fill_value = round(fill_value / total_rides, 2))
  }
  
  plot <- ggplot(plot_data, aes(x = start_district, y = end_district)) +
    geom_tile(aes(fill = fill_value)) +
    geom_text(aes(label = fill_value), color = "white")
  
  plot
}

plot_start_times <- function(data, year, vehicle_type) {
  data <- data %>%
    filter(year == !!year, vehicle_type == !!vehicle_type)
  
  # TODO: Should this be done in the data cleaning function?
  data <- data %>%
    mutate(
      day_of_week = wday(start_time, week_start = 1),
      day_of_week_label = wday(start_time, week_start = 1, label = TRUE),
      month_label = month(start_time, label = TRUE),
      hours_since_midnight = hour(start_time) + minute(start_time) / 60
        + second(start_time) / 3600,
      holiday = FALSE  # TODO: Deal with holidays.
    )
  
  # TODO: Way to adjust theme to make this look more like a conventional
  # geom_histogram()?
  # TODO: How to add counts on the y-axis?
  plot <- ggplot(data, aes(x = hours_since_midnight, y = day_of_week_label)) +
    geom_density_ridges(stat = "binline", bins = 24, pad = FALSE, scale = 0.9) +
    facet_wrap(~ month_label)
  
  plot
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
