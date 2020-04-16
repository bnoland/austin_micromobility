
# Day offsets corresponding to start and end of SXSW 2019.
sxsw_2019_day_offsets <- c(
  "start" = 66,
  "end" = 75
)

start_offset <- sxsw_2019_day_offsets[["end"]] + 1  # Day after end of SXSW 2019
end_offset <- 96

plan <- drake_plan(
  raw_data = load_data(file_in("Shared_Micromobility_Vehicle_Trips.csv")),
  data_all_dates = clean_data(raw_data),
  n_obs_deleted = nrow(raw_data) - nrow(data_all_dates),
  data = restrict_day_offset(data_all_dates, start_offset, end_offset),
  
  all_freq_plot = plot_frequencies(data, label_increment = 1),
  scooter_freq_plot = plot_frequencies(data, vehicle_types = c("scooter"),
                                       label_increment = 1),
  bicycle_freq_plot = plot_frequencies(data, vehicle_types = c("bicycle"),
                                       label_increment = 1),
  
  all_freq_2019_plot = plot_frequencies(data_all_dates, years = c("2019")),
  scooter_freq_2019_plot = plot_frequencies(data_all_dates, years = c("2019"),
                                            vehicle_types = c("scooter")),
  bicycle_freq_2019_plot = plot_frequencies(data_all_dates, years = c("2019"),
                                            vehicle_types = c("bicycle"))
)
