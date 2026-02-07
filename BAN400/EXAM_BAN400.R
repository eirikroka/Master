# EXAM -- BAN400 -- FALL 2024
#
# Enter your answers to the questions in this file. When you are done, you will
# hand in this script and *nothing else*. You can assume that the grader has the
# same folder structure as provided in the zip-file attached to the exam.

# Task a ------------------------------

# Loading packages
library(here)
library(tidyverse)

setwd(here())

# Reading data for turbines and models
turbines <- read.csv(here("data", "ban400-h24-turbines.csv")) %>%
  as_tibble()

models <- read.csv(here("data", "ban400-h24-models.csv")) %>%
  as_tibble()

# Making a summary table for location, model, number of turbines and capacity of each turbine
turbines %>%
  group_by(location, model) %>%
  summarise(n_turbines = n()) %>%
  left_join(models)

# Task b ------------------------------

# Reading data for wind
wind <- read.csv(here("data", "ban400-h24-wind.csv")) %>%
  as_tibble() %>%
  mutate(time = dmy_hm(time) - hours(1)) %>% # Reduced it with 1 hours such that it covers the start of the measurement of wind
  rename(location = name)

# Plotting wind data
wind %>%
  ggplot(aes(x = time, y = mean_wind, color = location)) +
  geom_line() +
  xlab("Time") +
  ylab("Average wind (m/s) per hour ") +
  ggtitle("Average wind (m/s) per hour plotted over time") +
  theme_minimal()


# Task c ------------------------------

# Adding the turbine parameters to models
models <- models %>%
  mutate(
    w_cut_in = c(4, 4, 3),
    w_rated = c(13, 11.4, 10.8),
    w_cut_out = c(25, 25, 25)
  )

# Joining turbines with models
turbines <- turbines %>%
  left_join(models)

# Task d ------------------------------

# This function calculates the output based in the vectors for w, w_cut_in, w_rate, w_cut_out and capacity.
# It returns a output vector.
output_calc1 <- function(w, w_cut_in, w_rated,
                         w_cut_out, capacity) {
  if (length(w) != length(w_cut_in) | length(w) != length(w_rated) |
    length(w) != length(w_cut_out) | length(w) != length(capacity)) {
    stop("At least one vector have a different length")
  } # This stops the function if the vectors have different length

  output_matrix <- if_else(w < w_cut_in,
    0,
    if_else(w_cut_in <= w & w < w_rated,
      capacity *
        ((w - w_cut_in) / (w_rated - w_cut_in))^3,
      if_else(w_rated <= w & w < w_cut_out,
        capacity, 0
      )
    )
  )

  return(output_matrix)
}

# Task e ------------------------------

# This is a adjusted function of "output_calc1" function such that calculates the result is stored in a tibble
# It returns a table with the columns output, wind and the model
# I use this function for the rest of the exam
output_calc2 <- function(w, data = models) {
  output_matrix <- tibble(
    output = NA,
    wind = w,
    model = data$model
  )

  output_matrix$output <- if_else(w < data$w_cut_in,
    0,
    if_else(data$w_cut_in <= w & w < data$w_rated,
      data$capacity *
        ((w - data$w_cut_in) / (data$w_rated - data$w_cut_in))^3,
      if_else(data$w_rated <= w & w < data$w_cut_out,
        data$capacity, 0
      )
    )
  )
  return(output_matrix)
}

# Adding a vector
wind_vector <- c(0:30)

# Calculating the output for each model and plotting
map(wind_vector, ~ output_calc2(.x, models)) %>%
  bind_rows() %>%
  ggplot(aes(x = wind, y = output, color = model)) +
  geom_line() +
  xlab("Wind (m/s)") +
  ylab("Output (kW)")

# Task f ------------------------------
# Joining turbines data with wind data
turbines_and_wind <- wind %>%
  left_join(turbines, by = "location", relationship = "many-to-many")

# Gathering all unique wind values
unique_winds <- unique(turbines_and_wind$mean_wind)

# Calculating the output for each model for each wind
outputs <- map(unique_winds, ~ output_calc2(.x, models)) %>%
  bind_rows()

# Adding the output results to the turbine data
turbines_and_wind <- turbines_and_wind %>%
  inner_join(outputs, join_by(mean_wind == wind, model == model))

# Task g ------------------------------

# Plotting the hourly total production at each location, the y-axis is free
turbines_and_wind %>%
  group_by(location, time) %>%
  summarise(total_output = sum(output)) %>%
  ggplot(aes(x = time, y = total_output)) +
  geom_line() +
  facet_wrap(~location, nrow = 2, scale = "free_y")

# Average hourly production per model per location
# The reason for not using turbine_id is that we assume each turbine_id with the same model preformed equal
turbines_and_wind %>%
  group_by(location, model) %>%
  summarise(mean_hourly_prod_per_turbine = mean(output))

# The monthly production at each location i kWh
turbines_and_wind %>%
  mutate(month = month(time)) %>%
  group_by(location, month) %>%
  summarise(month_prod = sum(output))

# Task h ------------------------------

# There are quite a big difference between these three places.
# Based on the plot from task f, you see that both Bremsund and Kvernes has
# more consistent production in comparison to Kappstad.

# The second summary table shows us also that the average production per turbine
# is much much lower in Kappstad compared to Bremsund and Kvernes.
# There is a possibility for bias in the data, f.ex that seasonal winds or similar.

# The last summary table also show a consistency for Bremsund and Kvernes, and that
# Kappstads production is rather volatile.

# In conclusion geographical features are not created equal, so it is not a good take to say that
# there is equal wind everywhere.

# The windmill is most efficient between w_rated and w_cut_out, and some places seem to have more consistent winds in that interval

# Task i ------------------------------

# Calculating the output after the parameter development

# For development in "cut_in"
w_cut_in_outputs <- map(unique_winds, ~ output_calc2(.x, models %>%
  mutate(
    w_cut_in = 0.9 * w_cut_in,
  ))) %>%
  bind_rows() %>%
  rename(w_cut_in_output = output)

# For development in "capacity"
capacity_outputs <- map(unique_winds, ~ output_calc2(.x, models %>%
  mutate(
    capacity = 1.1 * capacity,
  ))) %>%
  bind_rows() %>%
  rename(capacity_output = output)

# For development in "cut_out"
w_cut_out_outputs <- map(unique_winds, ~ output_calc2(.x, models %>%
  mutate(
    w_cut_out = 1.1 * w_cut_out,
  ))) %>%
  bind_rows() %>%
  rename(w_cut_out_output = output)


# Joining all the calculated results, with the original outputs and with the parameters changes
# Changing the data to show the increased production for each model at each location for each parameter development
data_parameters <- turbines_and_wind %>%
  distinct(location, time, model, mean_wind) %>%
  inner_join(
    outputs,
    join_by(mean_wind == wind, model == model)
  ) %>%
  rename(orginal_output = output) %>%
  inner_join(
    w_cut_in_outputs,
    join_by(mean_wind == wind, model == model)
  ) %>%
  inner_join(
    capacity_outputs,
    join_by(mean_wind == wind, model == model)
  ) %>%
  inner_join(
    w_cut_out_outputs,
    join_by(mean_wind == wind, model == model)
  ) %>%
  group_by(location, model) %>%
  summarise(
    increase_w_cut_in =
      sum(w_cut_in_output) - sum(orginal_output),
    increase_cap =
      sum(capacity_output) - sum(orginal_output),
    increase_w_cut_out =
      sum(w_cut_out_output) - sum(orginal_output)
  )

print(data_parameters)

# A table showing the best result for each model for each location
data_parameters %>%
  pivot_longer(c(increase_w_cut_in, increase_cap, increase_w_cut_out),
    names_to = "parameter", values_to = "increase_prod"
  ) %>%
  group_by(location, model) %>%
  slice_max(increase_prod, n = 1)
# The table above shows us that increasing capacity is the most effective at Bremsund and Kvernes,
# but at Kappstad lowering the "cut in" has the most effect.
# I would advice to increase capacity since placing windmills at Kappstad is rather inefficient compared
# to the two other places.


# This table shows the average effect across location and model based on development parameters
data_parameters %>%
  pivot_longer(c(increase_w_cut_in, increase_cap, increase_w_cut_out),
    names_to = "parameter", values_to = "increase_prod"
  ) %>%
  group_by(parameter) %>%
  summarise(
    mean_increase = mean(increase_prod)
  )

# Increasing capacity has the biggest effect