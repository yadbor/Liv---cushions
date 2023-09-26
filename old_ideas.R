# Read the two sheets constructed from Liv's data & name them
lcdod <- readxl::read_excel(data_file, "LCDOD")
hysteresis <- readxl::read_excel(data_file, "Hysteresis")


# Convert to long form for easier analysis,
# then combine both data sets by row and
# group by foam type, variable and the load level for each result
liv_data <- bind_rows(
  lcdod = readxl::read_excel(data_file, "LCDOD") %>%
    pivot_longer(cols = where(is.numeric), # only the results columns
                 names_to = "level",
                 values_to = "value"
    ),
  hysteresis = readxl::read_excel(data_file, "Hysteresis") %>%
    pivot_longer(cols = where(is.numeric), # only the results columns
                 names_to = "level",
                 values_to = "value"
    ),
  .id = "var"
) %>% group_by(foam, var, level)


# Define a function to calculate the span of the given percentile
# percentile defaults to 95% (0.95)
CI = function(sd, percentile = 0.95) {
  interval = (percentile + 1)/2
  qnorm(p = interval, mean = 0, sd = sd)
}

liv_results <- liv_data %>%
  summarise(avg = mean(value), sd = sd(value), n = n()) %>%
  mutate(delta = CI(sd, 0.95), lo = avg - delta, hi = avg + delta) %>%
  select(-delta) %>%
  arrange(var, level, foam)

print(liv_results)

# LCDOD

lcdod_res <- liv_lcdod %>%
  pivot_longer(cols = starts_with("L"), names_to = "variable") %>%
  group_by(foam, variable) %>%
  summarise(average = mean(value),
            sd = sd(value),
            n = n()) %>%
  mutate(delta = CI(sd, 0.95), lo = average - delta, hi = average + delta)

print(lcdod_res)

liv_lcdod %>%
  pivot_longer(cols = starts_with("L"),
               names_to = "load",
               values_to = "LCDOD") %>%
  group_by(foam, LCDOD) %>%
  ggplot() +
  aes(x = foam, y = LCDOD, colour = foam) +
  geom_boxplot() +
  geom_jitter(width = 0.1, colour = "gray10") +
  facet_wrap( ~ load, scales = "free_y")

plots <- list()
for (var_name in unique(liv_data$var)) {
  plots[[var_name]] <- liv_data %>%
    filter(var == var_name) %>%
    ggplot() +
    aes(y = value, x = foam, colour = foam) +
    geom_boxplot() +
    geom_jitter(colour = "black", width = 0.25) +
    labs(title = var_name, y = var_name) +
    facet_wrap(~level)
}
print(plots)

