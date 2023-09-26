# Run a Two-one-sided-test on Liv's data

library(readxl)
library(dplyr)
library(TOSTER)

data_file <- here::here("data", "results.xlsx")

# Display sheet names
sheet_names <- readxl::excel_sheets(data_file)
print(sheet_names)

# Read the two sheets constructed from Liv's data & name them
# Convert to long form for easier analysis,
# then combine both data sets by row and
# group by foam type, variable and the load level for each result
liv_data <- sheet_names %>%                  # Take the named sheets
  purrr::set_names() %>%                     # make them a named list
  purrr::map(                                # apply a function to each element
    function(x) {
      readxl::read_excel(data_file, x) %>%   # read the named sheet
      pivot_longer(cols = where(is.numeric), # use the results columns (numeric)
                   names_to = "level",       # column names to 'level'
                   values_to = "value"       # each value to 'value'
                  )
    }
  ) %>%
  bind_rows(.id = "var") %>%      # combine by rows, put original names in 'var'
  group_by(foam, var, level)      # make groups for each independent measurement

# liv_data is now a list with columns named
# var = lcdod or hysteresis
# cushion = cushion ID number
# foam = EN40-230 or EN50-250
# level = load level for measurement
# value = measurement

# Define a function to calculate the span of the given percentile
# percentile defaults to 95% (0.95)
CI = function(sd, percentile = 0.95) {
  interval = (percentile + 1)/2
  qnorm(p = interval, mean = 0, sd = sd)
}

# Show the Summary stats for each variable,including the 95% CI
liv_summary <- liv_data %>%
  summarise(avg = mean(value), sd = sd(value), n = n()) %>%
  mutate(delta = CI(sd, 0.95), lo_95 = avg - delta, hi_95 = avg + delta) %>%
  select(-delta) %>%
  arrange(var, level, foam)

print(liv_summary)

# Make some simple box plots to show the spread of the data
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

