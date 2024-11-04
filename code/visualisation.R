# This script performs data visualization and analysis on suicide data.

### Define plotting functions
# Plot independent variable against altitude
plot_against_altitude <- function(y_var, data) {
    ggplot(data = data, aes(x = altitude, y = .data[[y_var]])) +
        geom_point() +
        xlim(-20, 3300) +
        geom_smooth(method = "lm") +
        theme(text = element_text(size = 20))
}

# Plot dependent variable against altitude
plot_suicide_against_altitude <- function(y_var, data) {
    plot_against_altitude(y_var, data) + ylim(0, 55)
}

### Overview of data
str(suicide_data)
summary(suicide_data)

# Filter data and create variable
suicide_data <- suicide_data %>% mutate(share_firearms_suicides = suicide_firearms_rate / suicide_overall_rate)
suicide_data_no_nas <- suicide_data %>% filter(if_all(starts_with("suicide"), ~ !is.na(.x)))

### Plot dependent variables against altitude
plot_suicide_variables <- c("suicide_overall_rate", "suicide_firearms_rate", "suicide_other_rate")
plots_list <- plot_suicide_variables %>% lapply(plot_suicide_against_altitude, data = suicide_data_no_nas)

# Print plot to file
png("figures/suicide_vs_altitude.png", width = 600, height = 250)
grid.arrange(grobs = plots_list, ncol = 3)
dev.off()

### Plot selected independent and other variables against altitude
plot_independent_variables <- c(
    "firearms_rate", "log_population_density", "log_median_household_income", "poverty_level_overall",
    "poverty_level_65_over", "percent_white", "percent_native", "labor_force_participation",
    "percent_unemployment", "median_age", "fifty_and_above", "old_age_dependency",
    "percent_highschool_or_higher", "percent_bachelor_or_higher", "dod_excl_suicide_rate",
    "gun_dealer_licenses_rate", "percent_veteran", "sex_ratio", "y_coordinate", "share_firearms_suicides"
)
plots_ind_list <- plot_independent_variables %>% lapply(plot_against_altitude, data = suicide_data_no_nas)

# Print plot to file
png("figures/covariates_vs_altitude.png", width = 800, height = 800)
grid.arrange(grobs = plots_ind_list, ncol = 5)
dev.off()

# Mapping

#import shapefile
counties_shape <- st_read("data/census/county_shapefile")

counties_shape <- counties_shape %>% mutate(county_code = as.integer(paste0(STATE, COUNTY)))
counties_shape <- counties_shape %>% select(c(geometry, county_code))

#merge with main data
mapping_data <- counties_shape %>% left_join(suicide_data, by = "county_code")

#remove non contiguous US
mapping_data <- mapping_data %>% filter(state != "Alaska" & state != "Hawaii" & state != "Puerto Rico")

#Plot suicide rate
ggplot(mapping_data) +
  geom_sf(aes(fill = suicide_overall_rate), linewidth = 0.001) +
  scale_fill_viridis_c(name = "Suicide rate", option = "rocket") +
  labs(title = "Suicide Rate of US Counties") +
  theme_minimal()

#Set width for slides
slide_width <- 10  # inches
slide_height <- 7.5  # inches

# Export the plot as a PNG
ggsave(
  filename = "figures/suicide_map.png",
  plot = last_plot(),  # If this is the last plot you created
  width = slide_width,
  height = slide_height,
  dpi = 300  # High resolution for presentations
)

#Plot altitude
ggplot(mapping_data) +
  geom_sf(aes(fill = altitude), linewidth = 0.001) +
  scale_fill_viridis_c(name = "Avg Altitude (m)", option = "rocket", trans = "sqrt") +
  labs(title = "Altitude of US Counties") +
  theme_minimal()

#Export
ggsave(
  filename = "figures/altitude_map.png",
  plot = last_plot(),  # If this is the last plot you created
  width = slide_width,
  height = slide_height,
  dpi = 300  # High resolution for presentations
)
