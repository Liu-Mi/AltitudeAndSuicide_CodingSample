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

# Filter data
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
