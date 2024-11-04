### Setup
rm(list = ls())

# Load in libraries
library(tidyverse)
library(marginaleffects)
library(patchwork)
library(gridExtra)
library(car)
library(MASS)
library(sf)
library(haven)
library(stargazer)
library(xtable)
library(tidycensus)
select <- dplyr::select

# Clean data, visualise and load functions
source("code/clean_data.R")
source("code/visualisation.R")
source("code/functions.R")

# Load in Data
suicide_data <- read.csv("data/final_data.csv")
suicide_data <- suicide_data %>%
    filter(
        if_all(starts_with("suicide"), ~ !is.na(.)) &
            if_all(c(-ends_with("rate"), "gun_dealer_licenses_rate"), ~ !is.na(.))
    )


### Replicate Brenner et al.
# Add altitude fixed effects
step_size <- 304 # Step size of 1000 ft as in Brenner et al.
no_alt_steps <- floor(3000 / step_size)
suicide_data_fe <- suicide_data %>%
    mutate(altitude = pmin(pmax(floor(altitude / step_size), 0), no_alt_steps)) %>%
    filter(!is.na(altitude)) %>%
    mutate(altitude = as.factor(altitude))

# Brenner et al. replication with 3 different dependent variables
brenner_lm_overall <- lm(
    suicide_overall_rate ~ altitude + fifty_and_above + sex_ratio + percent_white +
        I(exp(log_median_household_income)) + I(exp(log_population_density)),
    data = suicide_data_fe
)
brenner_lm_firearms <- lm(
    suicide_firearms_rate ~ altitude + fifty_and_above + sex_ratio + percent_white +
        I(exp(log_median_household_income)) + I(exp(log_population_density)),
    data = suicide_data_fe
)
brenner_lm_other <- lm(
    suicide_other_rate ~ altitude + fifty_and_above + sex_ratio + percent_white +
        I(exp(log_median_household_income)) + I(exp(log_population_density)),
    data = suicide_data_fe
)

brenner_lms <- list(overall = brenner_lm_overall, firearms = brenner_lm_firearms, other = brenner_lm_other)

# Calculate brackets statistics and marginal effect of moving up an altitude bracket
altitude_brackets_stats_df <- calculate_altitude_brackets_stats(suicide_data, suicide_data_fe, step_size, no_alt_steps, brenner_lms)

# Create LaTeX table with mean altitude
altitude_brackets_stats_latex <- xtable(altitude_brackets_stats_df, caption = "Altitude Brackets Statistics", label = "tab:altitude_brackets_stats")
print(altitude_brackets_stats_latex,
    type = "latex", file = "tables/altitude_brackets_stats.tex",
    include.rownames = FALSE, size = "\\footnotesize", sanitize.text.function = function(x) x
)


### Main analysis
# Define interaction terms and powers of altitude
interaction_terms <- c(
    "fifty_and_above:log_population_density",
    "percent_veteran:percent_white",
    "poverty_level_overall:percent_white"
)
powers <- c("I(altitude^2)", "I(altitude^3)", "I(altitude^4)")

# Main model
main_model <- main_regression_models(suicide_data, c("state", "gun_dealer_licenses_rate"), c(powers, interaction_terms))

# Generate LaTeX tables and plots for the main model
generate_latex_and_plots(main_model, "main_model", "Regression Results of Main Model", ylim_range = c(-0.005, 0.005), omit = c("altitude", "state"))


### Decompose size of mediator effects
# Get all relevant mediator variables
mediator_data <- suicide_data %>%
    select(-c(
        county, county_code, state, population,
        fifty_and_above, sex_ratio, percent_white, log_median_household_income, log_population_density,
        ends_with("rate")
    ), gun_dealer_licenses_rate, suicide_overall_rate) %>%
    mutate(across(c(everything(), -c(altitude, suicide_overall_rate)), scale))
level_terms <- mediator_data %>%
    select(-c(altitude, suicide_overall_rate)) %>%
    names()

# Check how well altitude explains each variable
lm_alt_med <- level_terms %>% lapply(function(x, data) lm(as.formula(paste(x, "~ altitude")), mediator_data, weights = mediator_data$weights))
coeffs_alt_med <- lm_alt_med %>%
    lapply(function(x) data.frame(coefficients = coefficients(x)[2], p_values = summary(x)$coefficients[2, 4])) %>%
    do.call(what = rbind)
row.names(coeffs_alt_med) <- level_terms
coeffs_alt_med %>% filter(p_values > 0.05) # Lists variables that aren't explained by altitude

# Check how well each variable explains suicide rates
lm_suicide_med <- level_terms %>% lapply(function(x, data) lm(as.formula(paste("suicide_overall_rate ~", x)), mediator_data, weights = mediator_data$weights))
coeffs_suicide_med <- lm_suicide_med %>%
    lapply(function(x) data.frame(coefficients = coefficients(x)[2], p_values = summary(x)$coefficients[2, 4])) %>%
    do.call(what = rbind)
coeffs_suicide_med %>% filter(p_values > 0.05) # List variables that don't explain suicide rates
coeffs_suicide_med %>% arrange(desc(abs(coefficients)))

# Check how well each variable explains suicide rates conditional on altitude
lm_cond_suicide_med <- level_terms %>% lapply(function(x, data) lm(as.formula(paste("suicide_overall_rate ~", x, " + altitude")), mediator_data, weights = mediator_data$weights))
coeffs_cond_suicide_med <- lm_cond_suicide_med %>%
    lapply(function(x) data.frame(coefficients = coefficients(x)[2], p_values = summary(x)$coefficients[2, 4])) %>%
    do.call(what = rbind)
coeffs_cond_suicide_med %>% arrange(desc(abs(coefficients)))

# Order size of mediator effect according to relationship with suicide conditional on altitude
order_mediators <- coeffs_cond_suicide_med %>%
    filter(p_values <= 0.05) %>%
    arrange(desc(abs(coefficients))) %>%
    row.names()
order_mediators <- c("state", order_mediators)

# Add variables one by one to regression
mediator_names <- c("altitude", c(order_mediators) %>% Reduce(f = c, accumulate = TRUE))
mediator_effect_lms <- mediator_names %>% lapply(function(x) {
    data <- suicide_data %>% select(-c(ends_with("rate"), all_of(order_mediators %>% setdiff(x))), gun_dealer_licenses_rate, starts_with("suicide"))
    main_regression_models(data, x)
})

# Extract size of mediator effects
mediator_effects <- mediator_effect_lms %>%
    lapply(function(x) {
        coefficients <- coefficients(x$overall)
        coefficient <- coefficients[names(coefficients) == "altitude"]
        coefficient
    }) %>%
    unlist() %>%
    diff()
names(mediator_effects) <- order_mediators
mediator_effects


### Simple Robustness checks
# Model including deaths of despair rate
dod_rate <- main_regression_models(suicide_data, c("state", "gun_dealer_licenses_rate", "dod_excl_suicide_rate"), c(powers, interaction_terms))
generate_latex_and_plots(dod_rate, "dod_rate", "Regression Results of model with deaths of despair excl. suicides", ylim_range = c(-0.005, 0.005), omit = c("altitude", "state"))

# Model with no state fixed effects
no_state_fe <- main_regression_models(suicide_data, c("gun_dealer_licenses_rate"), c(powers, interaction_terms))
generate_latex_and_plots(no_state_fe, "no_state_fe", "Regression Results with no state fixed effects", ylim_range = c(-0.01, 0.01), omit = c("altitude", "state"))

# Model with only level altitude
no_powers <- main_regression_models(suicide_data, c("state", "gun_dealer_licenses_rate"), c(interaction_terms))
generate_latex_and_plots(no_powers, "no_powers", "Regression Results of model with no powers", ylim_range = c(-0.005, 0.005), omit = c("state"))


### Model with added altitude fixed effects
alt_fixed_effects <- main_regression_models(suicide_data_fe, c("state", "gun_dealer_licenses_rate"), c(interaction_terms))

# Create statistics as with Brenner et al.
alt_fixed_effects_df <- altitude_brackets_stats_df[, -c((length(altitude_brackets_stats_df) - 2):length(altitude_brackets_stats_df))]
alt_fixed_effects_df <- add_marginal_effects(alt_fixed_effects_df, alt_fixed_effects)

# Create LaTeX table with mean altitude
alt_fixed_effects_df_latex <- xtable(alt_fixed_effects_df, caption = "Marginal effect of altitude fixed effects", label = "tab:alt_fixed_effects_me")
print(alt_fixed_effects_df_latex,
    type = "latex", file = "tables/alt_fixed_effects_me.tex",
    include.rownames = FALSE, size = "\\footnotesize", sanitize.text.function = function(x) x
)

# Create regression output table
stargazer(
    ... = alt_fixed_effects, out = "tables/alt_fixed_effects.tex", label = "tab:alt_fixed_effects", title = "Atltiude fixed effects model",
    dep.var.labels = c("Overall suicides", "Firarm suicides", "Nonfirearm suicides"),
    font.size = "scriptsize", single.row = TRUE, omit = c("state")
)
