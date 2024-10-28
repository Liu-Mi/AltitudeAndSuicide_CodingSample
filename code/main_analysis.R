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

brenner_lms <- list(brenner_lm_overall, brenner_lm_firearms, brenner_lm_other)

# Calculate brackets statistics and marginal effect of moving up an altitude bracket
altitude_brackets_stats_df <- calculate_altitude_brackets_stats(suicide_data, suicide_data_fe, step_size, no_alt_steps, brenner_lms)

# Create LaTeX table with mean altitude
altitude_brackets_stats_latex <- xtable(altitude_brackets_stats_df, caption = "Altitude Brackets Statistics", label = "tab:altitude_brackets_stats")
print(altitude_brackets_stats_latex,
    type = "latex", file = "tables/altitude_brackets_stats.tex",
    include.rownames = FALSE, size = "\\footnotesize", sanitize.text.function = function(x) x
)

# Create regression output table
stargazer(
    ... = brenner_lms, out = "tables/brenner.tex", label = "tab:brenner", title = "Brenner et al. replication",
    dep.var.labels = c("Overall suicides", "Firarm suicides", "Nonfirearm suicides"),
    font.size = "scriptsize", single.row = TRUE,
    covariate.labels = c(
        altitude_brackets_text, "Fifty and above", "Sex Ratio", "Percent White",
        "Median household income (level)", "Population density (level)"
    )
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
stargazer(
    ... = alt_fixed_effects, out = "tables/alt_fixed_effects.tex", label = "tab:alt_fixed_effects", title = "Atltiude fixed effects model",
    dep.var.labels = c("Overall suicides", "Firarm suicides", "Nonfirearm suicides"),
    font.size = "scriptsize", single.row = TRUE, omit = c("state")
)


### Model with stepwise altitude fixed effects
# Add stepwise altitude fixed effects
alt_fe_stepwise_data <- suicide_data %>%
    mutate(altitude = pmin(pmax(floor(altitude / step_size), 0), no_alt_steps)) %>%
    filter(!is.na(altitude))
alt_fe_matrix <- alt_fe_stepwise_data$altitude %>%
    lapply(FUN = function(x) {
        c(rep(1, times = x), rep(0, no_alt_steps - x))
    }) %>%
    unlist() %>%
    matrix(ncol = no_alt_steps, byrow = TRUE)
colnames(alt_fe_matrix) <- paste0("altitude_fe", 1:(no_alt_steps))

dod_data_altitude_fe <- cbind(alt_fe_stepwise_data, alt_fe_matrix)

# Run model with altitude fixed effects
dod_data_altitude_fe_lm <- main_regression_models(dod_data_altitude_fe, c("state", "gun_dealer_licenses_rate"), c(interaction_terms), "altitude")

# Run hypothesis tests for altitude up to a bracket
types <- c("overall", "firearms", "other")
hypotheses_list <- lapply(types, function(type) {
    p_values <- numeric(no_alt_steps)
    for (x in 1:no_alt_steps) {
        hypothesis_test <- hypotheses(dod_data_altitude_fe_lm[[which(types == type)]], paste0(paste0("altitude_fe", 1:x, collapse = "+"), " = 0"))
        p_values[x] <- hypothesis_test$p.value
    }
    p_values
})
names(hypotheses_list) <- types

# Create a dataframe with the coefficients of the different types
coefficients_df <- data.frame(
    altitude_bracket = 1:no_alt_steps,
    overall = extract_coefficients(dod_data_altitude_fe_lm[[1]], "altitude"),
    firearms = extract_coefficients(dod_data_altitude_fe_lm[[2]], "altitude"),
    other = extract_coefficients(dod_data_altitude_fe_lm[[3]], "altitude")
)

# Add hypothesis results to the dataframe
coefficients_df <- coefficients_df %>%
    mutate(
        overall_significant = hypotheses_list$overall < 0.05,
        firearms_significant = hypotheses_list$firearms < 0.05,
        other_significant = hypotheses_list$other < 0.05
    )

# Pivot the coefficients dataframe to a longer format for better visualization
coefficients_long_df <- coefficients_df %>%
    pivot_longer(cols = c(overall, firearms, other), names_to = "type", values_to = "estimate") %>%
    pivot_longer(cols = c(overall_significant, firearms_significant, other_significant), names_to = "significance_type", values_to = "significant") %>%
    filter(type == sub("_significant", "", significance_type)) %>%
    mutate(
        estimate = ifelse(significant, paste0("$\\pmb{", round(estimate, 3), "}$"), round(estimate, 3))
    ) %>%
    select(altitude_bracket, type, estimate) %>%
    pivot_wider(names_from = type, values_from = estimate)
coefficients_long_df$altitude_bracket <- paste0("Altitude $\\geq$ ", seq(step_size, (no_alt_steps) * step_size, step_size), "m")
colnames(coefficients_long_df) <- c("Bracket", "Overall", "Firearms", "Other")

# Create LaTeX table with the coefficients
coefficients_long_df_latex <- xtable(coefficients_long_df, caption = "Coefficients of the stepwise altitude fixed effects model", label = "tab:alt_fe_stepwise_coefficients")
print(coefficients_long_df_latex,
    type = "latex", file = "tables/alt_fe_stepwise_coefficients.tex",
    include.rownames = FALSE, size = "\\footnotesize", sanitize.text.function = function(x) x
)

# stargarzer table
stargazer(
    ... = dod_data_altitude_fe_lm, out = "tables/alt_fe_stepwise.tex", label = "tab:alt_fe_stepwise", title = "Stepwise altitude fixed effects model",
    dep.var.labels = c("Overall suicides", "Firarm suicides", "Nonfirearm suicides"),
    font.size = "scriptsize", single.row = TRUE, omit = c("altitude", "state")
)
