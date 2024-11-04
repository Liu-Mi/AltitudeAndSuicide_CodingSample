# This file contains functions used in the main analysis section of the code.

# Function to create WLS model with all variables in data as predictors
suicide_lm_model <- function(data, response_var, predictor_vars, interaction_terms = c(), omitted_predictors = c()) {
    selected_data <- data %>% select(
        -c(county, county_code, starts_with("state"), population, ends_with("rate"), all_of(omitted_predictors)),
        all_of(response_var), all_of(predictor_vars)
    )
    level_terms <- names(selected_data %>% select(-all_of(response_var)))
    explanatory_vars <- paste(c(level_terms, interaction_terms), collapse = "+")
    lm(as.formula(paste(response_var, "~", explanatory_vars)), data = selected_data, weights = data$population)
}

# Run WLS regressions on overall, firearms and nonfirearms suicide rates
main_regression_models <- function(data, predictor_vars, interaction_terms = c(), omitted_predictors = c()) {
    suicide_overall_lm <- suicide_lm_model(data, "suicide_overall_rate", predictor_vars, interaction_terms, omitted_predictors)
    suicide_firearms_lm <- suicide_lm_model(data, "suicide_firearms_rate", predictor_vars, interaction_terms, omitted_predictors)
    suicide_other_lm <- suicide_lm_model(data, "suicide_other_rate", predictor_vars, interaction_terms, omitted_predictors)
    list(overall = suicide_overall_lm, firearms = suicide_firearms_lm, other = suicide_other_lm)
}

# Function to plot marginal effects and calculate slopes
plot_and_calculate_slopes <- function(models) {
    # Marginal effects
    plots_data <- lapply(models, function(model) {
        plot_slopes(model, vcov = "HC3", variables = c("altitude"), condition = c("altitude"), rug = T, draw = F) %>% select(estimate, conf.low, conf.high, altitude)
    })

    # Combine plots
    if (length(plots_data) > 1) {
        plots_data <- lapply(X = names(plots_data), FUN = function(x) {
            plots_data[[x]]$dependent_var <- x
            plots_data[[x]][, c(5, 1, 2, 3, 4)]
        }) %>% do.call(what = rbind)
    } else {
        plots_data[[1]]$dependent_var <- "variable"
        plots_data <- plots_data[[1]]
    }

    # Extract names of coefficients that contain "altitude"
    coef_names <- names(coef(models[[1]]))
    altitude_coef_names <- coef_names[grepl("altitude", coef_names)]

    # Plot marginal effects
    combined_plot <- ggplot(plots_data, aes(x = altitude, y = estimate, color = dependent_var)) +
        geom_line() +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
        labs(title = "Marginal Effects of Altitude", x = "Altitude", y = "Estimate") +
        theme_minimal()

    # Average slopes
    avg_slopes_list <- lapply(models, avg_slopes, vcov = "HC3")

    # Slopes of altitude for easy viewing
    adj_elevation <- lapply(avg_slopes_list, function(x) {
        (x %>%
            filter(term == "altitude") %>%
            select(term, estimate, p.value)) %>% unlist()
    }) %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame()

    list(
        combined_plot = combined_plot,
        avg_slopes = avg_slopes_list,
        adj_elevation = adj_elevation
    )
}

# Function to create dataframe of comparisons of marginal effects
create_comparison_df <- function(model, altitude_brackets_stats_df, alpha = 0.05) {
    mean_altitude <- altitude_brackets_stats_df$`Mean Altitude`
    avg_inc_in_alt <- mean_altitude %>% diff()
    no_steps <- length(avg_inc_in_alt)

    comparison_types <- c("Overall", "Firearms", "Other")
    comparison_models <- model[1:3]
    comparisons <- lapply(1:no_steps, function(i) {
        lapply(1:3, function(j) {
            avg_comparisons(comparison_models[[j]], variables = list(altitude = avg_inc_in_alt[i]), newdata = datagrid(altitude = mean_altitude[i]))
        })
    })

    names(comparisons) <- altitude_brackets_stats_df$`Altitude Bracket`[1:no_steps]
    names(comparisons) <- lapply(comparisons, function(x) setNames(x, comparison_types))

    # Create a data frame to store the comparisons
    comparison_df <- do.call(rbind, lapply(1:no_steps, function(i) {
        do.call(rbind, lapply(1:3, function(j) {
            P_Value <- comparisons[[i]][[j]]$p.value < alpha
            Estimate <- round(comparisons[[i]][[j]]$estimate, 3)
            Estimate <- ifelse(P_Value, paste0("$\\pmb{", Estimate, "}$"), Estimate)
            data.frame(
                Bracket = altitude_brackets_stats_df$`Altitude Bracket`,
                Type = comparison_types[j],
                Estimate = Estimate,
                Inc_to_next = avg_inc_in_alt[i],
                Mean_Altitude = mean_median_altitude$mean_altitude[i]
            )
        }))
    }))

    # Pivot wider on Type
    comparison_df_wide <- comparison_df %>% pivot_wider(names_from = Type, values_from = Estimate)
    colnames(comparison_df_wide) <- c("Bracket", "Increase", "Mean altitude", "Overall", "Firearms", "Other")
    return(comparison_df_wide)
}

# Function to generate LaTeX tables and plots for a given model
generate_latex_and_plots <- function(model, output_prefix, title, dep_var_labels, ylim_range, omit = c()) {
    (model_res <- plot_and_calculate_slopes(model))
    # Use stargazer to create a LaTeX table with each covariate in one row
    stargazer(
        ... = model, type = "latex", out = paste0("tables/", output_prefix, "_results.tex"),
        title = title,
        label = paste0("tab:", output_prefix, "_result"),
        dep.var.labels = c("Overall suicides", "Firearm suicides", "Nonfirearm suicides"),
        font.size = "scriptsize",
        omit = omit,
        single.row = TRUE
    )

    # Plot the model
    png(paste0("figures/", output_prefix, "_marginal_effect.png"), height = 500, width = 1000)
    print(model_res$combined_plot + coord_cartesian(ylim = ylim_range))
    dev.off()

    # Create LaTeX table for the model
    comparison_df_wide <- create_comparison_df(model, altitude_brackets_stats_df)
    comparison_latex_wide <- xtable(comparison_df_wide,
        caption = paste("Comparisons of Marginal Effects by Altitude Bracket for", title),
        label = paste0("tab:", output_prefix, "_comparison")
    )
    print(comparison_latex_wide,
        type = "latex",
        file = paste0("tables/", output_prefix, "_comparison.tex"),
        include.rownames = FALSE,
        size = "\\footnotesize",
        sanitize.text.function = function(x) x
    )
}

# Function to extract coefficients
extract_coefficients <- function(model, pattern) {
    model$coefficients %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        filter(grepl(pattern, rowname)) %>%
        select(2) %>%
        unlist()
}

# Function to add the marginal effect of moving up 1 altitude bracket in model with altitude fixed effects
add_marginal_effects <- function(altitude_brackets_stats_df, models) {
    marginal_effects <- lapply(models, function(model) {
        c(0, extract_coefficients(model, "altitude")) %>%
            diff() %>%
            c(NA)
    })

    altitude_brackets_stats_df$marginal_effect_overall <- marginal_effects[[1]]
    altitude_brackets_stats_df$marginal_effect_firearms <- marginal_effects[[2]]
    altitude_brackets_stats_df$marginal_effect_other <- marginal_effects[[3]]

    colnames(altitude_brackets_stats_df) <- c("Altitude Bracket", "Count", "Mean Altitude", "Median Altitude", "Overall", "Firearms", "Nonfirearms")

    return(altitude_brackets_stats_df)
}

# Function to calculate the mean altitude for each altitude bracket in a model with altitude fixed effects
calculate_altitude_brackets_stats <- function(suicide_data, suicide_data_fe, step_size, no_alt_steps, brenner_lms) {
    altitude_brackets_text <- c(
        paste0(
            seq(step_size, (no_alt_steps - 1) * step_size, step_size),
            "m < Altitude $\\leq$ ",
            seq(step_size * 2, no_alt_steps * step_size, step_size),
            "m"
        ),
        paste0(no_alt_steps * step_size, "m $\\leq$ Altitude")
    )
    altitude_brackets_stats <- table(suicide_data_fe$altitude)
    names(altitude_brackets_stats) <- c(paste0("Altitude $\\leq$ ", step_size, " m"), altitude_brackets_text)

    # Print altitude_brackets_stats in a vertical LaTeX table
    altitude_brackets_stats_df <- as.data.frame(altitude_brackets_stats)
    altitude_brackets_stats_df$group <- 0:9

    # Calculate mean altitude for each bracket
    altitude_brackets_data <- suicide_data %>%
        mutate(altitude_groups = pmin(pmax(floor(altitude / step_size), 0), no_alt_steps)) %>%
        filter(!is.na(altitude))
    mean_median_altitude <- altitude_brackets_data %>%
        group_by(altitude_groups) %>%
        summarize(mean_altitude = mean(as.numeric(altitude), na.rm = T), median_altitude = median(as.numeric(altitude), na.rm = T))

    # Merge mean altitude with altitude_brackets_stats_df
    altitude_brackets_stats_df <- altitude_brackets_stats_df %>%
        left_join(mean_median_altitude, by = c("group" = "altitude_groups")) %>%
        select(-group)

    # Calculate marginal effects
    altitude_brackets_stats_df <- add_marginal_effects(altitude_brackets_stats_df, brenner_lms)

    return(altitude_brackets_stats_df)
}
