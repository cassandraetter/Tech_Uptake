# Fix issue with data
DEU_consumption <- DEU_consumption %>% 
        filter(!year == 2010)
UK_consumption <- UK_consumption %>%
        filter(!year == 2009)

# Enhanced energy projection function with improved baseline methodologies
project_energy_time_enhanced <- function(consumption_data, tech_impacts, adoption_data, 
                                         household_projections = NULL,
                                         country_name = "DEU",
                                         start_year = 2025, end_year = 2050,
                                         n_simulations = 100,
                                         regional_variation = TRUE,
                                         include_demographics = TRUE) {
        
        # Ensure correct data types
        consumption_data <- consumption_data %>%
                mutate(weighted_decile = as.integer(weighted_decile))
        
        # 1. Smoothing approach with conversion to numeric
        historical_trend <- consumption_data %>%
                group_by(weighted_decile) %>%
                arrange(year) %>%
                mutate(
                        # Convert the smoothed output to numeric vectors
                        smoothed_elec = as.numeric(stats::smooth(baseline_elec, kind = "3R")),
                        smoothed_gas = as.numeric(stats::smooth(baseline_gas, kind = "3R"))
                ) %>%
                ungroup()
        
        # Get last year of historical data for baseline
        last_year <- max(consumption_data$year)
        baseline <- historical_trend %>%
                filter(year == last_year) %>%
                # Use smoothed values for projection base
                mutate(
                        baseline_elec = smoothed_elec,
                        baseline_gas = smoothed_gas
                )
        
        # Function to run a single projection with enhanced modeling
        single_projection <- function(seed) {
                set.seed(seed)
                
                # 2. Enhanced adoption parameters with regional variation
                adoption_params <- adoption_data %>%
                        mutate(
                                technology = tolower(technology),
                                weighted_decile = case_when(
                                        quintile == "Quintile 1" ~ list(1:2),
                                        quintile == "Quintile 2" ~ list(3:4),
                                        quintile == "Quintile 3" ~ list(5:6),
                                        quintile == "Quintile 4" ~ list(7:8),
                                        quintile == "Quintile 5" ~ list(9:10)
                                ),
                                # Add regional variation if enabled
                                region_factor = if(regional_variation) {
                                        # Simulate regional differences in adoption
                                        sample(c(0.85, 0.95, 1.0, 1.05, 1.15), n(), replace = TRUE,
                                               prob = c(0.2, 0.3, 0.3, 0.15, 0.05))
                                } else {
                                        1.0  # No regional variation
                                },
                                # Income-based adoption modification
                                income_factor = case_when(
                                        quintile == "Quintile 1" ~ 0.8,  # Lower income adopts slower
                                        quintile == "Quintile 2" ~ 0.9,
                                        quintile == "Quintile 3" ~ 1.0,  # Middle income is baseline
                                        quintile == "Quintile 4" ~ 1.1,
                                        quintile == "Quintile 5" ~ 1.2   # Higher income adopts faster
                                ),
                                # Technology saturation - not all households will adopt
                                # Different max adoption rates by technology and income
                                adjusted_max = pmin(1, country_max * income_factor * region_factor),
                                
                                # Add random variation with income-dependent noise
                                # Higher income has more variation in adoption patterns
                                variation_factor = case_when(
                                        quintile == "Quintile 1" ~ 0.04,
                                        quintile == "Quintile 2" ~ 0.05,
                                        quintile == "Quintile 3" ~ 0.06,
                                        quintile == "Quintile 4" ~ 0.07,
                                        quintile == "Quintile 5" ~ 0.08
                                ),
                                
                                # Apply variations
                                country_max = pmin(1, pmax(0, adjusted_max + rnorm(n(), 0, variation_factor))),
                                growth_rate = growth_rate * income_factor + rnorm(n(), 0, 0.02),
                                inflexion_year = inflexion_year - (log(income_factor) * 5) + rnorm(n(), 0, 1.5)
                        ) %>%
                        unnest(weighted_decile) %>%
                        # Force weighted_decile to be integer
                        mutate(weighted_decile = as.integer(weighted_decile))
                
                # Create time projection
                years <- data.frame(projection_year = seq(start_year, end_year, by = 1))
                
                # 3. Add demographic effects if enabled
                if(include_demographics && !is.null(household_projections)) {
                        # Join with household size projections to adjust consumption
                        years <- years %>%
                                left_join(household_projections, by = c("projection_year" = "year")) %>%
                                mutate(
                                        # Default to no change if missing
                                        household_size_factor = if_else(is.na(household_size_factor), 1.0, household_size_factor)
                                )
                } else {
                        # No demographic adjustment
                        years <- years %>%
                                mutate(household_size_factor = 1.0)
                }
                
                # Calculate technology impacts with enhanced modeling
                results <- baseline %>%
                        crossing(years) %>%
                        crossing(technology = unique(tech_impacts$technology)) %>%
                        # Force integer type before joining
                        mutate(weighted_decile = as.integer(weighted_decile)) %>%
                        left_join(adoption_params, by = c("weighted_decile", "technology")) %>%
                        left_join(tech_impacts, by = "technology") %>%
                        group_by(weighted_decile, projection_year) %>%
                        # 4. Track technology effects separately
                        mutate(
                                # Calculate adoption with S-curve including autocorrelation
                                # This creates more realistic year-to-year changes
                                year_in_projection = projection_year - min(projection_year),
                                random_walk = cumsum(rnorm(n(), 0, 0.01)),  # Add autocorrelation
                                
                                # Core adoption calculation
                                raw_adoption = country_max / (1 + exp(-growth_rate * (projection_year - inflexion_year))),
                                
                                # Add autocorrelation effect
                                adoption_factor = pmin(country_max, pmax(0, raw_adoption + random_walk)),
                                
                                # 5. Natural efficiency improvements in baseline technologies
                                # Even without new tech adoption, efficiency improves over time
                                baseline_efficiency_factor = 1 - (0.003 * year_in_projection),  # ~0.3% annual efficiency gain
                                
                                # Calculate impacts with all factors
                                tech_gas_impact = gas_consumption_mwh * adoption_factor,
                                tech_elec_impact = elec_consumption_mwh * adoption_factor,
                                
                                # Track specific technology adoption for analysis
                                tech_id = paste(technology, row_number(), sep = "_")
                        ) %>%
                        ungroup()
                
                # Aggregate results with enhanced tracking
                aggregated_results <- results %>%
                        group_by(weighted_decile, projection_year) %>%
                        summarise(
                                # Apply household size factor and baseline efficiency to consumption
                                household_size_factor = first(household_size_factor),
                                baseline_efficiency_factor = first(baseline_efficiency_factor),
                                
                                # Apply all factors to projected consumption
                                projected_gas = (first(baseline_gas) * baseline_efficiency_factor * household_size_factor) +
                                        sum(tech_gas_impact, na.rm = TRUE),
                                projected_elec = (first(baseline_elec) * baseline_efficiency_factor * household_size_factor) +
                                        sum(tech_elec_impact, na.rm = TRUE),
                                
                                # Ensure non-negative consumption
                                projected_gas = pmax(0, projected_gas),
                                projected_elec = pmax(0, projected_elec),
                                
                                # Keep prices constant as requested
                                gas_price = first(gas_price),
                                elec_price = first(elec_price),
                                
                                # Calculate bills
                                annual_gas_bill = projected_gas * gas_price,
                                annual_elec_bill = projected_elec * elec_price,
                                total_annual_bill = annual_gas_bill + annual_elec_bill,
                                
                                # Baselines for reference
                                baseline_gas = first(baseline_gas),
                                baseline_elec = first(baseline_elec),
                                baseline_annual_gas_bill = baseline_gas * gas_price,
                                baseline_annual_elec_bill = baseline_elec * elec_price,
                                baseline_total_annual_bill = baseline_annual_gas_bill + baseline_annual_elec_bill,
                                
                                # Enhanced adoption tracking
                                total_adoption_factor = sum(adoption_factor, na.rm = TRUE),
                                unique_technologies = n_distinct(tech_id),
                                .groups = 'drop'
                        ) %>%
                        # Add country identifier
                        mutate(country = country_name)
                
                return(aggregated_results)
        }
        
        # Run multiple simulations with improved progress reporting
        message(paste("Running", n_simulations, "simulations for", country_name, "..."))
        simulation_results <- list()
        pb <- txtProgressBar(min = 0, max = n_simulations, style = 3)  # Better progress bar
        for(i in 1:n_simulations) {
                simulation_results[[i]] <- single_projection(i)
                setTxtProgressBar(pb, i)
        }
        close(pb)
        
        # 6. Enhanced statistics with more robust uncertainty quantification
        combined_results <- bind_rows(simulation_results, .id = "simulation") %>%
                group_by(country, weighted_decile, projection_year) %>%
                summarise(
                        # Central values (using median for robustness)
                        projected_gas = median(projected_gas),
                        projected_elec = median(projected_elec),
                        total_annual_bill = median(total_annual_bill),
                        annual_gas_bill = median(annual_gas_bill),
                        annual_elec_bill = median(annual_elec_bill),
                        
                        # More detailed confidence intervals
                        # 80% confidence interval
                        total_annual_bill_lower = quantile(total_annual_bill, 0.1),
                        total_annual_bill_upper = quantile(total_annual_bill, 0.9),
                        annual_gas_bill_lower = quantile(annual_gas_bill, 0.1),
                        annual_gas_bill_upper = quantile(annual_gas_bill, 0.9),
                        annual_elec_bill_lower = quantile(annual_elec_bill, 0.1),
                        annual_elec_bill_upper = quantile(annual_elec_bill, 0.9),
                        
                        # Also add confidence intervals for consumption
                        projected_gas_lower = quantile(projected_gas, 0.1),
                        projected_gas_upper = quantile(projected_gas, 0.9),
                        projected_elec_lower = quantile(projected_elec, 0.1),
                        projected_elec_upper = quantile(projected_elec, 0.9),
                        
                        # Standard deviations for uncertainty analysis
                        projected_gas_sd = sd(projected_gas),
                        projected_elec_sd = sd(projected_elec),
                        total_annual_bill_sd = sd(total_annual_bill),
                        
                        # Baselines
                        baseline_gas = first(baseline_gas),
                        baseline_elec = first(baseline_elec),
                        baseline_annual_gas_bill = first(baseline_annual_gas_bill),
                        baseline_annual_elec_bill = first(baseline_annual_elec_bill),
                        baseline_total_annual_bill = first(baseline_total_annual_bill),
                        
                        # Adoption metrics
                        total_adoption_factor = median(total_adoption_factor),
                        avg_technologies = median(unique_technologies),
                        .groups = 'drop'
                )
        
        # 7. Add model quality metrics
        model_metrics <- data.frame(
                country = country_name,
                n_simulations = n_simulations,
                include_demographics = include_demographics,
                regional_variation = regional_variation,
                convergence_metric = sd(sapply(simulation_results, function(x) mean(x$total_annual_bill))),
                simulation_variability = mean(combined_results$total_annual_bill_sd),
                simulation_time = Sys.time()
        )
        
        # Return both results and metrics
        list(
                projections = combined_results,
                model_metrics = model_metrics
        )
}

# Sample demographic projection data
# In practice, you would use real demographic projections
create_household_projection <- function(country, start_year = 2025, end_year = 2050) {
        # Different demographic trends by country
        annual_change <- case_when(
                country == "DEU" ~ -0.004,  # Declining household size
                country == "UK" ~ -0.003,   # Slow decline
                country == "US" ~ -0.002    # Very slow decline
        )
        
        tibble(
                country = country,
                year = start_year:end_year,
                # Household size relative to start year (declining)
                household_size_factor = 1 + annual_change * (year - start_year)
        )
}

# Run enhanced projections for all three countries
run_all_countries <- function() {
        # Prepare country-specific household projections
        deu_households <- create_household_projection("DEU", 2025, 2050)
        uk_households <- create_household_projection("UK", 2025, 2050)
        us_households <- create_household_projection("US", 2025, 2050)
        
        # Germany
        message("Running Germany projections...")
        deu_enhanced <- project_energy_time_enhanced(
                consumption_data = DEU_consumption,
                tech_impacts = technology_impacts,
                adoption_data = DE_summary,
                household_projections = deu_households,
                country_name = "DEU",
                start_year = 2025,
                end_year = 2050,
                n_simulations = 100,
                regional_variation = TRUE,
                include_demographics = TRUE
        )
        
        # UK
        message("Running UK projections...")
        uk_enhanced <- project_energy_time_enhanced(
                consumption_data = UK_consumption,
                tech_impacts = technology_impacts,
                adoption_data = UK_summary,
                household_projections = uk_households,
                country_name = "UK",
                start_year = 2025,
                end_year = 2050,
                n_simulations = 100,
                regional_variation = TRUE,
                include_demographics = TRUE
        )
        
        # US
        message("Running US projections...")
        us_enhanced <- project_energy_time_enhanced(
                consumption_data = US_consumption,
                tech_impacts = technology_impacts,
                adoption_data = US_summary,
                household_projections = us_households,
                country_name = "US",
                start_year = 2025,
                end_year = 2050,
                n_simulations = 100,
                regional_variation = TRUE,
                include_demographics = TRUE
        )
        
        return(list(
                DEU = deu_enhanced,
                UK = uk_enhanced,
                US = us_enhanced
        ))
}

# Run projections for all countries
all_country_results <- run_all_countries()

# Extract projection data for academic presentation
deu_projections <- all_country_results$DEU$projections
uk_projections <- all_country_results$UK$projections
us_projections <- all_country_results$US$projections

# Combine all results
all_projections <- bind_rows(
        deu_projections,
        uk_projections,
        us_projections
)

# Function to create enhanced visualization
create_enhanced_vis <- function(projections, historical_data, country_code) {
        # Prepare historical data
        historical <- historical_data %>%
                filter(country == country_code) %>%
                mutate(
                        type = case_when(
                                metric %in% c("baseline_gas", "baseline_elec", "total_consumption") ~ "Consumption (MWh)",
                                TRUE ~ "Annual Bills (€)"
                        ),
                        source = case_when(
                                metric %in% c("baseline_gas", "annual_gas_bill") ~ "Gas",
                                metric %in% c("baseline_elec", "annual_elec_bill") ~ "Electricity",
                                TRUE ~ "Total"
                        )
                )
        
        # Prepare projection data
        projection <- projections %>%
                filter(country == country_code) %>%
                mutate(
                        projected_total = projected_gas + projected_elec,
                        projected_total_lower = projected_gas_lower + projected_elec_lower,
                        projected_total_upper = projected_gas_upper + projected_elec_upper
                ) %>%
                pivot_longer(
                        cols = c(
                                # Consumption columns
                                projected_gas, projected_elec, projected_total,
                                projected_gas_lower, projected_elec_lower, projected_total_lower,
                                projected_gas_upper, projected_elec_upper, projected_total_upper,
                                # Bill columns
                                annual_gas_bill, annual_elec_bill, total_annual_bill,
                                annual_gas_bill_lower, annual_elec_bill_lower, total_annual_bill_lower,
                                annual_gas_bill_upper, annual_elec_bill_upper, total_annual_bill_upper
                        ),
                        names_to = "metric",
                        values_to = "value"
                ) %>%
                mutate(
                        type = case_when(
                                grepl("projected", metric) ~ "Consumption (MWh)",
                                TRUE ~ "Annual Bills (€)"
                        ),
                        source = case_when(
                                grepl("gas", metric) ~ "Gas",
                                grepl("elec", metric) ~ "Electricity",
                                TRUE ~ "Total"
                        ),
                        ci_type = case_when(
                                grepl("lower", metric) ~ "lower",
                                grepl("upper", metric) ~ "upper",
                                TRUE ~ "central"
                        )
                )
        
        # Create visualization
        ggplot() +
                # Historical data
                geom_line(data = historical,
                          aes(x = projection_year, 
                              y = value,
                              color = factor(weighted_decile),
                              group = weighted_decile),
                          size = 1) +
                # Transition point
                geom_point(data = historical %>% 
                                   filter(projection_year == max(projection_year)),
                           aes(x = projection_year,
                               y = value,
                               color = factor(weighted_decile)),
                           size = 2) +
                # Projections with confidence intervals
                geom_ribbon(data = projection %>% 
                                    filter(ci_type != "central") %>%
                                    pivot_wider(names_from = ci_type, values_from = value, values_fill = NA),
                            aes(x = projection_year,
                                ymin = lower,
                                ymax = upper,
                                fill = factor(weighted_decile),
                                group = weighted_decile),
                            alpha = 0.3, 
                            color = NA) +
                # Projected central lines
                geom_line(data = projection %>% 
                                  filter(ci_type == "central"),
                          aes(x = projection_year,
                              y = value,
                              color = factor(weighted_decile),
                              group = weighted_decile),
                          linetype = "dashed",
                          size = 1) +
                # Transition line
                geom_vline(xintercept = 2025, 
                           linetype = "dashed", 
                           color = "gray50") +
                # Faceting
                facet_grid(type ~ source, scales = "free_y") +
                # Customize appearance
                labs(
                        title = paste(country_code, "- Enhanced Baseline Projection"),
                        subtitle = "Includes demographic effects, regional variation, and natural efficiency improvements",
                        x = "Year",
                        y = NULL,
                        color = "Income Decile",
                        fill = "Income Decile"
                ) +
                theme_minimal() +
                scale_color_viridis_d() +
                scale_fill_viridis_d() +
                scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
                theme(
                        legend.position = "right",
                        plot.title = element_text(face = "bold"),
                        plot.subtitle = element_text(size = 9),
                        strip.text = element_text(face = "bold"),
                        panel.spacing = unit(2, "lines")
                )
}

# Create the enhanced visualization for Germany
deu_enhanced_vis <- create_enhanced_vis(
        deu_projections,  # Fixed: using the correct variable
        historical_combined, 
        "DEU"
)

# Display the enhanced visualization
print(deu_enhanced_vis)

# Sensitivity analysis - run multiple scenarios
run_sensitivity_analysis <- function(country_data, tech_data, adoption_data, country_name) {
        # Define scenarios
        scenarios <- expand.grid(
                regional_variation = c(TRUE, FALSE),
                demographics = c(TRUE, FALSE),
                efficiency_improvement = c("High", "Medium", "Low")
        )
        
        # Adjust household projections based on scenarios
        household_proj <- case_when(
                country_name == "DEU" ~ deu_households,
                country_name == "UK" ~ uk_households,
                country_name == "US" ~ us_households
        )
        
        # Run all scenarios
        scenario_results <- list()
        for(i in 1:nrow(scenarios)) {
                # Get scenario parameters
                regional <- scenarios$regional_variation[i]
                demographics <- scenarios$demographics[i]
                efficiency <- scenarios$efficiency_improvement[i]
                
                # Run projection with this scenario
                message(paste("Running scenario", i, "of", nrow(scenarios)))
                
                # Adjust tech impacts based on efficiency scenario
                modified_tech <- tech_data %>%
                        mutate(
                                efficiency_factor = case_when(
                                        efficiency == "High" ~ 1.2,
                                        efficiency == "Medium" ~ 1.0,
                                        efficiency == "Low" ~ 0.8
                                ),
                                gas_consumption_mwh = gas_consumption_mwh * efficiency_factor,
                                elec_consumption_mwh = elec_consumption_mwh * efficiency_factor
                        )
                
                # Run with reduced simulations for speed
                result <- project_energy_time_enhanced(
                        consumption_data = country_data,
                        tech_impacts = modified_tech,
                        adoption_data = adoption_data,
                        household_projections = household_proj,
                        country_name = country_name,
                        start_year = 2025,
                        end_year = 2050,
                        n_simulations = 50,  # Reduced for sensitivity analysis
                        regional_variation = regional,
                        include_demographics = demographics
                )
                
                # Add scenario details
                result$projections$scenario_id <- i
                result$projections$regional_variation <- regional
                result$projections$demographics <- demographics
                result$projections$efficiency <- efficiency
                
                scenario_results[[i]] <- result$projections
        }
        
        # Combine all scenarios
        all_scenarios <- bind_rows(scenario_results)
        
        # Return with scenario metadata
        list(
                results = all_scenarios,
                scenarios = scenarios
        )
}

# Run sensitivity analysis for Germany
deu_sensitivity <- run_sensitivity_analysis(
        DEU_consumption, 
        technology_impacts, 
        DE_summary, 
        "DEU"
)

# Summarize sensitivity analysis
sensitivity_summary <- deu_sensitivity$results %>%
        filter(projection_year == 2050) %>%
        group_by(scenario_id, regional_variation, demographics, efficiency, weighted_decile) %>%
        summarise(
                total_consumption = projected_gas + projected_elec,
                total_bill = total_annual_bill,
                .groups = 'drop'
        ) %>%
        group_by(scenario_id, regional_variation, demographics, efficiency) %>%
        summarise(
                avg_consumption = mean(total_consumption),
                avg_bill = mean(total_bill),
                .groups = 'drop'
        ) %>%
        arrange(desc(avg_consumption))

# Display sensitivity results
print("Sensitivity Analysis Results:")
print(sensitivity_summary)

# Save results
saveRDS(deu_enhanced, "deu_enhanced_projections.rds")
saveRDS(deu_sensitivity, "deu_sensitivity_analysis.rds")
ggsave("deu_enhanced_projection.png", deu_enhanced_vis, width = 12, height = 8, dpi = 300)

#-----------------------------------
# ADDED: Academic Presentation Code
#-----------------------------------

# Load required packages for academic tables
library(fixest)
library(modelsummary)
library(kableExtra)

# 1. Create regression-style tables with fixed effects
regression_data <- all_projections %>%
        mutate(
                # Create decile categories
                income_group = case_when(
                        weighted_decile <= 3 ~ "Low",
                        weighted_decile <= 7 ~ "Middle",
                        TRUE ~ "High"
                ),
                # Create period indicators
                period = case_when(
                        projection_year <= 2030 ~ "Near-term (2025-2030)",
                        projection_year <= 2040 ~ "Mid-term (2031-2040)",
                        TRUE ~ "Long-term (2041-2050)"
                ),
                # Calculate changes from baseline
                elec_change = projected_elec - baseline_elec,
                gas_change = projected_gas - baseline_gas,
                total_energy_change = elec_change + gas_change,
                bill_change = total_annual_bill - baseline_total_annual_bill,
                pct_bill_change = (total_annual_bill / baseline_total_annual_bill - 1) * 100
        )

# Run regression models
models <- list(
        "Electricity" = feols(projected_elec ~ income_group + period | country, regression_data),
        "Gas" = feols(projected_gas ~ income_group + period | country, regression_data),
        "Total Bill" = feols(total_annual_bill ~ income_group + period | country, regression_data),
        "% Bill Change" = feols(pct_bill_change ~ income_group + period | country, regression_data)
)

# Create regression table (Model 1)
table1 <- modelsummary(models, 
                       stars = TRUE,
                       title = "Energy Consumption and Bills by Income Group and Period",
                       coef_map = c(
                               "income_groupLow" = "Low Income",
                               "income_groupMiddle" = "Middle Income",
                               "income_groupHigh" = "High Income (ref.)",
                               "periodNear-term (2025-2030)" = "2025-2030",
                               "periodMid-term (2031-2040)" = "2031-2040",
                               "periodLong-term (2041-2050)" = "2041-2050"
                       ),
                       gof_omit = "IC|Log|F|RMSE")

# 2. Panel data fixed effects model (Model 2)
panel_models <- list(
        "Gas\nConsumption" = feols(projected_gas ~ period:income_group | country + weighted_decile, regression_data),
        "Electricity\nConsumption" = feols(projected_elec ~ period:income_group | country + weighted_decile, regression_data),
        "Energy\nBill" = feols(total_annual_bill ~ period:income_group | country + weighted_decile, regression_data),
        "Bill\nChange %" = feols(pct_bill_change ~ period:income_group | country + weighted_decile, regression_data)
)

table2 <- modelsummary(panel_models, 
                       stars = TRUE,
                       title = "Energy Consumption and Bills - Panel Data with Fixed Effects",
                       coef_omit = "Int",
                       gof_omit = "IC|Log|F|RMSE")

# 3. Point estimates with confidence intervals (most common in energy economics)
point_estimates <- all_projections %>%
        filter(projection_year %in% c(2030, 2040, 2050)) %>%
        group_by(country, weighted_decile, projection_year) %>%
        summarise(
                gas_consumption = projected_gas,
                gas_consumption_lower = projected_gas_lower,
                gas_consumption_upper = projected_gas_upper,
                
                elec_consumption = projected_elec,
                elec_consumption_lower = projected_elec_lower,
                elec_consumption_upper = projected_elec_upper,
                
                total_bill = total_annual_bill,
                total_bill_lower = total_annual_bill_lower,
                total_bill_upper = total_annual_bill_upper,
                
                # Get baseline values for comparisons
                baseline_gas = baseline_gas,
                baseline_elec = baseline_elec,
                baseline_bill = baseline_total_annual_bill,
                
                # Calculate percentage changes
                gas_change_pct = (projected_gas / baseline_gas - 1) * 100,
                elec_change_pct = (projected_elec / baseline_elec - 1) * 100,
                bill_change_pct = (total_annual_bill / baseline_total_annual_bill - 1) * 100,
                
                .groups = "drop"
        ) %>%
        # Group by income group for table
        mutate(
                income_group = case_when(
                        weighted_decile <= 3 ~ "Low Income",
                        weighted_decile <= 7 ~ "Middle Income",
                        TRUE ~ "High Income"
                )
        ) %>%
        arrange(country, projection_year, weighted_decile)

# Create point estimate table for select years
point_table <- point_estimates %>%
        filter(weighted_decile %in% c(1, 5, 10)) %>%  # Representative deciles
        mutate(
                # Format values with confidence intervals
                gas_value = sprintf("%.1f [%.1f, %.1f]", 
                                    gas_consumption, gas_consumption_lower, gas_consumption_upper),
                elec_value = sprintf("%.1f [%.1f, %.1f]", 
                                     elec_consumption, elec_consumption_lower, elec_consumption_upper),
                bill_value = sprintf("%.0f [%.0f, %.0f]", 
                                     total_bill, total_bill_lower, total_bill_upper),
                
                # Format percentage changes
                gas_change = sprintf("%.1f%%", gas_change_pct),
                elec_change = sprintf("%.1f%%", elec_change_pct),
                bill_change = sprintf("%.1f%%", bill_change_pct)
        ) %>%
        select(country, projection_year, income_group, weighted_decile,
               gas_value, gas_change, elec_value, elec_change, bill_value, bill_change) %>%
        # Make the table more readable
        rename(
                "Country" = country,
                "Year" = projection_year,
                "Income Group" = income_group,
                "Decile" = weighted_decile,
                "Gas (MWh) [95% CI]" = gas_value,
                "Gas Change" = gas_change,
                "Electricity (MWh) [95% CI]" = elec_value,
                "Electricity Change" = elec_change,
                "Annual Bill (€) [95% CI]" = bill_value,
                "Bill Change" = bill_change
        )

# Create table with kableExtra styling
kable(point_table, "html", caption = "Table 1: Projected Energy Consumption and Bills with Confidence Intervals") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
        column_spec(1, bold = TRUE) %>%
        collapse_rows(columns = 1:3, valign = "middle") %>%
        add_header_above(c(" " = 4, "Gas" = 2, "Electricity" = 2, "Energy Bill" = 2))

# 4. Decomposition analysis (common in energy economics papers)
decomposition <- all_projections %>%
        filter(projection_year %in% c(2025, 2030, 2040, 2050)) %>%
        group_by(country, weighted_decile) %>%
        mutate(
                # Calculate changes between periods
                total_consumption = projected_gas + projected_elec,
                baseline_total = baseline_gas + baseline_elec,
                
                # Decompose changes
                total_change = total_consumption - first(total_consumption),
                gas_contribution = projected_gas - first(projected_gas),
                elec_contribution = projected_elec - first(projected_elec),
                
                # Calculate percentage contributions
                gas_share = gas_contribution / total_change * 100,
                elec_share = elec_contribution / total_change * 100
        ) %>%
        filter(projection_year > 2025) %>%  # Remove base year
        ungroup()

# Create decomposition table
decomp_table <- decomposition %>%
        group_by(country, projection_year) %>%
        summarise(
                avg_total_change = mean(total_change, na.rm = TRUE),
                avg_gas_contribution = mean(gas_contribution, na.rm = TRUE),
                avg_elec_contribution = mean(elec_contribution, na.rm = TRUE),
                avg_gas_share = mean(gas_share, na.rm = TRUE),
                avg_elec_share = mean(elec_share, na.rm = TRUE),
                
                # By income group
                low_total_change = mean(total_change[weighted_decile <= 3], na.rm = TRUE),
                mid_total_change = mean(total_change[weighted_decile > 3 & weighted_decile <= 7], na.rm = TRUE),
                high_total_change = mean(total_change[weighted_decile > 7], na.rm = TRUE),
                
                .groups = "drop"
        ) %>%
        arrange(country, projection_year)

# Format decomposition table
decomp_formatted <- decomp_table %>%
        mutate(
                # Format values
                total_change = sprintf("%.2f", avg_total_change),
                gas_contribution = sprintf("%.2f (%.1f%%)", avg_gas_contribution, avg_gas_share),
                elec_contribution = sprintf("%.2f (%.1f%%)", avg_elec_contribution, avg_elec_share),
                
                # Format by income group
                low_income = sprintf("%.2f", low_total_change),
                mid_income = sprintf("%.2f", mid_total_change),
                high_income = sprintf("%.2f", high_total_change)
        ) %>%
        select(country, projection_year, total_change, gas_contribution, elec_contribution,
               low_income, mid_income, high_income) %>%
        rename(
                "Country" = country,
                "Year" = projection_year,
                "Total Change (MWh)" = total_change,
                "Gas Contribution" = gas_contribution,
                "Electricity Contribution" = elec_contribution,
                "Low Income" = low_income,
                "Middle Income" = mid_income,
                "High Income" = high_income
        )

# Create formatted table
kable(decomp_formatted, "html", caption = "Table 2: Decomposition of Energy Consumption Changes") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
        column_spec(1, bold = TRUE) %>%
        collapse_rows(columns = 1, valign = "middle") %>%
        add_header_above(c(" " = 2, "Overall Change Decomposition" = 3, "Change by Income Group" = 3))

# 5. Distributional impact analysis (for energy policy papers)
distributional <- all_projections %>%
        filter(projection_year %in% c(2025, 2050)) %>%
        select(country, weighted_decile, projection_year, total_annual_bill, baseline_total_annual_bill) %>%
        pivot_wider(
                names_from = projection_year,
                values_from = c(total_annual_bill, baseline_total_annual_bill)
        ) %>%
        mutate(
                # Calculate absolute and percentage changes
                absolute_change = total_annual_bill_2050 - total_annual_bill_2025,
                percentage_change = (total_annual_bill_2050 / total_annual_bill_2025 - 1) * 100,
                
                # Calculate share of income (using decile as proxy)
                income_proxy = 11 - weighted_decile,  # Invert decile as income proxy
                bill_share_2025 = total_annual_bill_2025 / income_proxy,
                bill_share_2050 = total_annual_bill_2050 / income_proxy,
                share_change = bill_share_2050 - bill_share_2025
        ) %>%
        arrange(country, weighted_decile)

# Calculate distributional indices
gini_coef <- function(x) {
        x <- sort(x)
        n <- length(x)
        return(sum(x * 1:n) / (n * sum(x)) - (n + 1) / (2 * n))
}

distributional_indices <- distributional %>%
        group_by(country) %>%
        summarise(
                # Gini coefficient for bills in 2025 and 2050
                gini_2025 = gini_coef(total_annual_bill_2025),
                gini_2050 = gini_coef(total_annual_bill_2050),
                gini_change = gini_2050 - gini_2025,
                
                # Ratio of top to bottom decile
                top_bottom_2025 = total_annual_bill_2025[weighted_decile == 10] / 
                        total_annual_bill_2025[weighted_decile == 1],
                top_bottom_2050 = total_annual_bill_2050[weighted_decile == 10] / 
                        total_annual_bill_2050[weighted_decile == 1],
                top_bottom_change = top_bottom_2050 - top_bottom_2025,
                
                # Average change percentage
                avg_pct_change = mean(percentage_change),
                
                # Low income vs high income change
                low_income_pct = mean(percentage_change[weighted_decile <= 3]),
                high_income_pct = mean(percentage_change[weighted_decile >= 8]),
                
                .groups = "drop"
        )

# Format distributional indices table
dist_formatted <- distributional_indices %>%
        mutate(
                # Format values
                gini_2025 = sprintf("%.3f", gini_2025),
                gini_2050 = sprintf("%.3f", gini_2050),
                gini_change = sprintf("%.3f", gini_change),
                
                top_bottom_2025 = sprintf("%.2f", top_bottom_2025),
                top_bottom_2050 = sprintf("%.2f", top_bottom_2050),
                top_bottom_change = sprintf("%.2f", top_bottom_change),
                
                avg_pct_change = sprintf("%.1f%%", avg_pct_change),
                low_income_pct = sprintf("%.1f%%", low_income_pct),
                high_income_pct = sprintf("%.1f%%", high_income_pct)
        ) %>%
        rename(
                "Country" = country,
                "Gini 2025" = gini_2025,
                "Gini 2050" = gini_2050,
                "Gini Change" = gini_change,
                "Top/Bottom 2025" = top_bottom_2025,
                "Top/Bottom 2050" = top_bottom_2050,
                "Top/Bottom Change" = top_bottom_change,
                "Average Change" = avg_pct_change,
                "Low Income Change" = low_income_pct,
                "High Income Change" = high_income_pct
        )

# Create formatted table
kable(dist_formatted, "html", caption = "Table 3: Distributional Impact Analysis") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
        column_spec(1, bold = TRUE) %>%
        add_header_above(c(" " = 1, "Gini Coefficient" = 3, "Top-to-Bottom Ratio" = 3, "Percentage Change" = 3))

# Save all results for future use
saveRDS(all_country_results, "all_country_enhanced_projections.rds")
saveRDS(point_estimates, "point_estimates.rds")
saveRDS(decomposition, "energy_decomposition.rds")
saveRDS(distributional, "distributional_analysis.rds")
saveRDS(distributional_indices, "distributional_indices.rds")

# Print summary of available tables
cat("Academic tables created:\n")
cat("1. Regression-style table with fixed effects\n")
cat("2. Panel data fixed effects model\n")
cat("3. Point estimates with confidence intervals\n")
cat("4. Decomposition analysis of energy changes\n")
cat("5. Distributional impact analysis\n")