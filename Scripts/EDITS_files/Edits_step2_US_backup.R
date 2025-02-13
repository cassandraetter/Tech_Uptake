# Define technologies and their labels
tech_columns <- c("smartphone_prop", "hybrid_prop", "solar_prop", 
                  "internet_prop", "repairs_prop", "skills_prop",
                  "fuel_switch_prop")

tech_labels <- c(
        "Smartphone" = "smartphone_prop",
        "Hybrid/EV" = "hybrid_prop",
        "Solar Panels" = "solar_prop",
        "Internet" = "internet_prop",
        "Home Repairs" = "repairs_prop",
        "Digital Skills" = "skills_prop",
        "Clean Heating" = "fuel_switch_prop"
)

# Calculate uptake using base R
# First get 2015 and 2021 data
data_2015 <- us_props[us_props$year == 2015, ]
data_2021 <- us_props[us_props$year == 2021, ]

# Create empty list to store results
uptake_results <- list()

# Calculate uptake for each decile and technology
for(d in 1:10) {
        for(tech in tech_columns) {
                initial_adoption <- data_2015[data_2015$decile == d, tech]
                final_adoption <- data_2021[data_2021$decile == d, tech]
                
                uptake_results[[length(uptake_results) + 1]] <- data.frame(
                        decile = d,
                        technology = tech,
                        initial_adoption = initial_adoption,
                        final_adoption = final_adoption,
                        total_uptake = final_adoption - initial_adoption,
                        percent_change = ifelse(initial_adoption == 0, NA, 
                                                (final_adoption - initial_adoption) / initial_adoption * 100)
                )
        }
}

# Combine results into a data frame
uptake_by_decile <- do.call(rbind, uptake_results)

# Add technology labels
uptake_by_decile$technology_label <- names(tech_labels)[match(uptake_by_decile$technology, tech_labels)]

# Create heatmap of uptake rates
ggplot(uptake_by_decile, 
       aes(x = factor(decile), y = technology_label, fill = total_uptake)) +
        geom_tile() +
        scale_fill_gradient2(
                low = "red",
                mid = "white",
                high = "darkgreen",
                midpoint = 0
        ) +
        theme_minimal() +
        labs(
                title = "Technology Uptake by Income Decile (2015-2021)",
                subtitle = "Change in adoption rates (percentage points)",
                x = "Income Decile",
                y = NULL,
                fill = "Change in\nAdoption Rate"
        ) +
        theme(
                panel.grid = element_blank(),
                axis.text = element_text(size = 10)
        )

# Calculate summary statistics
uptake_summary <- data.frame(
        technology = unique(uptake_by_decile$technology_label),
        avg_uptake = tapply(uptake_by_decile$total_uptake, uptake_by_decile$technology_label, mean, na.rm = TRUE),
        min_uptake = tapply(uptake_by_decile$total_uptake, uptake_by_decile$technology_label, min, na.rm = TRUE),
        max_uptake = tapply(uptake_by_decile$total_uptake, uptake_by_decile$technology_label, max, na.rm = TRUE)
)

# Print summary
print("Summary of Uptake Rates 2015-2021:")
print(uptake_summary[order(-uptake_summary$avg_uptake), ])

# Create boxplot
ggplot(uptake_by_decile, 
       aes(y = reorder(technology_label, total_uptake), x = total_uptake)) +
        geom_boxplot(fill = "lightblue", alpha = 0.5) +
        geom_jitter(height = 0.2, alpha = 0.5) +
        theme_minimal() +
        labs(
                title = "Distribution of Technology Uptake by Income Decile (2015-2021)",
                subtitle = "Percentage point change in adoption",
                x = "Change in Adoption Rate",
                y = NULL
        ) +
        theme(
                panel.grid.minor = element_blank(),
                axis.text.y = element_text(hjust = 1)
        )