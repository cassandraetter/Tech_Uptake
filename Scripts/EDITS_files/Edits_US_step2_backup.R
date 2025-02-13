US <- US %>%
        group_by(pid) %>%
        arrange(pid, year) %>%
        mutate(
                # Track those who switched to electric
                switched_to_electric = lag(heat_method) %in% c(1,3,4,5,10,11) & 
                        heat_method == 2,
                
                # Track those always on electric
                always_electric = all(heat_method == 2, na.rm = TRUE),
                
                # Track those who switched to solar
                switched_to_solar = lag(heat_method) %in% c(1,3,4,5,10,11) & 
                        heat_method == 6,
                
                # Track those always on solar
                always_solar = all(heat_method == 6, na.rm = TRUE),
                
                # Combined measures: either switched or always been on that method
                using_electric = heat_method == 2,
                using_solar = heat_method == 6,
                
                # Switch years
                switch_year_electric = case_when(
                        switched_to_electric ~ year,
                        TRUE ~ NA_real_
                ),
                switch_year_solar = case_when(
                        switched_to_solar ~ year,
                        TRUE ~ NA_real_
                )
        ) %>%
        ungroup()

# For decile analysis:
uptake_rates <- US %>%
        group_by(year, decile) %>%
        summarise(
                total_households = n_distinct(pid),
                
                # Electric heating metrics
                new_electric_switches = sum(switched_to_electric, na.rm = TRUE),
                total_electric_users = sum(using_electric, na.rm = TRUE),
                
                # Solar heating metrics
                new_solar_switches = sum(switched_to_solar, na.rm = TRUE),
                total_solar_users = sum(using_solar, na.rm = TRUE),
                
                # Calculate rates
                electric_switch_rate = (new_electric_switches / total_households) * 100,
                total_electric_rate = (total_electric_users / total_households) * 100,
                solar_switch_rate = (new_solar_switches / total_households) * 100,
                total_solar_rate = (total_solar_users / total_households) * 100
        ) %>%
        ungroup()

library(ggplot2)
library(patchwork)  # For combining plots

# 1. Switching rates over time by decile
p1 <- ggplot(uptake_rates, aes(x = year, y = electric_switch_rate, color = factor(decile))) +
        geom_line() +
        theme_minimal() +
        labs(
                title = "New Electric Heating Adoption by Decile",
                x = "Year",
                y = "Switch Rate (%)",
                color = "Income Decile"
        )

p2 <- ggplot(uptake_rates, aes(x = year, y = solar_switch_rate, color = factor(decile))) +
        geom_line() +
        theme_minimal() +
        labs(
                title = "New Solar Heating Adoption by Decile",
                x = "Year",
                y = "Switch Rate (%)",
                color = "Income Decile"
        )

# 2. Total usage rates over time by decile
p3 <- ggplot(uptake_rates, aes(x = year, y = total_electric_rate, color = factor(decile))) +
        geom_line() +
        theme_minimal() +
        labs(
                title = "Total Electric Heating Usage by Decile",
                x = "Year",
                y = "Total Usage Rate (%)",
                color = "Income Decile"
        )

p4 <- ggplot(uptake_rates, aes(x = year, y = total_solar_rate, color = factor(decile))) +
        geom_line() +
        theme_minimal() +
        labs(
                title = "Total Solar Heating Usage by Decile",
                x = "Year",
                y = "Total Usage Rate (%)",
                color = "Income Decile"
        )

# Combine all plots
(p1 + p2) / (p3 + p4)

# Optional: Create summary statistics for the most recent year
latest_year_stats <- uptake_rates %>%
        filter(year == max(year)) %>%
        select(decile, total_electric_rate, total_solar_rate) %>%
        arrange(desc(total_electric_rate))

# Print summary of latest year
print(latest_year_stats)

# Calculate cumulative adoption by decile
cumulative_adoption <- uptake_rates %>%
        group_by(decile) %>%
        arrange(year) %>%
        mutate(
                cum_electric_switches = cumsum(new_electric_switches),
                cum_solar_switches = cumsum(new_solar_switches),
                cum_electric_rate = (cum_electric_switches / total_households) * 100,
                cum_solar_rate = (cum_solar_switches / total_households) * 100
        )

# Plot cumulative adoption
p5 <- ggplot(cumulative_adoption, aes(x = year, y = cum_electric_rate, color = factor(decile))) +
        geom_line() +
        theme_minimal() +
        labs(
                title = "Cumulative Electric Heating Adoption by Decile",
                x = "Year",
                y = "Cumulative Adoption Rate (%)",
                color = "Income Decile"
        )

p6 <- ggplot(cumulative_adoption, aes(x = year, y = cum_solar_rate, color = factor(decile))) +
        geom_line() +
        theme_minimal() +
        labs(
                title = "Cumulative Solar Heating Adoption by Decile",
                x = "Year",
                y = "Cumulative Adoption Rate (%)",
                color = "Income Decile"
        )

# Show cumulative plots
p5 + p6