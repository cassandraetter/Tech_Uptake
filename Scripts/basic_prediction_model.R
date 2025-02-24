
income_growth_rate <- mean(diff(log(energy_data_uk$adjusted_income_2yr)), na.rm = TRUE)
projected_income <- rep(tail(energy_data_uk$adjusted_income_2yr, 1) * (1 + income_growth_rate)^(1:29), 
                        each = length(unique(energy_data_uk$weighted_decile)))

# Create a future data frame for years 2022 to 2050
future_years <- data.frame(year = 2022:2050)

num_households <- nrow(energy_data_uk)

projected_income_all <- rep(projected_income, length.out = num_households * length(future_years$year))

future_data <- data.frame(
        year = rep(future_years$year, each = num_households),  # Repeat for each year
        hidp = rep(energy_data_uk$hidp, times = length(future_years$year)),  # Use existing 'hidp' as household ID
        weighted_decile = rep(energy_data_uk$weighted_decile, times = length(future_years$year)),
        elec_price = rep(forecasted_UK_elec_prices, length.out = num_households * length(future_years$year)),
        gas_price = rep(forecasted_UK_gas_prices, length.out = num_households * length(future_years$year)),
        adjusted_income_2yr = projected_income_all,  # Projected income, constant for each household
        hhsize = rep(energy_data_uk$hhsize, times = length(future_years$year)),  # Keep hhsize constant
        urban_dv = rep(energy_data_uk$urban_dv, times = length(future_years$year)),
        age_dv = rep(energy_data_uk$age_dv, times = length(future_years$year)),
        hsrooms = rep(energy_data_uk$hsrooms, times = length(future_years$year)),
        tenure_dv = rep(energy_data_uk$tenure_dv, times = length(future_years$year)),
        gor_dv = rep(energy_data_uk$gor_dv, times = length(future_years$year))
)

# Check the structure of the future data
head(future_data)

# Forecast electricity consumption
future_elec_preds <- predict(model_uk_elec, newdata = future_data)

# Forecast gas consumption
future_gas_preds <- predict(model_uk_gas, newdata = future_data)

# Combine the future predictions with the future data
future_data$elec_consumption_forecast <- future_elec_preds
future_data$gas_consumption_forecast <- future_gas_preds


ggplot(future_data, aes(x = year)) +
        geom_line(aes(y = elec_consumption_forecast, color = "Electricity")) +
        geom_line(aes(y = gas_consumption_forecast, color = "Gas")) +
        labs(title = "Forecasted Consumption to 2050", x = "Year", y = "MWh") +
        scale_color_manual(name = "Fuel Type", values = c("Electricity" = "blue", "Gas" = "red"))
