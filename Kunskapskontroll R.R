# Load packages
library(pxweb)
library(readxl)
library(lmtest)
install.packages("car")
library(car)
library(ggplot2)

# Define the JSON query for SCB data
query_json <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "item",
        "values": ["01", "03", "05", "06", "08", "09", "12", "14", "20"]
      }
    },
    {
      "code": "Drivmedel",
      "selection": {
        "filter": "item",
        "values": ["100", "110", "120", "130"]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": ["TK1001AA"]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": ["2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01", "2024M02", "2024M03"]
      }
    }
  ],
  "response": {
    "format": "json"
  }
}'

# Download SCB data
px_data <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel",
                     query = query_json)

# Convert data to data frame
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Prepare SCB data
colnames(px_data_frame) <- gsub(" ", "_", colnames(px_data_frame))
px_data_frame$year <- substr(px_data_frame$månad, 1, 4)
sum_per_year <- aggregate(Nyregistrerade_personbilar ~ year + region + drivmedel, data = px_data_frame, FUN = sum)

# Prepare Blocket data
blocket_data <- read_excel("/Users/Bella/Desktop/EC/R/Blocket_data.xlsx")
blocket_data_df <- as.data.frame(blocket_data)
blocket_data_df <- blocket_data_df[, !names(blocket_data_df) %in% c("Datum_i_trafik")]
blocket_data_df$Modellår <- as.numeric(blocket_data_df$Modellår)
blocket_data_df$Miltal <- as.numeric(blocket_data_df$Miltal)
blocket_data_df$Hästkrafter <- as.numeric(blocket_data_df$Hästkrafter)
blocket_data_df$Pris <- as.numeric(blocket_data_df$Pris)
blocket_data_df$Märke <- as.factor(blocket_data_df$Märke)
blocket_data_df$Färg <- as.factor(blocket_data_df$Färg)
blocket_data_df$Biltyp <- as.factor(blocket_data_df$Biltyp)
blocket_data_df$Län <- as.factor(blocket_data_df$Län)
blocket_data_df$Stad <- as.factor(blocket_data_df$Stad)
blocket_data_df$Drivning <- as.factor(blocket_data_df$Drivning)
median_hästkrafter <- median(blocket_data_df$Hästkrafter, na.rm = TRUE)
blocket_data_df$Hästkrafter[is.na(blocket_data_df$Hästkrafter)] <- median_hästkrafter

# Split Blocket data into training and test sets
set.seed(123)
train_index <- sample(1:nrow(blocket_data_df), 0.7 * nrow(blocket_data_df))
train_data <- blocket_data_df[train_index, ]
test_data <- blocket_data_df[-train_index, ]

# Perform regression analysis for Blocket data
model_blocket <- lm(Pris ~ Modellår + Miltal + Märke + Bränsle + Län + Växellåda + Biltyp + Drivning + Hästkrafter + Färg, data = train_data)
summary(model_blocket)

# Plot model
plot(model_blocket)

# Convert coefficients to readable format and add stars for significance level
coefficients_summary <- summary(model_blocket)$coefficients
coefficients_summary[, "Estimate"] <- format(coefficients_summary[, "Estimate"], scientific = FALSE, digits = 3)
stars <- ifelse(coefficients_summary[, "Pr(>|t|)"] < 0.05, "*", "")
coefficients_summary <- cbind(coefficients_summary, stars)

# Print the updated summary
print(coefficients_summary)

# Prepare test data subset
test_data_subset <- test_data[, intersect(names(model_blocket$model), names(test_data))]

# Check for missing columns
missing_columns <- setdiff(names(model_blocket$model), names(test_data_subset))
print(missing_columns)

# Ensure proper encoding of categorical variables in train_data
train_data <- within(train_data, {
  Märke <- as.factor(Märke)
  Bränsle <- as.factor(Bränsle)
  Län <- as.factor(Län)
  Växellåda <- as.factor(Växellåda)
  Biltyp <- as.factor(Biltyp)
  Drivning <- as.factor(Drivning)
  Färg <- as.factor(Färg)
})

# Plot residuals vs. fitted values
plot(residuals(model_blocket) ~ fitted(model_blocket), xlab = "Fitted values", ylab = "Residuals")

# Check for missing values in train_data
missing_rows <- which(rowSums(is.na(train_data)) > 0)
missing_data <- train_data[missing_rows, ]
print(missing_data)

# Remove missing values from train_data
train_data <- train_data[complete.cases(train_data), ]

# Make predictions on test data
predictions <- predict(model_blocket, newdata = test_data_subset)
test_rmse <- sqrt(mean((test_data$Pris - predictions)^2))
cat("Root Mean Squared Error (RMSE) on test data:", test_rmse, "\n")

# check if 'Modellår' exhibits non-linearity:
train_data$Modellår_squared <- train_data$Modellår^2
# Include 'Modellår_squared' in the model and re-run regression
model_blocket <- lm(Pris ~ Modellår + Modellår_squared + Miltal + Märke + Bränsle + Län + Växellåda + Biltyp + Drivning + Hästkrafter + Färg, data = train_data)
summary(model_blocket)

test_data$Modellår_squared <- test_data$Modellår^2

# Prepare test data subset
test_data_subset <- test_data[, intersect(names(model_blocket$model), names(test_data))]

# Make predictions on test data
predictions <- predict(model_blocket, newdata = test_data_subset)

# Calculate RMSE on test data
test_rmse <- sqrt(mean((test_data$Pris - predictions)^2))
cat("Root Mean Squared Error (RMSE) on test data:", test_rmse, "\n")

outliers <- which(cooks.distance(model_blocket) > 4/length(cooks.distance(model_blocket)))
print(outliers)

cooksd_blocket <- cooks.distance(model_blocket)

# Plot Cook's distance
plot(cooksd_blocket, pch = 20, main = "Cook's distance plot (Blocket data)", xlab = "Index", ylab = "Cook's Distance")
abline(h = 4/length(cooksd_blocket), col = "red", lty = 2)

shapiro.test(residuals(model_blocket))

electric_cars_blocket <- blocket_data_df[blocket_data_df$Bränsle == "El", ]
plot(electric_cars_blocket$Modellår, electric_cars_blocket$Pris, type = "l", xlab = "Year", ylab = "Price", main = "Price Development of Electric Cars Over Time")

future_year <- 2025 
future_data <- data.frame(Modellår = 2023, Miltal = 8000, Märke = "Volvo", Bränsle = "El", Län = "Stockholm", Växellåda = "Automat", Biltyp = "Halvkombi", Drivning = "Tvåhjulsdriven", Hästkrafter = 500, Färg = "Svart", Year = future_year)
future_data$Modellår_squared <- future_data$Modellår^2
future_predictions <- predict(model_blocket, newdata = future_data)
print(future_predictions)

future_year_26 <- 2026 
future_data_26 <- data.frame(Modellår = 2024, Miltal = 8000, Märke = "Volvo", Bränsle = "El", Län = "Stockholm", Växellåda = "Automat", Biltyp = "Halvkombi", Drivning = "Tvåhjulsdriven", Hästkrafter = 500, Färg = "Svart", Year = future_year_26)
future_data_26$Modellår_squared <- future_data_26$Modellår^2
future_predictions_26 <- predict(model_blocket, newdata = future_data_26)
print(future_predictions_26)

# Perform regression analysis for SCB data
model_scb <- lm(Nyregistrerade_personbilar ~ drivmedel + region + year, data = px_data_frame)
summary(model_scb)

# Check for multicollinearity in the SCB model
vif_values_scb <- vif(model_scb)
print(vif_values_scb)

# Plot Cook's distance to identify influential observations
plot(cooks.distance(model_scb), pch = 20, main = "Cook's distance plot")
abline(h = 4/length(cooks.distance(model_scb)), col = "red")

# Identify outliers using Cook's distance
outliers_scb <- which(cooks.distance(model_scb) > 4/length(cooks.distance(model_scb)))
print(outliers_scb)

# Diagnostic plots for SCB model
plot(model_scb)

# Check for heteroscedasticity and normality of residuals in SCB model
bptest(model_scb)
shapiro.test(residuals(model_scb))

# Filter Blocket data for cars between modellår 2019-2024
filtered_blocket_data <- blocket_data_df[blocket_data_df$Modellår >= 2019 & blocket_data_df$Modellår <= 2024, ]

# Calculate average price for diesel, bensin, and el cars
average_price_diesel <- mean(filtered_blocket_data$Pris[filtered_blocket_data$Bränsle == "Diesel"])
average_price_bensin <- mean(filtered_blocket_data$Pris[filtered_blocket_data$Bränsle == "Bensin"])
average_price_el <- mean(filtered_blocket_data$Pris[filtered_blocket_data$Bränsle == "El"])

# Create a data frame for average prices
average_prices <- data.frame(Fuel_Type = c("Diesel", "Bensin", "El"),
                             Average_Price = c(average_price_diesel, average_price_bensin, average_price_el))

print(average_prices)

# Visualization of average prices for diesel, bensin, and el cars
ggplot(average_prices, aes(x = Fuel_Type, y = Average_Price, fill = Fuel_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Prices for Diesel, Bensin, and El Cars (2019-2024)",
       x = "Fuel Type",
       y = "Average Price (SEK)") +
  theme_minimal()



# Filter SCB data for electric cars
electric_cars <- px_data_frame[px_data_frame$drivmedel %in% c("el", "elhybrid"), ]
electric_cars_year <- aggregate(Nyregistrerade_personbilar ~ year, data = electric_cars, FUN = sum)

options(scipen = 10)
# Create a bar plot
barplot(electric_cars_year$Nyregistrerade_personbilar, 
        names.arg = electric_cars_year$year, 
        xlab = "Year",
        ylab = "Number of Electric Cars",
        main = "Number of Electric Cars Over the Years",
        col = "blue",
        border = "black")

# Filter SCB data for bensin/diesel cars
bensin_diesel_cars <- px_data_frame[px_data_frame$drivmedel %in% c("bensin", "diesel"), ]
bensin_diesel_cars_year <- aggregate(Nyregistrerade_personbilar ~ year, data = bensin_diesel_cars, FUN = sum)

options(scipen = 10)
# Create a bar plot
barplot(bensin_diesel_cars_year$Nyregistrerade_personbilar, 
        names.arg = bensin_diesel_cars_year$year, 
        xlab = "Year",
        ylab = "Number of Bensin & Diesel Cars",
        main = "Number of Bensin & Diesel Over the Years",
        col = "grey",
        border = "black")

# Calculate the total number of cars (of all types) for each region
total_cars_region <- aggregate(Nyregistrerade_personbilar ~ region, data = px_data_frame, FUN = sum)

# Calculate the proportion of electric cars in each region
electric_cars_region <- aggregate(Nyregistrerade_personbilar ~ region, data = px_data_frame[px_data_frame$drivmedel %in% c("el", "elhybrid"), ], FUN = sum)
electric_cars_region$Proportion <- electric_cars_region$Nyregistrerade_personbilar / total_cars_region$Nyregistrerade_personbilar

# Identify regions with highest proportion of electric cars
top_regions <- electric_cars_region[order(-electric_cars_region$Proportion), ]

# Plotting the result
ggplot(top_regions, aes(x = region, y = Proportion)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Regions with Highest Proportion of Electric Cars",
       x = "Region",
       y = "Proportion of Electric Cars") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


total_fuel_count <- aggregate(px_data_frame$Nyregistrerade_personbilar, 
                              by = list(Fuel = px_data_frame$drivmedel),
                              FUN = sum)

# Calculate the total number of registered cars
total_registered_cars <- sum(total_fuel_count$x)

# Calculate the proportion of each fuel type
total_fuel_count$Proportion <- total_fuel_count$x / total_registered_cars

# Sort the dataframe by proportion
total_fuel_count <- total_fuel_count[order(-total_fuel_count$Proportion), ]

# Calculate the percentage for each slice
total_fuel_count$Percentage <- paste(round(total_fuel_count$Proportion * 100, 1), "%", sep = "")

# Plot a pie chart
pie_chart_total <- ggplot(total_fuel_count, aes(x = "", y = Proportion, fill = Fuel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Registered Cars by Fuel Type (2014-2024)",
       x = NULL, y = NULL,
       fill = "Fuel Type") +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5))

# Display the pie chart
print(pie_chart_total)

# Filter the dataframe to include only data from 2022
fuel_data_2022 <- px_data_frame[px_data_frame$year == "2022", ]

# Aggregate the total count of each type of fuel for 2022
total_fuel_count_2022 <- aggregate(fuel_data_2022$Nyregistrerade_personbilar, 
                                   by = list(Fuel = fuel_data_2022$drivmedel),
                                   FUN = sum)

# Calculate the total number of registered cars in 2022
total_registered_cars_2022 <- sum(total_fuel_count_2022$x)

# Calculate the proportion of each fuel type in 2022
total_fuel_count_2022$Proportion <- total_fuel_count_2022$x / total_registered_cars_2022

# Sort the dataframe by proportion
total_fuel_count_2022 <- total_fuel_count_2022[order(-total_fuel_count_2022$Proportion), ]

# Calculate the percentage for each slice
total_fuel_count_2022$Percentage <- paste(round(total_fuel_count_2022$Proportion * 100, 1), "%", sep = "")

# Plot a pie chart for 2022
pie_chart_2022 <- ggplot(total_fuel_count_2022, aes(x = "", y = Proportion, fill = Fuel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Registered Cars by Fuel Type in 2022",
       x = NULL, y = NULL,
       fill = "Fuel Type") +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5))

# Display the pie chart for 2022
print(pie_chart_2022)

# Filter the dataframe to include only data from 2023
fuel_data_2023 <- px_data_frame[px_data_frame$year == "2023", ]

# Aggregate the total count of each type of fuel for 2023
total_fuel_count_2023 <- aggregate(fuel_data_2023$Nyregistrerade_personbilar, 
                                   by = list(Fuel = fuel_data_2023$drivmedel),
                                   FUN = sum)

# Calculate the total number of registered cars in 2023
total_registered_cars_2023 <- sum(total_fuel_count_2023$x)

# Calculate the proportion of each fuel type in 2023
total_fuel_count_2023$Proportion <- total_fuel_count_2023$x / total_registered_cars_2023

# Sort the dataframe by proportion
total_fuel_count_2023 <- total_fuel_count_2023[order(-total_fuel_count_2023$Proportion), ]

# Calculate the percentage for each slice
total_fuel_count_2023$Percentage <- paste(round(total_fuel_count_2023$Proportion * 100, 1), "%", sep = "")

# Plot a pie chart for 2023
pie_chart_2023 <- ggplot(total_fuel_count_2023, aes(x = "", y = Proportion, fill = Fuel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Registered Cars by Fuel Type in 2023",
       x = NULL, y = NULL,
       fill = "Fuel Type") +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5))

# Display the pie chart for 2023
print(pie_chart_2023)

# Calculate the cost per kilometer for electric vehicles
electric_cost_per_kilometer <- (16.8 * 5) / 100
# Calculate the cost per kilometer for diesel cars
diesel_cost_per_kilometer <- (0.42 * 18.44)
# Calculate the cost per kilometer for gasoline cars
gasoline_cost_per_kilometer <- (0.41 * 19.49)

# Create a data frame for cost savings
cost_savings <- data.frame(Fuel_Type = c("Electric", "Diesel", "Gasoline"),
                           Average_Cost_per_Kilometer = c(electric_cost_per_kilometer,
                                                          diesel_cost_per_kilometer,
                                                          gasoline_cost_per_kilometer))

print(cost_savings)

# Visualization of cost for electric vehicles compared to gasoline and diesel cars
ggplot(cost_savings, aes(x = Fuel_Type, y = Average_Cost_per_Kilometer, fill = Fuel_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cost for Electric Vehicles vs. Gasoline and Diesel Cars",
       x = "Fuel Type",
       y = "Average Cost per Kilometer (SEK)") +
  theme_minimal() 

