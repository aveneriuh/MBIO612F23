###Create regression###
whales <- read.csv("Lewis_Raw_Sheet.csv")
model <- lm(Volume.m3 ~ Total.length.m, data = whales)
model
whale_plot <- plot(whales$Total.length.m, whales$Volume.m3, main = "Whale Length vs. Volume", 
     xlab = "Whale Length (m)", ylab = "Whale Body Volume (m^3)")
summary_result <- summary(model)
summary_result
r_squared <- summary_result$r.squared
r_squared

# Plot with points of different colors based on Region
colors <- ifelse(whales$Region == "HI", "blue", "black")
whale_plot <- plot(
  whales$Total.length.m, whales$Volume.m3,
  main = "Whale Length vs. Volume",
  xlab = "Whale Length (m)", ylab = "Whale Body Volume (m^3)",
  col = colors, pch = 16, cex = 0.5
)
whale_plot
smoothed_data <- lowess(whales$Total.length.m, whales$Volume.m3)
lines(smoothed_data, col = "red", lwd = 2)
legend("topleft", legend = c("Hawaii", "Alaska"), col = c("blue", "black"), pch = 16, cex = 1, title = "Region")

###Transform Data###
whales$log_length <- log(whales$Total.length.m)
whales$log_volume <- log(whales$Volume.m3)
log_regression <- lm(log_volume ~ log_length, data = whales)
plot(whales$log_length, whales$log_volume, main = "Log-Transformed Whale Length vs. Volume", 
     xlab = "Log(Whale Length (m))", ylab = "Log(Whale Body Volume(m^3))")
abline(log_regression, col = "red")
summary_result_log <- summary(log_regression)
summary_result_log
r_squared_log <- summary_result_log$r.squared
r_squared_log

# Plot with points of different colors based on Region
colors <- ifelse(whales$Region == "HI", "blue", "black")
whale_plot <- plot(
  whales$log_length, whales$log_volume,
  main = "Log-Transformed Whale Length vs. Volume",
  xlab = "Log(Whale Length (m))", ylab = "Log(Whale Body Volume (m^3))",
  col = colors, pch = 16, cex = 0.5
)
whale_plot
legend("topleft", legend = c("Hawaii", "Alaska"), col = c("blue", "black"), pch = 16, cex = 1, title = "Region")
abline(log_regression, col = "red")
summary_result_log <- summary(log_regression)
summary_result_log
r_squared_log <- summary_result_log$r.squared
r_squared_log

###Calculating residuals###
residuals_vector <- residuals(log_regression)
residuals_vector
residual_variance <- var(residuals_vector)
residual_variance
whales$BCI <- residuals_vector
head(whales)

###Create month column###
library(dplyr)
whales <- whales %>%
  mutate(Date = as.Date(paste(Year, DOY, sep = "-"), format = "%Y-%j"))
whales <- whales %>%
  mutate(Month = format(Date, "%m"))
avg_bv_per_month <- whales %>%
  group_by(Year, Month, Rep.class) %>%
  summarise(Avg_BV = mean(Volume.m3))
print(avg_bv_per_month)


###Create a facet grid###
library(ggplot2)
whale_grid <- ggplot(whales, aes(x = Rep.class, y = BCI, fill = "Year")) +
  geom_boxplot() +
  facet_wrap(~Year, scales = "free_y") +
  labs(title = "Life stage and residual",
       x = "Life Stage",
       y = "BCI") +
  theme_minimal()
whale_grid

###Facet grid with month###
#omit yearling#
whales_filtered1 <- whales %>%
  filter(Rep.class != "Yearling")
ggplot(whales_filtered1, aes(x = Month, y = BCI, fill = Rep.class)) +
  geom_boxplot() +
  facet_grid(~Year) +
  labs(x = "Month", y = "BCI", fill = "Rep.class") +
  ggtitle("Box Plot Facet Grid for BCI by Month and Class for Each Year")

###Facet grid with month 1,2, and 3###
whales_subset <- whales %>% filter(Month %in% c("01", "02", "03"))
whales_filtered <- whales_subset %>%
  filter(Rep.class != "Yearling")
ggplot(whales_filtered, aes(x = as.factor(Month), y = BCI, fill = Rep.class)) +
  geom_boxplot() +
  facet_grid(~Year) +
  labs(x = "Month", y = "BCI", fill = "Rep.class") +
  ggtitle("Box Plot Facet Grid for BCI by Month and Class for Each Year")

###Perform a t-test to compare BCI of calf and mother###
t_test_result <- t.test(BCI ~ Rep.class, data = whales_filtered1)
print(t_test_result)

#T-test for Hawaii
t_test_hawaii <- t.test(BCI ~ Rep.class, data = whales_filtered1[whales_filtered1$Region == "HI", ])
print("T-Test for Hawaii:")
print(t_test_hawaii)

#T-test for Alaska
t_test_alaska <- t.test(BCI ~ Rep.class, data = whales_filtered1[whales_filtered1$Region == "AK", ])
print("T-Test for Alaska:")
print(t_test_alaska)

###Creating a time series plot###
rm(list=ls())
##TIME SERIES
library(dplyr)

##First, find the average BV of calves and mothers per month
#convert year and DOY to date format
###make avg per month
whales <- whales %>%
  mutate(Date = as.Date(paste(Year, DOY, sep = "-"), format = "%Y-%j"))

# Extract month from the date
whales <- whales %>%
  mutate(Month = format(Date, "%m"))

# Group by Year, Month, and class, then calculate the average body volume
avg_bv_per_month <- whales %>%
  group_by(Year, Month, Rep.class) %>%
  summarise(Avg_BV = mean(BV))

# View the resulting dataset with average body volume per month for each year and Rep.class
print(avg_bv_per_month)


##NEXT, prepare data by excluding yearling class to focus on mothers and calves,
#then group by year, month and class to calculate avg body volume for each year

# Exclude 'yearling' class
whales_filtered <- whales %>%
  filter(Rep.class != "Yearling")

# Group by Year, Month, and Rep.class, then calculate the average BCI
avg_BCI_per_month <- whales_filtered %>%
  group_by(Year, Month, Rep.class) %>%
  summarise(Avg_BCI = mean(BCI))

# Plotting for each year
for (year in 2019:2022) {
  # Subset data for the current year
  year_data <- avg_BCI_per_month %>%
    filter(Year == year) %>%
    filter(Rep.class != "Yearling")  # Filter out 'yearling' within each year
  
  # Plotting average body volume over months for each Rep.class
  ggplot(year_data, aes(x = as.numeric(Month), y = Avg_BCI, group = Rep.class, color = Rep.class)) +
    geom_line() +
    labs(title = paste("Average Body Volume Over Months for Whales in", year),
         x = "Month", y = "Average BCI") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # You can save the plot here or use any preferred method to display it
  # ggsave(filename = paste("Avg_BV_over_months_", year, ".png"), plot = last_plot())
}
print(last_plot())

# Convert 'Year' and 'Month' to numeric for plotting
avg_BCI_per_month <- avg_BCI_per_month %>%
  mutate(Year = as.numeric(Year),
         Month = as.numeric(Month))

# Filter data for year 2019
data_2019 <- avg_BCI_per_month %>%
  filter(Year == 2019)

# Plotting for 2019
ggplot(data_2019, aes(x = Month, y = Avg_BCI, group = Rep.class, color = Rep.class)) +
  geom_line() +
  labs(title = "Average BCI Over Months for Whales in 2019",
       x = "Month", y = "Average BCI") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Filter data for year 2020
data_2020 <- avg_BCI_per_month %>%
  filter(Year == 2020)

# Plotting for 2020
ggplot(data_2020, aes(x = Month, y = Avg_BCI, group = Rep.class, color = Rep.class)) +
  geom_line() +
  labs(title = "Average BCI Over Months for Whales in 2020",
       x = "Month", y = "Average BCI") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Filter data for year 2021
data_2021 <- avg_BCI_per_month %>%
  filter(Year == 2021)

# Plotting for 2021
ggplot(data_2021, aes(x = Month, y = Avg_BCI, group = Rep.class, color = Rep.class)) +
  geom_line() +
  labs(title = "Average BCI Over Months for Whales in 2021",
       x = "Month", y = "Average BCI") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Filter data for year 2022

data_2022 <- avg_BCI_per_month %>%
  filter(Year == 2022)

# Plotting for 2022
ggplot(data_2022, aes(x = Month, y = Avg_BCI, group = Rep.class, color = Rep.class)) +
  geom_line() +
  labs(title = "Average BCI Over Months for Whales in 2019",
       x = "Month", y = "Average BCI") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal() +
  theme(legend.position = "bottom")




# Filter data for years 2019 to 2022
data_years <- avg_BCI_per_month %>%
  filter(Year %in% c(2019, 2020, 2021, 2022))

# Plotting for all years in one panel
ggplot(data_years, aes(x = Month, y = Avg_BCI, group = Rep.class, color = Rep.class)) +
  geom_line() +
  labs(title = "Average  Over BCI for Whales (2019-2022)",
       x = "Month", y = "Average BCI") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  facet_wrap(~Year, scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom")
