# Load the necessary package
library(readr)

# Load dataset and capture parsing issues
wine_data <- read.csv("data/wines_SPA.csv", colClasses = c(
  winery = "character",
  wine = "character",
  year = "character",
  rating = "numeric",
  num_reviews = "numeric",
  country = "character",
  region = "character",
  type = "factor",
  body = "factor",
  acidity = "factor",
  price = "numeric"
))

# Check for parsing issues
problems(wine_data)

# Display the structure of the dataset
str(wine_data)

# View the first few rows of the dataset
head(wine_data)

# View the dataset in a separate viewer window
View(wine_data)

# Frequency tables for categorical variables
winery_freq <- table(wine_data$winery)
type_freq <- table(wine_data$type)

# Display the frequency tables
print("Winery Frequency:")
print(winery_freq)

print("Type Frequency:")
print(type_freq)

# Mean, median, and mode for numeric variables
mean_rating <- mean(wine_data$rating, na.rm = TRUE)
median_rating <- median(wine_data$rating, na.rm = TRUE)
mode_rating <- as.numeric(names(sort(table(wine_data$rating), decreasing = TRUE)[1]))

mean_num_reviews <- mean(wine_data$num_reviews, na.rm = TRUE)
median_num_reviews <- median(wine_data$num_reviews, na.rm = TRUE)
mode_num_reviews <- as.numeric(names(sort(table(wine_data$num_reviews), decreasing = TRUE)[1]))

mean_price <- mean(wine_data$price, na.rm = TRUE)
median_price <- median(wine_data$price, na.rm = TRUE)
mode_price <- as.numeric(names(sort(table(wine_data$price), decreasing = TRUE)[1]))

# Display the measures of central tendency
print(paste("Mean Rating:", mean_rating))
print(paste("Median Rating:", median_rating))
print(paste("Mode Rating:", mode_rating))

print(paste("Mean Number of Reviews:", mean_num_reviews))
print(paste("Median Number of Reviews:", median_num_reviews))
print(paste("Mode Number of Reviews:", mode_num_reviews))

print(paste("Mean Price:", mean_price))
print(paste("Median Price:", median_price))
print(paste("Mode Price:", mode_price))

# Standard deviation, variance, range, and IQR for numeric variables
sd_rating <- sd(wine_data$rating, na.rm = TRUE)
var_rating <- var(wine_data$rating, na.rm = TRUE)
range_rating <- range(wine_data$rating, na.rm = TRUE)
iqr_rating <- IQR(wine_data$rating, na.rm = TRUE)

sd_num_reviews <- sd(wine_data$num_reviews, na.rm = TRUE)
var_num_reviews <- var(wine_data$num_reviews, na.rm = TRUE)
range_num_reviews <- range(wine_data$num_reviews, na.rm = TRUE)
iqr_num_reviews <- IQR(wine_data$num_reviews, na.rm = TRUE)

sd_price <- sd(wine_data$price, na.rm = TRUE)
var_price <- var(wine_data$price, na.rm = TRUE)
range_price <- range(wine_data$price, na.rm = TRUE)
iqr_price <- IQR(wine_data$price, na.rm = TRUE)

# Display the measures of distribution
print(paste("Standard Deviation of Rating:", sd_rating))
print(paste("Variance of Rating:", var_rating))
print(paste("Range of Rating:", paste(range_rating, collapse = " - ")))
print(paste("IQR of Rating:", iqr_rating))

print(paste("Standard Deviation of Number of Reviews:", sd_num_reviews))
print(paste("Variance of Number of Reviews:", var_num_reviews))
print(paste("Range of Number of Reviews:", paste(range_num_reviews, collapse = " - ")))
print(paste("IQR of Number of Reviews:", iqr_num_reviews))

print(paste("Standard Deviation of Price:", sd_price))
print(paste("Variance of Price:", var_price))
print(paste("Range of Price:", paste(range_price, collapse = " - ")))
print(paste("IQR of Price:", iqr_price))

# Correlation matrix for numeric variables
cor_matrix <- cor(wine_data[, c("rating", "num_reviews", "price")], use = "complete.obs")

# Display the correlation matrix
print("Correlation Matrix:")
print(cor_matrix)

# Load necessary packages
library(dplyr)
library(car)  # For Levene's Test
library(ggplot2)  # For visualizing residuals


# Convert year to numeric, handling "N.V." and other non-numeric values
wine_data$year <- ifelse(wine_data$year == "N.V.", NA, as.integer(wine_data$year))

# ANOVA: Effect of wine type on price
anova_type <- aov(price ~ type, data = wine_data)
summary(anova_type)

# ANOVA: Effect of region on price
anova_region <- aov(price ~ region, data = wine_data)
summary(anova_region)

# Check assumptions for ANOVA on type

# 1. Normality of residuals
par(mfrow = c(2, 2))
plot(anova_type)
shapiro.test(residuals(anova_type))

# 2. Homogeneity of variances
leveneTest(price ~ type, data = wine_data)

# Check assumptions for ANOVA on region

# 1. Normality of residuals
par(mfrow = c(2, 2))
plot(anova_region)
shapiro.test(residuals(anova_region))

# 2. Homogeneity of variances
leveneTest(price ~ region, data = wine_data)

# If significant, perform post-hoc test (Tukey HSD)
if (summary(anova_type)[[1]]$`Pr(>F)`[1] < 0.05) {
  post_hoc_type <- TukeyHSD(anova_type)
  print(post_hoc_type)
}

if (summary(anova_region)[[1]]$`Pr(>F)`[1] < 0.05) {
  post_hoc_region <- TukeyHSD(anova_region)
  print(post_hoc_region)
}

# Visualization of results
# Boxplot of price by type
ggplot(wine_data, aes(x = type, y = price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Price by Wine Type", x = "Wine Type", y = "Price")

# Boxplot of price by region
ggplot(wine_data, aes(x = region, y = price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Price by Region", x = "Region", y = "Price")


# Load necessary packages
library(ggplot2)
library(reshape2)


# Convert year to numeric, handling "N.V." and other non-numeric values
wine_data$year <- ifelse(wine_data$year == "N.V.", NA, as.integer(wine_data$year))

# Univariate Plots

# Histogram for price
ggplot(wine_data, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Histogram of Wine Prices", x = "Price", y = "Frequency")

# Boxplot for price
ggplot(wine_data, aes(y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Wine Prices", y = "Price")

# Bar plot for type
ggplot(wine_data, aes(x = type)) +
  geom_bar(fill = "orange") +
  labs(title = "Bar Plot of Wine Types", x = "Wine Type", y = "Count")

# Multivariate Plots

# Scatter plot for rating vs. price
ggplot(wine_data, aes(x = rating, y = price)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of Rating vs. Price", x = "Rating", y = "Price")

# Boxplot of price by type
ggplot(wine_data, aes(x = type, y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Price by Wine Type", x = "Wine Type", y = "Price")

# Boxplot of price by region
ggplot(wine_data, aes(x = region, y = price)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Boxplot of Price by Region", x = "Region", y = "Price") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Correlation matrix heatmap

# Compute correlation matrix
cor_matrix <- cor(wine_data[, c("rating", "num_reviews", "price")], use = "complete.obs")

# Melt the correlation matrix into long format
melted_cor_matrix <- melt(cor_matrix)

# Plot heatmap
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
  theme_minimal()

