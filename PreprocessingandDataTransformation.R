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

# Load necessary packages
library(VIM)
library(naniar)

# Summary of missing values
summary(wine_data)

# Count of missing values in each column
sapply(wine_data, function(x) sum(is.na(x)))

# Percentage of missing values in each column
sapply(wine_data, function(x) mean(is.na(x)) * 100)

# Total number of missing values
sum(is.na(wine_data))

# Rows with missing values
missing_rows <- wine_data[!complete.cases(wine_data), ]
head(missing_rows)

# Visualization of missing values

# Using VIM package
aggr_plot <- aggr(wine_data, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, labels = names(wine_data), cex.axis = 0.7, gap = 3, ylab = c("Missing data", "Pattern"))

# Using naniar package
gg_miss_var(wine_data, show_pct = TRUE) +
  labs(title = "Missing Values by Variable")

# Load necessary packages
library(dplyr)
library(tidyr)

# Remove rows with missing values
wine_data_clean <- wine_data %>%
  drop_na()

# Display the structure of the cleaned dataset
str(wine_data_clean)

# View the first few rows of the cleaned dataset
head(wine_data_clean)

# Total number of missing values
sum(is.na(wine_data_clean))

# Load necessary libraries
library(dplyr)

# Drop the 'year' and 'country' columns from the dataset
wine_data_clean <- wine_data_clean %>% select(-year, -country)

# Verify that the columns have been removed
str(wine_data_clean)
