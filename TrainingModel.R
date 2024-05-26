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
library(caTools)

# Ensure reproducibility
set.seed(123)

# Create a partition index
split <- sample.split(wine_data_clean$price, SplitRatio = 0.8)

# Split the data into training and testing sets
train_data <- subset(wine_data_clean, split == TRUE)
test_data <- subset(wine_data_clean, split == FALSE)

# Display the structure of the training and testing sets
dim(test_data)
dim(train_data)

# Load necessary packages
library(boot)

# Ensure reproducibility
set.seed(123)

# Function to calculate the statistic of interest
bootstrap_stat <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample with replacement
  return(mean(sample_data$price)) # Calculate the mean of the price variable
}

# Perform bootstrapping
bootstrap_results <- boot(data = wine_data_clean, statistic = bootstrap_stat, R = 1000)

# Summary of bootstrap results
print(bootstrap_results)

# Plot the bootstrap distribution
plot(bootstrap_results)

