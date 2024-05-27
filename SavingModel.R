# Create a directory named "models" if it doesn't exist
if (!file.exists("./models")) {
  dir.create("./models")
}

# Save the decision tree model to a file
saveRDS(dt_model, file = "./models/dt_model.rds")

# Load the saved decision tree model
loaded_dt_model <- readRDS("./models/dt_model.rds")

# Prepare new data for prediction (example data, adjust according to your dataset)
new_data <- data.frame(
  winery = factor("Teso La Monja", levels = levels(train_data$winery)),
  wine = factor("Tinto", levels = levels(train_data$wine)),
  rating = 4.8,
  num_reviews = 100,
  region = factor("Toro", levels = levels(train_data$region)),
  body = factor(5, levels = levels(train_data$body)),
  acidity = factor(3, levels = levels(train_data$acidity)),
  price = 50.0
)

# Use the loaded model to make predictions for new data
predictions_loaded_model <- predict(loaded_dt_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
