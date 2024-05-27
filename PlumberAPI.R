# Load necessary libraries
library(plumber)

# Load the saved decision tree model
loaded_dt_model <- readRDS("./models/dt_model.rds")

#* @apiTitle Wine Type Prediction API
#* @apiDescription Predict the type of wine using a decision tree model.

#* @post /predict_wine_type
#* @param winery character: Name of the winery
#* @param wine character: Name of the wine
#* @param rating numeric: Wine rating
#* @param num_reviews numeric: Number of reviews
#* @param region character: Wine region
#* @param body factor: Wine body (1-5 scale)
#* @param acidity factor: Wine acidity (1-3 scale)
#* @param price numeric: Wine price
#* @serializer unboxedJSON
predict_wine_type <- function(winery, wine, rating, num_reviews, region, body, acidity, price) {
  # Prepare new data for prediction
  new_data <- data.frame(
    winery = as.character(winery),
    wine = as.character(wine),
    rating = as.numeric(rating),
    num_reviews = as.numeric(num_reviews),
    region = as.character(region),
    body = as.factor(body),
    acidity = as.factor(acidity),
    price = as.numeric(price)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_dt_model, newdata = new_data)
  
  # Return the prediction
  return(as.character(prediction))
}
