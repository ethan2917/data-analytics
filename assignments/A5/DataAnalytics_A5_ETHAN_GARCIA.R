# ============================================================
# Assignment 5: Data Analytics (Spring 2025)
# R Script for NYC Citywide Annualized Calendar Sales Update
# ============================================================

# Load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(class)

# Define a normalization function
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }

# ---------------------------
# 1. Read the dataset
# ---------------------------
data <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20241107.csv", 
                 stringsAsFactors = FALSE)

# Check structure of the BOROUGH column
str(data$BOROUGH)

# ---------------------------
# 2. Create a derived dataset for borough 4 (Queens)
# ---------------------------
# Convert BOROUGH properly and filter for Queens (code 4)
queens_data <- data %>% filter(as.numeric(as.character(BOROUGH)) == 4)
cat("Number of rows in Queens dataset:", nrow(queens_data), "\n")

# Clean SALE.PRICE:
cat("Original SALE.PRICE values:", head(queens_data$SALE.PRICE), "\n")
queens_data$SALE.PRICE <- as.numeric(gsub("[^0-9]", "", queens_data$SALE.PRICE))
cat("Cleaned SALE.PRICE values:", head(queens_data$SALE.PRICE), "\n")

# Ensure at least one non-NA SALE.PRICE exists
if(all(is.na(queens_data$SALE.PRICE))){
  stop("All SALE.PRICE values are NA. Please check the format in your CSV.")
}

# Clean GROSS.SQUARE.FEET if present
if("GROSS.SQUARE.FEET" %in% names(queens_data)){
  queens_data$GROSS.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", queens_data$GROSS.SQUARE.FEET))
}

# Print summary to check data quality
print(summary(queens_data))

# ---------------------------
# 2.c Regression Analysis to Predict SALE.PRICE
# ---------------------------
if(nrow(queens_data) > 0) {
  # Instead of using all numeric predictors (which yields 0 complete cases),
  # we select a subset of predictors that are likely more complete.
  # Adjust the following vector if you wish to include other variables.
  selected_vars <- c("SALE.PRICE", "GROSS.SQUARE.FEET", "YEAR.BUILT", 
                     "RESIDENTIAL.UNITS", "COMMERCIAL.UNITS", "TOTAL.UNITS")
  
  # Ensure the selected columns exist in the data
  selected_vars <- selected_vars[selected_vars %in% names(queens_data)]
  
  model_data <- queens_data[, selected_vars]
  
  # Print missing percentages for each selected variable
  missing_rate <- sapply(model_data, function(x) mean(is.na(x)))
  cat("Missing percentages for selected predictors:\n")
  print(missing_rate)
  
  # Drop rows with any missing values
  model_data <- na.omit(model_data)
  cat("Number of complete cases for regression using selected predictors:", nrow(model_data), "\n")
  
  if(nrow(model_data) == 0){
    stop("No complete cases available for regression using selected predictors!")
  }
  
  # Fit the regression model using the complete cases
  lm_model <- lm(SALE.PRICE ~ ., data = model_data)
  print(summary(lm_model))
} else {
  stop("No data available in the Queens dataset after filtering.")
}

# ---------------------------
# Test the regression model on a subset based on NEIGHBORHOOD.
# ---------------------------
if("NEIGHBORHOOD" %in% names(queens_data)){
  test_neighborhood <- unique(queens_data$NEIGHBORHOOD)[1]
  test_data <- queens_data %>% filter(NEIGHBORHOOD == test_neighborhood)
  
  # Remove rows with missing values from the test subset for prediction
  test_data <- na.omit(test_data)
  
  if(nrow(test_data) > 0){
    test_data$predicted_price <- predict(lm_model, newdata = test_data)
    test_data$residuals <- test_data$SALE.PRICE - test_data$predicted_price
    
    # Plot Actual vs. Predicted SALE.PRICE.
    ggplot(test_data, aes(x = SALE.PRICE, y = predicted_price)) +
      geom_point(color = "darkgreen") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title = paste("Actual vs Predicted Sale Price for", test_neighborhood, "in Queens"),
           x = "Actual Sale Price", y = "Predicted Sale Price")
    
    # Plot Residuals.
    ggplot(test_data, aes(x = predicted_price, y = residuals)) +
      geom_point(color = "purple") +
      labs(title = paste("Residual Plot for", test_neighborhood, "in Queens"),
           x = "Predicted Sale Price", y = "Residuals")
  } else {
    cat("No complete cases available in test subset for", test_neighborhood, "\n")
  }
}

# ---------------------------
# 2.d Classification Models to Predict NEIGHBORHOOD
# ---------------------------
if(all(c("NEIGHBORHOOD", "SALE.PRICE", "GROSS.SQUARE.FEET") %in% names(queens_data))){
  classification_data <- queens_data %>% select(NEIGHBORHOOD, SALE.PRICE, GROSS.SQUARE.FEET)
  classification_data <- na.omit(classification_data)
  
  # Filter out very rare neighborhoods (e.g., those with fewer than 3 observations)
  class_counts <- table(classification_data$NEIGHBORHOOD)
  common_neighborhoods <- names(class_counts)[class_counts >= 3]
  classification_data <- classification_data %>% filter(NEIGHBORHOOD %in% common_neighborhoods)
  
  # Convert NEIGHBORHOOD to a factor so that levels are consistent
  classification_data$NEIGHBORHOOD <- as.factor(classification_data$NEIGHBORHOOD)
  
  set.seed(123)
  train_index <- createDataPartition(classification_data$NEIGHBORHOOD, p = 0.7, list = FALSE)
  train_class <- classification_data[train_index, ]
  test_class <- classification_data[-train_index, ]
  
  # ----- Naïve Bayes Model -----
  nb_model <- naiveBayes(NEIGHBORHOOD ~ ., data = train_class)
  nb_predictions <- predict(nb_model, test_class)
  nb_predictions <- factor(nb_predictions, levels = levels(test_class$NEIGHBORHOOD))
  cat("Naïve Bayes Confusion Matrix for Queens:\n")
  print(confusionMatrix(nb_predictions, test_class$NEIGHBORHOOD))
  
  # ----- k-NN Classification with Jitter to Break Ties -----
  train_knn <- train_class
  test_knn <- test_class
  train_knn$SALE.PRICE <- normalize(train_knn$SALE.PRICE)
  train_knn$GROSS.SQUARE.FEET <- normalize(train_knn$GROSS.SQUARE.FEET)
  test_knn$SALE.PRICE <- normalize(test_knn$SALE.PRICE)
  test_knn$GROSS.SQUARE.FEET <- normalize(test_knn$GROSS.SQUARE.FEET)
  
  # Add a small jitter to break ties
  train_knn$SALE.PRICE <- jitter(train_knn$SALE.PRICE, amount = 1e-6)
  train_knn$GROSS.SQUARE.FEET <- jitter(train_knn$GROSS.SQUARE.FEET, amount = 1e-6)
  test_knn$SALE.PRICE <- jitter(test_knn$SALE.PRICE, amount = 1e-6)
  test_knn$GROSS.SQUARE.FEET <- jitter(test_knn$GROSS.SQUARE.FEET, amount = 1e-6)
  
  knn_predictions <- knn(train = train_knn[, c("SALE.PRICE", "GROSS.SQUARE.FEET")],
                         test = test_knn[, c("SALE.PRICE", "GROSS.SQUARE.FEET")],
                         cl = train_knn$NEIGHBORHOOD,
                         k = 5)
  knn_predictions <- factor(knn_predictions, levels = levels(test_knn$NEIGHBORHOOD))
  cat("k-NN Confusion Matrix for Queens:\n")
  print(confusionMatrix(knn_predictions, test_knn$NEIGHBORHOOD))
  
  # ----- Random Forest Model -----
  rf_model <- randomForest(NEIGHBORHOOD ~ ., data = train_class)
  rf_predictions <- predict(rf_model, test_class)
  rf_predictions <- factor(rf_predictions, levels = levels(test_class$NEIGHBORHOOD))
  cat("Random Forest Confusion Matrix for Queens:\n")
  print(confusionMatrix(rf_predictions, test_class$NEIGHBORHOOD))
}

# ---------------------------
# 3. Create a derived dataset for a different borough
# ---------------------------
# Create a new numeric column for BOROUGH, suppressing warnings from coercion
data$BOROUGH_NUM <- suppressWarnings(as.numeric(as.character(data$BOROUGH)))

# Now filter for borough 3 (assumed to be Brooklyn)
brooklyn_data <- data %>% filter(BOROUGH_NUM == 3)
brooklyn_data$SALE.PRICE <- as.numeric(gsub("[^0-9]", "", brooklyn_data$SALE.PRICE))
if("GROSS.SQUARE.FEET" %in% names(brooklyn_data)){
  brooklyn_data$GROSS.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", brooklyn_data$GROSS.SQUARE.FEET))
}

# Remove rows with missing values in Brooklyn dataset for regression prediction
brooklyn_data <- na.omit(brooklyn_data)

# ---------------------------
# 3.a Apply the Regression Model from Queens to the Brooklyn dataset.
# ---------------------------
brooklyn_data$predicted_price <- predict(lm_model, newdata = brooklyn_data)

ggplot(brooklyn_data, aes(x = SALE.PRICE, y = predicted_price)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Sale Price (Brooklyn using Queens model)",
       x = "Actual Sale Price", y = "Predicted Sale Price")

brooklyn_data$residuals <- brooklyn_data$SALE.PRICE - brooklyn_data$predicted_price
ggplot(brooklyn_data, aes(x = predicted_price, y = residuals)) +
  geom_point(color = "red") +
  labs(title = "Residual Plot for Brooklyn",
       x = "Predicted Sale Price", y = "Residuals")

# ---------------------------
# 3.b Apply the Classification Models from Queens to the Brooklyn dataset.
# ---------------------------
if(all(c("NEIGHBORHOOD", "SALE.PRICE", "GROSS.SQUARE.FEET") %in% names(brooklyn_data))){
  classification_brooklyn <- brooklyn_data %>% select(NEIGHBORHOOD, SALE.PRICE, GROSS.SQUARE.FEET)
  classification_brooklyn <- na.omit(classification_brooklyn)
  
  if(nrow(classification_brooklyn) == 0){
    cat("No complete cases available for Brooklyn classification.\n")
  } else {
    # Convert NEIGHBORHOOD to factor if not already
    classification_brooklyn$NEIGHBORHOOD <- as.factor(classification_brooklyn$NEIGHBORHOOD)
    
    classification_brooklyn$SALE.PRICE <- normalize(classification_brooklyn$SALE.PRICE)
    classification_brooklyn$GROSS.SQUARE.FEET <- normalize(classification_brooklyn$GROSS.SQUARE.FEET)
    
    nb_predictions_brooklyn <- predict(nb_model, classification_brooklyn)
    cat("Naïve Bayes Contingency Table for Brooklyn:\n")
    print(table(Predicted = nb_predictions_brooklyn, Actual = classification_brooklyn$NEIGHBORHOOD))
  }
}


# ---------------------------
# 3.c Observations and Conclusions
# ---------------------------
# ---------------------------
# 3.c Observations and Conclusions
# ---------------------------
# Observations:
# 1. Many neighborhood classes have few records, creating severe class imbalance.
# 2. Overall accuracy (~12.8%) is only slightly above the most frequent-class baseline (~9.95%).
# 3. Kappa (~0.07) is near zero, indicating poor agreement beyond random chance.
# 4. Sensitivity is very low for most classes, suggesting the model rarely predicts them correctly.

# Conclusions:
# 1. Class imbalance and limited data for many neighborhoods drive low performance.
# 2. Combining rare neighborhoods or using additional features could improve classification.
# 4. Despite slight improvement over guessing, more data and feature engineering are needed.
# ============================================================
