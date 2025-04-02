################################
# Evaluating Regression Models #
################################

# Load required libraries
library(readr)
library(ggplot2)
library(e1071)
library(caret)

# Read the dataset
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")
dataset <- NY_House_Dataset

# Check column names
print(names(dataset))

# Exploratory plots
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  ggtitle("Log-Log Scatter Plot: PRICE vs. PROPERTYSQFT")

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  ggtitle("Scatter Plot: PRICE vs. PROPERTYSQFT")

# Data Cleaning: Remove an outlier (if needed)
# Remove the observation with PROPERTYSQFT == 2184.207862
dataset_clean <- dataset[-which(dataset$PROPERTYSQFT == 2184.207862),]

# Set seed for reproducibility and create a 75/25 train/test split
set.seed(123)
train_indexes <- sample(1:nrow(dataset_clean), 0.75 * nrow(dataset_clean))
train <- dataset_clean[train_indexes, ]
test  <- dataset_clean[-train_indexes, ]

# Define a function to compute error metrics
compute_metrics <- function(actual, predicted) {
  error <- predicted - actual
  mae   <- mean(abs(error))
  mse   <- mean(error^2)
  rmse  <- sqrt(mse)
  return(c(MAE = mae, MSE = mse, RMSE = rmse))
}

# Note: We are modeling log10(PRICE) as a function of log10(PROPERTYSQFT)

###########################
# Model 1: Linear Regression
###########################
lin_mod <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), data = train)
lin_pred <- predict(lin_mod, newdata = test)
metrics_lin <- compute_metrics(log10(test$PRICE), lin_pred)
print("Linear Regression Metrics (Test Set):")
print(metrics_lin)

# Plot linear regression fit
ggplot(train, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  ggtitle("Linear Regression Fit")

###########################
# Model 2: SVM with Linear Kernel
###########################
svm_linear <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), data = train, kernel = "linear")
svm_linear_pred <- predict(svm_linear, newdata = test)
metrics_svm_linear <- compute_metrics(log10(test$PRICE), svm_linear_pred)
print("SVM Linear Kernel Metrics (Test Set):")
print(metrics_svm_linear)

# Plot SVM linear fit (using predictions on the training set)
ggplot(train, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  geom_line(aes(y = predict(svm_linear, newdata = train)), color = "green") +
  ggtitle("SVM Linear Kernel Fit")

###########################
# Model 3: SVM with Radial Kernel
###########################
# Here we use preset gamma and cost values (which could be further tuned)
svm_radial <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), data = train, kernel = "radial", gamma = 0.1, cost = 100)
svm_radial_pred <- predict(svm_radial, newdata = test)
metrics_svm_radial <- compute_metrics(log10(test$PRICE), svm_radial_pred)
print("SVM Radial Kernel Metrics (Test Set):")
print(metrics_svm_radial)

# Plot SVM radial fit (using predictions on the training set)
ggplot(train, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  geom_line(aes(y = predict(svm_radial, newdata = train)), color = "red") +
  ggtitle("SVM Radial Kernel Fit")

#########################################
# Optional: Monte Carlo Cross-Validation
#########################################
# Use k = 100 iterations of random 75/25 splits to estimate average errors

k <- 100

# Initialize vectors to store metrics for each model
mae_lin_cv <- mse_lin_cv <- rmse_lin_cv <- numeric(k)
mae_svm_lin_cv <- mse_svm_lin_cv <- rmse_svm_lin_cv <- numeric(k)
mae_svm_rad_cv <- mse_svm_rad_cv <- rmse_svm_rad_cv <- numeric(k)

for (i in 1:k) {
  # Create a new random train/test split for each iteration
  train_idx <- sample(1:nrow(dataset_clean), 0.75 * nrow(dataset_clean))
  train_cv <- dataset_clean[train_idx, ]
  test_cv  <- dataset_clean[-train_idx, ]
  
  # Linear Regression CV
  mod_lin_cv <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), data = train_cv)
  pred_lin_cv <- predict(mod_lin_cv, newdata = test_cv)
  err_lin_cv <- pred_lin_cv - log10(test_cv$PRICE)
  mae_lin_cv[i] <- mean(abs(err_lin_cv))
  mse_lin_cv[i] <- mean(err_lin_cv^2)
  rmse_lin_cv[i] <- sqrt(mse_lin_cv[i])
  
  # SVM Linear Kernel CV
  mod_svm_lin_cv <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), data = train_cv, kernel = "linear")
  pred_svm_lin_cv <- predict(mod_svm_lin_cv, newdata = test_cv)
  err_svm_lin_cv <- pred_svm_lin_cv - log10(test_cv$PRICE)
  mae_svm_lin_cv[i] <- mean(abs(err_svm_lin_cv))
  mse_svm_lin_cv[i] <- mean(err_svm_lin_cv^2)
  rmse_svm_lin_cv[i] <- sqrt(mse_svm_lin_cv[i])
  
  # SVM Radial Kernel CV
  mod_svm_rad_cv <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), data = train_cv, kernel = "radial", gamma = 0.1, cost = 100)
  pred_svm_rad_cv <- predict(mod_svm_rad_cv, newdata = test_cv)
  err_svm_rad_cv <- pred_svm_rad_cv - log10(test_cv$PRICE)
  mae_svm_rad_cv[i] <- mean(abs(err_svm_rad_cv))
  mse_svm_rad_cv[i] <- mean(err_svm_rad_cv^2)
  rmse_svm_rad_cv[i] <- sqrt(mse_svm_rad_cv[i])
}

# Compute average metrics over k iterations
cv_lin   <- c(MAE = mean(mae_lin_cv), MSE = mean(mse_lin_cv), RMSE = mean(rmse_lin_cv))
cv_svm_lin   <- c(MAE = mean(mae_svm_lin_cv), MSE = mean(mse_svm_lin_cv), RMSE = mean(rmse_svm_lin_cv))
cv_svm_rad   <- c(MAE = mean(mae_svm_rad_cv), MSE = mean(mse_svm_rad_cv), RMSE = mean(rmse_svm_rad_cv))

print("Monte Carlo CV Metrics for Linear Regression:")
print(cv_lin)
print("Monte Carlo CV Metrics for SVM Linear Kernel:")
print(cv_svm_lin)
print("Monte Carlo CV Metrics for SVM Radial Kernel:")
print(cv_svm_rad)

#### THE END ####
