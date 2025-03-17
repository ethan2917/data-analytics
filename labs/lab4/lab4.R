###############################################
### Wine Data Analysis with PCA and kNN Classification ###
###############################################

# Load libraries
library(caret)
library(ggplot2)
library(ggfortify)

#------------------------------
# 1. Load the wine dataset
#------------------------------
wine <- read.csv("wine.data", header = FALSE)
colnames(wine) <- c("Wine", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash",
                    "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
                    "Proanthocyanins", "Color_Intensity", "Hue", "OD280_OD315", "Proline")
wine$Wine <- as.factor(wine$Wine)  # Convert wine class to factor

#------------------------------
# 2. PCA on full feature set
#------------------------------
# Use only the chemical features
X <- wine[, -1]

# Run PCA
pca <- princomp(X, cor = TRUE, score = TRUE)
summary(pca)

# Plot the data using the 1st and 2nd principal components
autoplot(pca, data = wine, colour = 'Wine', 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("PCA Plot of Wine Data (All Variables)")

#------------------------------
# 3. Identify variables contributing most to PC1
#------------------------------
loadings_pc1 <- pca$loadings[, 1]
cat("PC1 Loadings:\n")
print(loadings_pc1)

# Compute absolute loadings and sort
abs_loadings <- abs(loadings_pc1)
ordered_vars <- sort(abs_loadings, decreasing = TRUE)
cat("Variables ordered by contribution to PC1 (absolute loadings):\n")
print(ordered_vars)

#------------------------------
# 4. Drop the variables least contributing to PC1 and re-run PCA
#------------------------------
vars_to_drop <- names(sort(abs_loadings, decreasing = FALSE)[1:2])
cat("Dropping variables with least contribution to PC1:\n")
print(vars_to_drop)

# Create a reduced dataset
X_reduced <- X[, !(colnames(X) %in% vars_to_drop)]

# Re-run PCA
pca_reduced <- princomp(X_reduced, cor = TRUE, score = TRUE)
summary(pca_reduced)

# Plot the reduced PCA
autoplot(pca_reduced, data = wine, colour = 'Wine', 
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3) +
  ggtitle("PCA Plot of Wine Data (Reduced Variables)")

#------------------------------
# 5. kNN Classification using the Original Dataset
#------------------------------
# Split the data into training and test
set.seed(123)
trainIndex <- createDataPartition(wine$Wine, p = 0.7, list = FALSE)
trainData <- wine[trainIndex, ]
testData  <- wine[-trainIndex, ]

# Train a kNN model using caret
knn_model_orig <- train(Wine ~ ., data = trainData,
                        method = "knn",
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
cat("kNN model trained on original data:\n")
print(knn_model_orig)

# Predict on test set
pred_orig <- predict(knn_model_orig, newdata = testData)

# Create confusion matrix and compute metrics
cm_orig <- confusionMatrix(pred_orig, testData$Wine)
cat("Confusion Matrix for Original Data kNN model:\n")
print(cm_orig)

# Compute per-class precision, recall, and F1-score for original model
cm_table_orig <- cm_orig$table
precision_orig <- diag(cm_table_orig) / colSums(cm_table_orig)
recall_orig <- diag(cm_table_orig) / rowSums(cm_table_orig)
f1_orig <- 2 * precision_orig * recall_orig / (precision_orig + recall_orig)
cat("Performance Metrics for Original Data kNN model:\n")
print(data.frame(Precision = precision_orig,
                 Recall = recall_orig,
                 F1 = f1_orig))

#------------------------------
# 6. kNN Classification using the first 3 PC Scores
#------------------------------
# Extract the first 3 principal component scores and add the Wine class.
pca_scores <- data.frame(pca$scores[, 1:3])
pca_scores$Wine <- wine$Wine

# Split the PCA-transformed data
trainData_pca <- pca_scores[trainIndex, ]
testData_pca  <- pca_scores[-trainIndex, ]

# Train a kNN model on the first 3 PCs
knn_model_pca <- train(Wine ~ ., data = trainData_pca,
                       method = "knn",
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
cat("kNN model trained on first 3 PC scores:\n")
print(knn_model_pca)

# Predict on test set
pred_pca <- predict(knn_model_pca, newdata = testData_pca)

# Create confusion matrix for PCA model
cm_pca <- confusionMatrix(pred_pca, testData_pca$Wine)
cat("Confusion Matrix for PC Scores kNN model:\n")
print(cm_pca)

cm_table_pca <- cm_pca$table
precision_pca <- diag(cm_table_pca) / colSums(cm_table_pca)
recall_pca <- diag(cm_table_pca) / rowSums(cm_table_pca)
f1_pca <- 2 * precision_pca * recall_pca / (precision_pca + recall_pca)
cat("Performance Metrics for PC Scores kNN model:\n")
print(data.frame(Precision = precision_pca,
                 Recall = recall_pca,
                 F1 = f1_pca))

#------------------------------
# 7. Comparison of the Two Classification Models
#------------------------------
cat("Comparison of the Two Models:\n")
cat("\n--- Original Data kNN Model ---\n")
print(cm_orig$table)
cat("\n--- PC Scores kNN Model ---\n")
print(cm_pca$table)

cat("\nPerformance Metrics (Original Data):\n")
print(data.frame(Precision = precision_orig, Recall = recall_orig, F1 = f1_orig))
cat("\nPerformance Metrics (PC Scores):\n")
print(data.frame(Precision = precision_pca, Recall = recall_pca, F1 = f1_pca))
