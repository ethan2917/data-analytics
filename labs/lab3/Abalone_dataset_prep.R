###################
##### Abalone #####
###################
library(class)

# read dataset
abalone <- read.csv("abalone_dataset.csv")

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings,
                         br=c(0,8,11,35),
                         labels = c("young", 'adult', 'old'))

set.seed(123)

# 70%/30% train/test split
indexes <- sample(1:nrow(dataset), size = 0.7 * nrow(dataset))
trainset <- dataset[indexes, ]
testset  <- dataset[-indexes, ]




# Model 1 uses length, diameter, and height
knn_pred1 <- knn(
  train = trainset[, c("length", "diameter", "height")],
  test  = testset[, c("length", "diameter", "height")],
  cl    = trainset$age.group,
  k = 3
)

# Model 2 uses shucked_weight, viscera_weight, shell_weight
knn_pred2 <- knn(
  train = trainset[, c("shucked_wieght", "viscera_wieght", "shell_weight")],
  test  = testset[, c("shucked_wieght", "viscera_wieght", "shell_weight")],
  cl    = trainset$age.group,
  k = 3
)

# Contingency Tables
# Model 1
table(Predicted = knn_pred1, Actual = testset$age.group)
acc1 <- mean(knn_pred1 == testset$age.group) 
cat("Accuracy of Model 1:", acc1, "\n")

# Model 2
table(Predicted = knn_pred2, Actual = testset$age.group)
acc2 <- mean(knn_pred2 == testset$age.group)
cat("Accuracy of Model 2:", acc2, "\n")

# If Model 1 is better
feature_set <- c("length", "diameter", "height")

acc_values <- numeric()

for(k_val in 1:15) {
  pred_temp <- knn(
    train = trainset[, feature_set],
    test  = testset[, feature_set],
    cl    = trainset$age.group,
    k = k_val
  )
  acc_values[k_val] <- mean(pred_temp == testset$age.group)
}

# Identify the k with the highest accuracy
best_k <- which.max(acc_values)
best_acc <- max(acc_values)

cat("Best k:", best_k, "with accuracy:", best_acc, "\n")


