# Assignment 2
# Ethan Garcia
# garcie8@rpi.edu

library("readr")

epi_results_2024_pop_gdp <- read_csv("epi_results_2024_pop_gdp.csv")
epi_data <- epi_results_2024_pop_gdp

#1
#1.1
# Subset for "Southern Asia" and "Eastern Europe"
sub_data1 <- subset(epi_data, region == "Southern Asia")
sub_data2 <- subset(epi_results_2024_pop_gdp, region == "Eastern Europe")

# Check data
nrow(sub_data1)
head(sub_data1)
nrow(sub_data2)
head(sub_data2)

# Histogram for Region 1 (Southern Asia)
hist(sub_data1$EPI.new, main = "EPI Distribution (Southern Asia)", xlab = "EPI (New)", col  = "lightblue", freq = FALSE)
lines(density(sub_data1$EPI.new, na.rm = TRUE), col = "red", lwd = 2)

# Histogram for Region 2 (Eastern Europe)
# Histogram for Region 1 (Southern Asia)
hist(sub_data2$EPI.new, main = "EPI Distribution (Eastern Europe)", xlab = "EPI (New)", col  = "lightblue", freq = FALSE)
lines(density(sub_data2$EPI.new, na.rm = TRUE), col = "blue", lwd = 2)

#1.2
# QQ plot for sub_data1 compared to normal distribution
qqnorm(sub_data1$EPI.new, main = "QQ Plot of EPI (Southern Asia)")
qqline(sub_data1$EPI.new, col = "red")

# QQ plot for sub_data2 compared to normal distribution
qqnorm(sub_data2$EPI.new, main = "QQ Plot of EPI (Eastern Europe)")
qqline(sub_data2$EPI.new, col = "blue")


#2
#2.1
# Fit model 1
model1 <- lm(EPI.new ~ gdp, data = epi_data)

# Print model summary
print("Model 1 Summary (EPI.new vs GDP")
print(summary(model1))

# Plot the response vs. the most significant predictor
plot(epi_data$gdp, epi_data$EPI.new,
     xlab = "GDP", ylab = "EPI (New)",
     main = "EPI (New) vs. GDP")
abline(model1, col = "red", lwd = 2)

# Plot residuals
plot(fitted(model1), resid(model1),
     xlab = "Fitted EPI", ylab = "Residuals",
     main = "Residuals vs. Fitted (Model 1)")
abline(h = 0, col = "blue", lty = 2)

# Fit model 2
model2 <- lm(EPI.old ~ gdp + population, data = epi_data)

# 2. Print model summary
print("Model 2 Summary (EPI.old vs GDP + Population")
print(summary(model2))

# Plot GDP (Most significant) vs. EPI.old
plot(epi_data$gdp, epi_data$EPI.old,
     xlab = "GDP", ylab = "EPI (Old)",
     main = "EPI (Old) vs. GDP")
# Fix data to plot line
pop_mean <- mean(epi_data$population, na.rm = TRUE)
new_data <- data.frame(gdp = epi_data$gdp, population = pop_mean)
pred_vals <- predict(model2, newdata = new_data)

ix <- order(epi_data$gdp)
lines(epi_data$gdp[ix], pred_vals[ix], col = "green", lwd = 2)

# Plot residuals
plot(fitted(model2), resid(model2),
     xlab = "Fitted EPI (Old)", ylab = "Residuals",
     main = "Residuals vs. Fitted (Model 2)")
abline(h = 0, col = "purple", lty = 2)

#2.2
# Fit model 1 using subset 1 (Southern Asia)
# Model 1: EPI (New) ~ GDP
model1_south_asia <- lm(EPI.new ~ gdp, data = sub_data1)
summary(model1_south_asia)

# Model 2: EPI (Old) ~ GDP + Population
model2_south_asia <- lm(EPI.old ~ gdp + population, data = sub_data1)
summary(model2_south_asia)

cat("Model 1 for subset 1 (EPI.new ~ gdp) is better because it has a higher adjusted R-squared and a significant p-value.\n")


#3
# 3.1
sub_data_kNN <- subset(epi_data, region %in% c("Global West", "Latin America & Caribbean"))

# Select 3 numeric variables
vars_kNN_1 <- c("EPI.new", "ECO.new", "BDH.new")

# Create new data frame
df_kNN_1 <- sub_data_kNN[, c(vars_kNN_1, "region")]

# Make sure 'region' is a factor with only 2 levels
df_kNN_1$region <- factor(df_kNN_1$region)

# Split and train
set.seed(123) 
train_idx <- sample(seq_len(nrow(df_kNN_1)), size = 0.8 * nrow(df_kNN_1))

train_data_1 <- df_kNN_1[train_idx, ]
test_data_1  <- df_kNN_1[-train_idx, ]

# Separate predictors and the class label
train_x_1 <- train_data_1[, vars_kNN_1]
train_y_1 <- train_data_1$region

test_x_1 <- test_data_1[, vars_kNN_1]
test_y_1 <- test_data_1$region

library(class)

# Set K to 2 since it has an accuraccy of 1.0
k_value <- 2

pred_1 <- knn(train = train_x_1, test = test_x_1, cl = train_y_1, k = k_value)

# Confusion Matrix
table_pred_1 <- table(Predicted = pred_1, Actual = test_y_1)
print(table_pred_1)

accuracy_1 <- sum(diag(table_pred_1)) / sum(table_pred_1)
cat("Accuracy (Model 1, k =", k_value, ") =", accuracy_1, "\n")


# 3.2
vars_kNN_2 <- c("EPI.old", "ECO.old", "BDH.old")

df_kNN_2 <- sub_data_kNN[, c(vars_kNN_2, "region")]
df_kNN_2$region <- factor(df_kNN_2$region)

set.seed(456)
train_idx_2 <- sample(seq_len(nrow(df_kNN_2)), size = 0.8 * nrow(df_kNN_2))

train_data_2 <- df_kNN_2[train_idx_2, ]
test_data_2  <- df_kNN_2[-train_idx_2, ]

train_x_2 <- train_data_2[, vars_kNN_2]
train_y_2 <- train_data_2$region

test_x_2 <- test_data_2[, vars_kNN_2]
test_y_2 <- test_data_2$region

# 5 is the best again
k_value_2 <- 5

pred_2 <- knn(train = train_x_2, test = test_x_2, cl = train_y_2, k = k_value_2)
table_pred_2 <- table(pred_2, test_y_2)

accuracy_2 <- sum(diag(table_pred_2)) / sum(table_pred_2)
cat("Accuracy (Model 2, k =", k_value_2, ") =", accuracy_2, "\n")

# Explain which is better using accuracy
cat("Model 1 had accuracy of", accuracy_1, 
    "while Model 2 had accuracy of", accuracy_2, 
    ". Model", ifelse(accuracy_1 > accuracy_2, "1 is", "2 is"), "better.\n")

