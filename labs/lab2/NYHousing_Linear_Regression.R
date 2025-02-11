library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

# Models
## Model 1 Using PROPERTYSQFT, BEDS, and BATH as the predictors
m1 <- lm(log10(PRICE)~log10(PROPERTYSQFT) + BEDS + BATH, data = dataset)
## Model 2 Using PROPERTYSQFT + BEDS as the predictors
m2 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT) + BEDS, data = dataset)
## Model 3 Using BATHS + BEDS as the predictors
m3 <- lm(log10(PRICE) ~ BATH + BEDS, data = dataset)


## Print model output
cat("Model 1 Summary\n")
print(summary(m1))
cat("Model 2 Summary\n")
print(summary(m2))
cat("Model 3 Summary\n")
print(summary(m3))

# Plot of variables with best fit line
# M1 using PROPERTYSQFT as most significant variable
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Model 1: log10(PRICE) vs. log10(PROPERTYSQFT)",
       x = "log10(Property SqFt)",
       y = "log10(Price)")

# M2 using BEDS as most significant variable
ggplot(dataset, aes(x = BEDS, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Model 2: log10(PRICE) vs. BEDS",
       x = "BEDS",
       y = "log10(Price)")

# M3 using BATH as most significant variable
ggplot(dataset, aes(x = BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Model 3: log10(PRICE) vs. BATH",
       x = "Bath",
       y = "log10(Price)")

## Scatter plot of residuals
# M1
res1 <- data.frame(Fitted = fitted(m1), Residuals = residuals(m1))
ggplot(res1, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "blue") +
  labs(title = "Model 1: Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

res2 <- data.frame(Fitted = fitted(m2), Residuals = residuals(m2))
ggplot(res2, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "blue") +
  labs(title = "Model 2: Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

res3 <- data.frame(Fitted = fitted(m3), Residuals = residuals(m3))
ggplot(res3, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "blue") +
  labs(title = "Model 3: Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

