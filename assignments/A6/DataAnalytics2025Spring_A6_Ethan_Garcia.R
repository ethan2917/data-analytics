# Bank Marketing – Assignment 6 Analysis
# Author: Ethan Garcia
# Course: Data Analytics S25 (4000‑level)
# -----------------------------------------------------------------------------

req_pkgs <- c("tidyverse", "janitor", "caret", "glmnet", "randomForest",
              "pROC", "yardstick", "corrplot", "DataExplorer", "factoextra",
              "cluster")   # <-- added 'cluster' for silhouette()
for (p in req_pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(tidyverse)
library(janitor)
library(caret)
library(glmnet)
library(randomForest)
library(pROC)
library(yardstick)
library(corrplot)
library(DataExplorer)
library(factoextra)
library(cluster)

set.seed(42)  
# Basic cleaning
raw <- read_delim("bank.csv", delim = ";")

df <- raw %>%
  clean_names() %>%
  mutate(across(where(is.character), as_factor)) %>%
  mutate(y = factor(y, levels = c("no", "yes")))

# Exploratory Data Analysis
if (!dir.exists("plots")) dir.create("plots")

ggplot(df, aes(y)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Class distribution – Term Deposit Subscription",
       x = "Subscribed", y = "Count")
ggsave("plots/target_bar.png", width = 6, height = 4)

ggplot(df, aes(duration)) +
  geom_histogram(binwidth = 50, colour = "black", fill = "grey70") +
  theme_minimal() +
  labs(title = "Call Duration Distribution", x = "Seconds", y = "Count")

ggsave("plots/duration_hist.png", width = 6, height = 4)

nums <- df %>% select(where(is.numeric))
C <- cor(nums)

png("plots/corr_heatmap.png", width = 800, height = 600)
corrplot(C, method = "color", tl.cex = 0.7)
dev.off()

# Pre‑processing
prep  <- dummyVars(y ~ ., data = df, fullRank = TRUE)
X     <- as_tibble(predict(prep, df))
mod_df <- bind_cols(X, y = df$y)

train_idx <- createDataPartition(mod_df$y, p = 0.8, list = FALSE)
train <- mod_df[train_idx, ]
test  <- mod_df[-train_idx, ]

# Classification
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3,
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     savePredictions = "final")

logit <- train(y ~ ., data = train, method = "glmnet", metric = "ROC",
               trControl = ctrl, tuneLength = 15)

rf <- train(y ~ ., data = train, method = "rf", metric = "ROC",
            trControl = ctrl, tuneLength = 5)

cls_metrics <- function(model, name) {
  prob <- predict(model, test, type = "prob")[["yes"]]
  roc_obj <- roc(test$y, prob, levels = c("no", "yes"))
  pred_cls <- factor(if_else(prob > 0.5, "yes", "no"), levels = c("no", "yes"))
  cm <- confusionMatrix(pred_cls, test$y, positive = "yes")
  tibble(Model    = name,
         AUC      = as.numeric(auc(roc_obj)),
         Accuracy = as.numeric(cm$overall["Accuracy"]),
         F1       = as.numeric(cm$byClass["F1"]))
}

results_cls <- bind_rows(
  cls_metrics(logit, "ElasticNet Logit"),
  cls_metrics(rf,    "Random Forest")
)
write_csv(results_cls, "classification_results.csv")
print(results_cls)

roc_logit <- roc(test$y, predict(logit, test, type = "prob")[["yes"]])
roc_rf    <- roc(test$y, predict(rf,    test, type = "prob")[["yes"]])

png("plots/roc_curves.png", width = 800, height = 600)
plot(roc_logit, print.auc = TRUE, main = "ROC – Term Deposit Subscription")
plot(roc_rf, add = TRUE, col = "blue", print.auc = TRUE, print.auc.y = 0.4)
legend("bottomright", legend = c("ElasticNet", "Random Forest"),
       col = c("black", "blue"), lwd = 2)
dev.off()

imp <- varImp(rf)

png("plots/rf_importance.png", width = 800, height = 600)
plot(imp, top = 10, main = "Random Forest – Top 10 Features")
dev.off()

# Clustering
scaled_nums <- scale(nums)
ks <- 2:6
sils <- map_dbl(ks, ~ {
  km <- kmeans(scaled_nums, .x, nstart = 25)
  sil <- cluster::silhouette(km$cluster, dist(scaled_nums))
  mean(sil[, 3])
})
opt_k <- ks[which.max(sils)]
print(paste("Optimal k by silhouette =", opt_k))

km <- kmeans(scaled_nums, centers = opt_k, nstart = 25)
df$cluster <- factor(km$cluster)

png("plots/cluster_sizes.png", width = 600, height = 400)
barplot(table(df$cluster), col = "skyblue", main = "Cluster sizes")
dev.off()

clus_rate <- df %>%
  group_by(cluster) %>%
  summarise(rate = mean(as.numeric(y == "yes")))
write_csv(clus_rate, "cluster_deposit_rates.csv")
print(clus_rate)

# Wrap‑up
write_lines("Classification and clustering artefacts exported (see ./plots & CSVs)",
            "run_log.txt")

# End of bank_marketing_analysis.R
