###############################################
# Assignment 4 – Data Analytics (F25) – Level 4000
# Author: Ethan Garcia
# Hypothesis: Rising unemployment leads (with a
#             lag) to higher loan‑delinquency rates.
# Data files used: UNRATE.csv, FRB_CHGDEL.csv
###############################################

# ---- Libraries ----
required <- c("tidyverse", "lubridate", "zoo", "patchwork",
              "randomForest", "rpart", "rpart.plot")
invisible(lapply(required, function(p)
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)))

library(tidyverse); library(lubridate); library(zoo); library(patchwork)
library(randomForest); library(rpart); library(rpart.plot)

# ---- Import & tidy ------------------------------------------------------
# Unemployment (monthly, FRED series)
unemp_raw <- read_csv("UNRATE.csv",
                      col_types = cols(
                        observation_date = col_date(format = ""),
                        UNRATE           = col_double()
                      ))

unemp_qtr <- unemp_raw %>%
  mutate(quarter = paste0(year(observation_date), "Q", quarter(observation_date))) %>%
  group_by(quarter) %>%
  summarise(unemp = mean(UNRATE, na.rm = TRUE), .groups = "drop")

# Delinquency / charge‑off (quarterly, keep “all loans”)
chgdel_raw <- read_csv("FRB_CHGDEL.csv", skip = 5)
chgdel <- chgdel_raw %>%
  select(quarter = `Time Period`,
         delinq  = `STFBQC%STFBAIL_MA.Q`) %>%
  mutate(delinq = as.numeric(delinq)) %>%
  filter(!is.na(delinq))

# Merge series
df <- inner_join(chgdel, unemp_qtr, by = "quarter") %>%
  mutate(date = as.yearqtr(quarter, format = "%YQ%q")) %>%
  arrange(date)

dir.create("figures", showWarnings = FALSE)

# ---- Data Description & Preliminary Plots -------------------------------
p1 <- ggplot(df, aes(date, delinq)) +
  geom_line() +
  labs(title = "Charge‑off / Delinquency Rate (All Loans)",
       x = NULL, y = "Percent") +
  theme_minimal()

p2 <- ggplot(df, aes(date, unemp)) +
  geom_line() +
  labs(title = "Unemployment Rate (Quarterly Avg.)",
       x = NULL, y = "Percent") +
  theme_minimal()

ggsave("figures/section2_timeseries.png",
       p1 / p2,               # patchwork object
       width = 8, height = 6, dpi = 320)

p_scatter <- ggplot(df, aes(unemp, delinq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Delinquency vs. Unemployment (Same Quarter)",
       x = "Unemployment %", y = "Delinquency %") +
  theme_minimal()

ggsave("figures/section2_scatter.png", p_scatter,
       width = 5, height = 4, dpi = 320)

# ---- Exploratory Analysis ----------------------------------------------
## Rolling‑window correlation heat‑map
win_len <- 20          # 20 quarters ≈ 5 years
roll_corr <- tibble(
  end_idx = seq(win_len, nrow(df)),
  date    = df$date[seq(win_len, nrow(df))],
  corr    = map_dbl(end_idx,
                    ~ cor(df$unemp[(.x - win_len + 1):.x],
                          df$delinq[(.x - win_len + 1):.x],
                          use = "complete.obs"))
)

p_roll <- ggplot(roll_corr, aes(x = date, y = 1, fill = corr)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D", limits = c(-1, 1)) +
  labs(title = paste0("Rolling ", win_len, "-Quarter Correlation: ",
                      "Unemployment vs. Delinquency"),
       x = "Window End Date", y = NULL, fill = "Corr") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("figures/section3_rolling_corr.png", p_roll,
       width = 8, height = 2.5, dpi = 320)

# ---- Model Development --------------------------------------------------
# Lag features for tree/forest and regression
df <- df %>%
  arrange(date) %>%
  mutate(across(c(unemp, delinq), list(lag1 = ~ lag(.x, 1),
                                       lag2 = ~ lag(.x, 2))),
         .keep = "all") %>%
  drop_na()                                  # drop first two rows

# Random‑Forest model (non‑linear time tree)
set.seed(123)
rf_fit <- randomForest(
  delinq ~ unemp_lag1 + unemp_lag2 +
    delinq_lag1 + delinq_lag2,
  data = df,
  ntree = 500, importance = TRUE
)
## Variable‑importance bar‑plot  (fixed)
varimp_df <- as.data.frame(importance(rf_fit)) %>%
  rownames_to_column("Variable") %>%
  rename(IncMSE = `%IncMSE`) %>%  
  arrange(desc(IncMSE))

p_varimp <- ggplot(varimp_df,
                   aes(reorder(Variable, IncMSE), IncMSE)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Random‑Forest Variable Importance",
       x = NULL,
       y = "Importance (↑ = bigger impact on forecast error)") +
  theme_minimal()

ggsave("figures/section4_varimp.png", p_varimp,
       width = 6, height = 4, dpi = 320)

# Single decision‑tree
tree_fit <- rpart(
  delinq ~ unemp_lag1 + unemp_lag2 +
    delinq_lag1 + delinq_lag2,
  data = df,
  control = rpart.control(cp = 0.01)   # prune for legibility
)

png("figures/section4_tree.png", width = 900, height = 600, res = 120)
rpart.plot(tree_fit, type = 4, fallen.leaves = TRUE,
           main = "Decision Tree (Delinquency Forecast)")
dev.off()

# Simplified regression fit plot
df$pred_lm <- predict(lm_fit)

p_fit <- ggplot(df, aes(x = pred_lm, y = delinq)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs. Actual Delinquency",
       x = "Predicted (%)",
       y = "Actual (%)",
       caption = "Dashed line = perfect predictions") +
  theme_minimal()

ggsave("figures/section4_lm_fit.png", p_fit,
       width = 6, height = 4, dpi = 320)


# Comparison table
rf_rmse <- sqrt(mean((rf_fit$y - rf_fit$predicted)^2))
lm_rmse <- sqrt(mean(residuals(lm_fit)^2))

model_comp <- tibble(
  Model = c("Random‑Forest", "Lagged‑LM"),
  RMSE  = c(rf_rmse, lm_rmse)
)
print(model_comp)

# ---- Save cleaned analysis ------------------------------------------
write_csv(df, "cleaned_delinquency_unemployment.csv")
