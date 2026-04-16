# Task 1: Correlation Analysis ch
# ----------------------------

library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# 1) Load data
heart <- read.csv("SAM CW/heart.csv")


# --------------------------------------------------
# Basic Data Overview & Summary
# --------------------------------------------------
cat("Dataset dimensions:", dim(df), "\n")
cat("Missing values:", sum(is.na(df)), "\n\n")

# Variable types
str(df)

# Target distribution
cat("\nTarget distribution (1 = disease, 0 = no disease):\n")
print(table(df$target))
cat("Prevalence:", round(mean(df$target == 1), 3), "\n")

# Prevalence by sex
cat("\nPrevalence by sex (0 = female, 1 = male):\n")
print(round(prop.table(table(df$sex, df$target), margin = 1), 3))

# Summary statistics for numerical variables
num_vars <- c("age", "trestbps", "chol", "thalach", "oldpeak")
cat("\nSummary statistics:\n")
print(summary(df[, num_vars]))



# 2) Keep only required numeric variables
num_vars <- c("age", "trestbps", "chol", "thalach", "oldpeak")

df_num <- heart %>%
  select(all_of(num_vars)) %>%
  # Make sure they are numeric (in case something imported as character)
  mutate(across(everything(), as.numeric))

# 3) Quick checks: missing values + simple summaries
missing_counts <- df_num %>%
  summarise(across(everything(), ~ sum(is.na(.))))

summary_stats <- df_num %>%
  summarise(across(everything(),
                   list(mean = ~ mean(., na.rm = TRUE),
                        sd   = ~ sd(., na.rm = TRUE),
                        min  = ~ min(., na.rm = TRUE),
                        q1   = ~ quantile(., 0.25, na.rm = TRUE),
                        med  = ~ median(., na.rm = TRUE),
                        q3   = ~ quantile(., 0.75, na.rm = TRUE),
                        max  = ~ max(., na.rm = TRUE))))

print(missing_counts)
print(summary_stats)

# Optional: simple outlier view using boxplots
df_long <- df_num %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

p_box <- ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.alpha = 0.6) +
  labs(title = "Boxplots of Numerical Variables (Outlier/Skew Check)",
       x = "", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# 4) Compute correlation matrix (Pearson)
# pairwise.complete.obs handles missing values safely
cor_mat <- cor(df_num, use = "pairwise.complete.obs", method = "pearson")
print(round(cor_mat, 3))

# 5) Convert correlation matrix to "long" format for ggplot heatmap
cor_long <- as.data.frame(as.table(cor_mat)) %>%
  as_tibble() %>%
  rename(var1 = Var1, var2 = Var2, r = Freq) %>%
  mutate(r_label = sprintf("%.2f", r))

# 6) Heatmap (ggplot2)
p_heat <- ggplot(cor_long, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = r_label), size = 4) +
  coord_equal() +
  labs(title = "Correlation Heatmap (Pearson r)",
       subtitle = "Variables: age, trestbps, chol, thalach, oldpeak",
       x = "", y = "", fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# 7) Put plots together (patchwork)
(p_box / p_heat)


# Task 2 – Association with Target
# --------------------------------
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Load data
heart <- read.csv("SAM CW/heart.csv")

# Ensure target is numeric 0/1
heart <- heart %>%
  mutate(target = as.numeric(target))

num_vars <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
              "thalach", "exang", "oldpeak", "slope", "ca", "thal")

# ----- Association table: correlation + p-value for each predictor vs target -----
assoc_tbl <- tibble(var = num_vars) %>%
  rowwise() %>%
  mutate(
    test = list(cor.test(heart[[var]], heart$target,
                         use = "complete.obs", method = "pearson")),
    r = unname(test$estimate),
    p_value = test$p.value
  ) %>%
  ungroup() %>%
  mutate(
    abs_r = abs(r),
    p_value = signif(p_value, 3),
    r = round(r, 3)
  ) %>%
  arrange(desc(abs_r))

print(assoc_tbl)

# ----- Plot 1: Strength of association (absolute correlation) -----
p_strength <- ggplot(assoc_tbl, aes(x = reorder(var, abs_r), y = abs_r)) +
  geom_col() +
  coord_flip() +
  labs(title = "Task 2: Strength of Association with target",
       subtitle = "Absolute Pearson correlation (point-biserial for binary target)",
       x = "", y = "|r|") +
  theme_minimal()

# ----- Plot 2: Direction of association (signed correlation) -----
p_signed <- ggplot(assoc_tbl, aes(x = reorder(var, r), y = r)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +
  coord_flip() +
  labs(title = "Direction of Association with target",
       subtitle = "Positive r: higher predictor tends to relate to target=1",
       x = "", y = "r") +
  theme_minimal()

# ----- Optional: Boxplots to show separation by target (nice for report) -----
df_long <- heart %>%
  select(all_of(num_vars), target) %>%
  pivot_longer(cols = all_of(num_vars), names_to = "variable", values_to = "value") %>%
  mutate(target = factor(target, levels = c(0, 1), labels = c("No disease (0)", "Disease (1)")))

p_box <- ggplot(df_long, aes(x = target, y = value)) +
  geom_boxplot(outlier.alpha = 0.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Numeric Predictors by target Group",
       x = "", y = "Value") +
  theme_minimal()

# Combine plots (patchwork)
(p_strength | p_signed) / p_box


