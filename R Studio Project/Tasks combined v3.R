library(dplyr)
library(tibble)
library(caret)
library(ggplot2)
library(pROC)

# -----------------------
# Load and prepare data
# -----------------------
heart <- read.csv("SAM CW/heart.csv")

heart <- heart %>%
  mutate(target = factor(target, levels = c(0, 1)))

model_data <- heart %>%
  select(target, age, trestbps, chol, thalach, oldpeak) %>%
  mutate(across(c(age, trestbps, chol, thalach, oldpeak), as.numeric))

# -----------------------
# Train-test split (80/20)
# -----------------------
set.seed(123)
train_index <- createDataPartition(model_data$target, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

# -----------------------
# Train logistic regression (caret)
# -----------------------
log_model <- train(
  target ~ .,
  data = train_data,
  method = "glm",
  family = "binomial"
)

# -----------------------
# Predictions
# -----------------------
pred_class <- predict(log_model, newdata = test_data)
pred_prob  <- predict(log_model, newdata = test_data, type = "prob")[, "1"]

# -----------------------
# Confusion matrix
# -----------------------
cm <- confusionMatrix(pred_class, test_data$target)
print(cm)

# (Optional) Print key metrics clearly
cat("\nAccuracy:", cm$overall["Accuracy"], "\n")
cat("Sensitivity:", cm$byClass["Sensitivity"], "\n")
cat("Specificity:", cm$byClass["Specificity"], "\n")

# -----------------------
# ROC + AUC
# -----------------------
roc_obj <- roc(response = test_data$target, predictor = pred_prob)
auc_val <- auc(roc_obj)
print(auc_val)

plot(roc_obj, col = "blue", lwd = 2,
     main = "ROC Curve (Logistic Regression)")
abline(a = 0, b = 1, lty = 2)

# -----------------------
# Variable importance (caret)
# -----------------------
vi <- varImp(log_model, scale = TRUE)
print(vi)

vi_df <- vi$importance %>%
  rownames_to_column("variable") %>%
  as_tibble() %>%
  rename(importance = Overall) %>%
  arrange(desc(importance))

# Importance plot (ggplot2)
ggplot(vi_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Variable Importance (Logistic Regression)",
    subtitle = "caret::varImp() ranking of predictors",
    x = "", y = "Importance"
  ) +
  theme_minimal()


# Confusion matrix values
cm_df <- tibble(
  Prediction = c("Predicted 0", "Predicted 0", "Predicted 1", "Predicted 1"),
  Reference  = c("Actual 0", "Actual 1", "Actual 0", "Actual 1"),
  Count      = c(16, 6, 11, 27)
)

# Plot confusion matrix
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Confusion Matrix – Logistic Regression Model",
    x = "Actual Class",
    y = "Predicted Class",
    fill = "Count"
  ) +
  theme_minimal()





#-------=============All Combined----------=====================

# R Code for Full Part A: Heart Disease Dataset Analysis
# This script covers all required tasks: data summary, Task 1 (correlation), 
# Task 2 (association with target + visualization), Task 3 (GLM models)
# Run in R or RStudio

# Required packages (install if needed)
# install.packages(c("corrplot", "ggplot2", "dplyr", "tidyr", "performance"))

library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(performance)  # Optional: for model comparison

# Load data (adjust path if necessary)
df <- read.csv("SAM CW/heart.csv")

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

# --------------------------------------------------
# Task 1: Correlation Analysis (specified variables)
# --------------------------------------------------
cor_vars <- c("age", "trestbps", "chol", "thalach", "oldpeak")
cor_mat <- cor(df[, cor_vars])

cat("\nCorrelation matrix (Task 1):\n")
print(round(cor_mat, 3))

# Visualization: Heatmap (Figure 1)
corrplot(cor_mat, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "\nFigure 1: Correlation Matrix of Selected Numerical Variables",
         mar = c(0,0,2,0))

# --------------------------------------------------
# Task 2: Association with Target (all 13 predictors)
# --------------------------------------------------
predictors <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
                "thalach", "exang", "oldpeak", "slope", "ca", "thal")

# Function to compute point-biserial correlation (works for continuous/binary/categorical treated as numeric)
assoc_results <- data.frame(
  Predictor = predictors,
  Coefficient = NA,
  P_value = NA,
  stringsAsFactors = FALSE
)

for (pred in predictors) {
  test <- cor.test(df[[pred]], df$target)
  assoc_results[assoc_results$Predictor == pred, "Coefficient"] <- test$estimate
  assoc_results[assoc_results$Predictor == pred, "P_value"] <- test$p.value
}

# Sort by absolute coefficient
assoc_results <- assoc_results %>%
  mutate(Abs_Coefficient = abs(Coefficient),
         P_value_formatted = ifelse(P_value < 0.001, "<0.001", round(P_value, 3))) %>%
  arrange(desc(Abs_Coefficient))

cat("\nTask 2: Association with Target (Point-biserial correlations):\n")
print(assoc_results[, c("Predictor", "Coefficient", "P_value_formatted")])

# Visualization: Horizontal bar plot (Figure 2)
assoc_plot <- assoc_results %>%
  mutate(Predictor = factor(Predictor, levels = rev(Predictor)))  # Order by strength

ggplot(assoc_plot, aes(x = Coefficient, y = Predictor)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  labs(title = "Figure 2: Association Strength with Heart Disease Presence (target)",
       x = "Point-biserial Correlation Coefficient",
       y = "Predictor",
       caption = "Positive = higher values associate with disease presence\nNegative = higher values associate with absence") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# --------------------------------------------------
# Task 3 & 4: GLM Models
# --------------------------------------------------
# Model 1: target ~ age + sex
model1 <- glm(target ~ age + sex, data = df, family = binomial(link = "logit"))

cat("\nModel 1 Summary (target ~ age + sex):\n")
print(summary(model1))
cat("AIC:", round(AIC(model1), 2), "\n")

# Odds ratios
cat("\nOdds Ratios - Model 1:\n")
print(round(exp(coef(model1)), 3))
print(round(exp(confint(model1)), 3))

# Model 2: target ~ age + sex + thalach + oldpeak
model2 <- glm(target ~ age + sex + thalach + oldpeak, data = df, family = binomial(link = "logit"))

cat("\nModel 2 Summary (target ~ age + sex + thalach + oldpeak):\n")
print(summary(model2))
cat("AIC:", round(AIC(model2), 2), "\n")

# Odds ratios
cat("\nOdds Ratios - Model 2:\n")
print(round(exp(coef(model2)), 3))
print(round(exp(confint(model2)), 3))

# Model comparison
cat("\nModel Comparison:\n")
print(anova(model1, model2, test = "Chisq"))
cat("AIC Model 1:", round(AIC(model1), 2), "| AIC Model 2:", round(AIC(model2), 2), "\n")

# Pseudo R-squared
cat("\nPseudo R-squared (McFadden):\n")
cat("Model 1:", round(1 - model1$deviance / model1$null.deviance, 4), "\n")
cat("Model 2:", round(1 - model2$deviance / model2$null.deviance, 4), "\n")

# Optional: Save plots (uncomment if needed)
 ggsave("figure1_correlation_heatmap.png", width = 8, height = 6)
 ggsave("figure2_association_barplot.png", width = 9, height = 6)

