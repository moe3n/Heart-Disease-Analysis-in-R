# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(corrplot)

# Load the dataset
heart_data <- read.csv("SAM CW/heart.csv")

# Check for missing values (if any)
sum(is.na(heart_data))

# Select continuous variables (predictors)
numerical_vars <- heart_data %>% select(age, trestbps, chol, thalach, oldpeak, ca, thal)

# Initialize an empty data frame to store results for correlation and p-values
cor_results <- data.frame(Variable = character(), Correlation = numeric(), P_value = numeric())

# Loop through each numerical variable and compute Pearson correlation with the binary target
for (var in colnames(numerical_vars)) {
  test <- cor.test(heart_data[[var]], heart_data$target)
  cor_results <- rbind(cor_results, data.frame(Variable = var, Correlation = test$estimate, P_value = test$p.value))
}

# Display correlation results (correlation coefficients and p-values)
print(cor_results)

# Visualization: Heatmap of Correlation Matrix
cor_matrix <- cor(numerical_vars, heart_data$target)  # Compute correlation matrix between predictors and target
cor_matrix_df <- data.frame(Variable = names(cor_matrix), Correlation = cor_matrix)  # Convert to dataframe

# Heatmap visualization of correlations with the target variable
ggplot(cor_matrix_df, aes(x = Variable, y = 1, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0) +
  geom_text(aes(label = round(Correlation, 2)), color = "white", size = 5) +
  labs(title = "Correlation of Predictors with Heart Disease (Target)", x = "Predictors", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Visualization: Scatter plots for each continuous predictor against the binary target
# Scatter plots for individual predictors vs target variable

# Create a function to plot scatter plots
scatter_plot <- function(variable) {
  ggplot(heart_data, aes_string(x = variable, y = "target")) +
    geom_jitter(width = 0.1, height = 0.1, alpha = 0.6, color = "blue") +
    labs(title = paste("Scatter plot of", variable, "vs Heart Disease"), x = variable, y = "Heart Disease (Target)") +
    theme_minimal()
}

# Loop through all predictors and generate scatter plots
for (var in colnames(numerical_vars)) {
  print(scatter_plot(var))
}

# Visualize correlation matrix using corrplot
# Create a correlation matrix including only the predictors and the target variable
full_cor_matrix <- cor(heart_data %>% select(age, trestbps, chol, thalach, oldpeak, ca, thal, target))
corrplot(full_cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.cex = 0.7, diag = FALSE, title = "Correlation Matrix")

