# excercises

# Week 6 Tutorial: Hypothesis Testing in R
# Learning Outcomes
# By the end of this tutorial you will be able to:
# • Perform one-sample, paired, and unpaired hypothesis tests in R.
# • Understand the di!erence between independent and paired designs. 
# • Visualise results using histograms and boxplots.


# ============================================================
# Exercise 1: Small vs Large Sample: Same True Difference
# ============================================================
library(ggplot2)
set.seed(123)

# True population parameters
mu_true <- 1060 # true mean (actually higher than claim) 
sigma <- 300 # population SD
pop_size <- 100000
population <- rnorm(pop_size, mean = mu_true, sd = sigma)

# Claimed population mean (null hypothesis)
mu0 <- 1000

# Two scenarios: small n and large n
n_small <- 20
n_large <- 300

sample_small <- sample(population, n_small)
sample_large <- sample(population, n_large)

# Helper function for z-test
z_test <- function(xbar, mu0, sigma, n) {
  z <- (xbar - mu0) / (sigma / sqrt(n))
  p <- 2 * (1 - pnorm(abs(z)))
  return(list(z = z, p = p))
}

# Calculate results
res_small <- z_test(mean(sample_small), mu0, sigma, n_small)
res_large <- z_test(mean(sample_large), mu0, sigma, n_large)

# Print results
cat("=============================================\n")
cat(" Small Sample (n =", n_small, ")\n")
cat("Sample mean =", round(mean(sample_small), 2), "\n")
cat("Z =", round(res_small$z, 2), " p =", round(res_small$p, 4), "\n\n")
cat(" Large Sample (n =", n_large, ")\n")
cat("Sample mean =", round(mean(sample_large), 2), "\n")
cat("Z =", round(res_large$z, 2), " p =", round(res_large$p, 4), "\n") 
cat("=============================================\n\n")


# ============================================================
# Exercise 2: Paired t-test with boxplot and connecting lines
# ============================================================
library(ggplot2)
library(tidyr)
set.seed(321)
# Simulate data (some improvement)
n <- 15
before <- round(rnorm(n, mean = 135, sd = 8), 1)
after <- round(before - rnorm(n, mean = 5, sd = 4), 1) # average drop $\approx$ 5 mmHg
