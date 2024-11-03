#### Preamble ####
# Purpose: Models the forecast for the 2024 U.S. presidential election using "poll-of-polls"
# Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data should be available at 'outputs/data/analysis_data.csv'
# Any other information needed? Data must include relevant predictors like percentage, sample size, state, etc.

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(arrow)

# Set a seed for reproducibility
set.seed(853)

### Read data ###
analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")


# Filter data for Harris and Trump
harris_data <- analysis_data %>% filter(candidate == "Kamala Harris")
trump_data <- analysis_data %>% filter(candidate == "Donald Trump")

# Bayesian hierarchical model for Harris's support
harris_model <- stan_glm(
  percentage ~ pollster_name + sample_size + state,
  data = harris_data,
  family = gaussian(),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  seed = 853
)

# Generate posterior predictions for Harris' support
harris_pred <- posterior_predict(harris_model)
harris_pred_means <- colMeans(harris_pred)  # Mean predicted support across simulations

# Bayesian hierarchical model for Trump's support
trump_model <- stan_glm(
  percentage ~ pollster_name + sample_size + state,
  data = trump_data,
  family = gaussian(),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  seed = 853
)

# Generate posterior predictions for Trump's support
trump_pred <- posterior_predict(trump_model)
trump_pred_means <- colMeans(trump_pred)  # Mean predicted support across simulations

# Simulate election outcomes by comparing Harris and Trump's predicted support
simulated_outcomes <- ifelse(harris_pred_means > trump_pred_means, "Harris", "Trump")

# Calculate probabilities
harris_win_prob <- mean(simulated_outcomes == "Harris")
trump_win_prob <- mean(simulated_outcomes == "Trump")

# Print probabilities
cat("Probability Harris Wins:", harris_win_prob, "\n")
cat("Probability Trump Wins:", trump_win_prob, "\n")

# Plot distribution of predicted support for Harris
ggplot(data.frame(harris_pred_means), aes(x = harris_pred_means)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Predicted Support for Kamala Harris",
    x = "Predicted Support Percentage",
    y = "Frequency",
    caption = "Fig 3: This histogram shows the distribution of predicted support percentages\nfor Kamala Harris based on model simulations."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 10))
  )


# Plot distribution of predicted support for Trump
ggplot(data.frame(trump_pred_means), aes(x = trump_pred_means)) +
  geom_histogram(bins = 30, fill = "red", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Predicted Support for Donald Trump",
    x = "Predicted Support Percentage",
    y = "Frequency", 
    caption = "Fig 4: This histogram shows the distribution of predicted support percentages\nfor Donald Trump based on model simulations."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 10))
  )

# Determine the minimum length between the two prediction sets
min_length <- min(length(harris_pred_means), length(trump_pred_means))

# Subset both predictions to have the same length
harris_pred_means <- harris_pred_means[1:min_length]
trump_pred_means <- trump_pred_means[1:min_length]

# Combine predictions for comparison plot
comparison_data <- data.frame(
  support = c(harris_pred_means, trump_pred_means),
  candidate = rep(c("Harris", "Trump"), each = min_length)
)

# Plot comparison of support for Harris and Trump
ggplot(comparison_data, aes(x = support, fill = candidate)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Comparison of Predicted Support for Harris and Trump",
    x = "Predicted Support Percentage",
    y = "Density",
    fill = "Candidate",
    caption = "Fig 5: This density plot compares the predicted support percentages\n for Kamala Harris and Donald Trump based on model simulations."
  ) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 10))  # Adjust caption styling
  )