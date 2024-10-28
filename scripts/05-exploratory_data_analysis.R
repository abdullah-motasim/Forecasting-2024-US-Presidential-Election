#### Preamble ####
# Purpose: Build linear and generalized linear models to forecast U.S. presidential election outcomes
# Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data should be available at 'outputs/data/analysis_data.csv'
# Any other information needed? Ensure predictors like percentage, sample size, etc., are available in the data

#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
analysis_data <- read_csv("outputs/data/analysis_data.csv")

#### Prepare data for modeling ####
# Create a new variable to represent the target: the poll percentage as a continuous outcome
# Assuming 'percentage' is available in the dataset and represents the candidate's polling percentage
analysis_data <- analysis_data %>%
  mutate(
    poll_percentage = percentage / 100  # Convert to a proportion
  )

### Model 1: Linear regression model for poll percentage ####
linear_model <- stan_glm(
  formula = poll_percentage ~ sample_size + state + candidate,
  data = analysis_data,
  family = gaussian(),  # Linear regression for continuous outcome
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  seed = 853
)

#### Model 2: Generalized linear model for leading candidate prediction ####
# Create a binary outcome: 1 for leading candidate, 0 for others
analysis_data <- analysis_data %>%
  mutate(
    leading_candidate = if_else(candidate == "Leading Candidate", 1, 0)  # Replace "Leading Candidate" accordingly
  )

# Build a logistic regression model to predict the probability of being the leading candidate
glm_model <- stan_glm(
  formula = leading_candidate ~ poll_percentage + sample_size + state,
  data = analysis_data,
  family = binomial(link = "logit"),  # Logistic regression for binary outcome
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  seed = 853
)

#### Model summaries ####
print(summary(linear_model))
print(summary(glm_model))

#### Save models ####
saveRDS(
  linear_model,
  file = "models/linear_model.rds"
)

saveRDS(
  glm_model,
  file = "models/glm_model.rds"
)