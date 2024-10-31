#### Preamble ####
# Purpose: Models the forecast for the 2024 U.S. presidential election using "poll-of-polls"
# Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data should be available at 'data/02-analysis_data/analysis_data.csv'
# Any other information needed? Data must include relevant predictors like percentage, sample size, state, etc.

#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

#### Prepare data for modeling ####
# Create a binary outcome indicating if a candidate is leading
# Replace "Candidate A" with the actual leading candidate's name
analysis_data <- analysis_data %>%
  mutate(
    winner = if_else(candidate == "Candidate A", 1, 0)  # Adjust "Candidate A" as needed
  )

### Model data ####
# Build a logistic regression model for the probability of winning
election_model <- stan_glm(
  formula = winner ~ percentage + sample_size + state,
  data = analysis_data,
  family = binomial(link = "logit"),  # Logistic regression for binary outcomes
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  seed = 853
)

#### Model summary ####
print(summary(election_model))

#### Save model ####
saveRDS(
  election_model,
  file = "models/election_model.rds"
)

