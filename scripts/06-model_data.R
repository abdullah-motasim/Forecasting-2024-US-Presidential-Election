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

### Read data ###
analysis_data <- read_csv("outputs/data/analysis_data.csv")

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


#### Bayesian Framework and Probability Distributions ####
# Prepare the data for modeling
# Convert 'state' to a binary indicator: 1 if national, 0 if state-specific
analysis_data_testing <- analysis_data %>%
  mutate(
    is_national = if_else(is.na(state), 1, 0),  # 1 for national poll, 0 for state-specific poll
    pollster = as.factor(pollster)  # Convert pollster to factor
  )

# Filter data for Harris (assuming 'candidate' column is available)
harris_data <- analysis_data_testing %>%
  filter(candidate == "Harris") %>%
  select(pct, pollster, is_national)

# Bayesian hierarchical model for Harris's support
harris_model <- stan_glm(
  pct ~ pollster + is_national,
  data = harris_data,
  family = gaussian(),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  seed = 853
)

# Extract predicted values from the model
predicted_harris <- posterior_predict(harris_model)

# Create a data frame of predicted values
pred_data_harris <- harris_data %>%
  mutate(pred_pct = colMeans(predicted_harris))

# Visualizations

# Extract MCMC draws for the model parameters
mcmc_draws_harris <- as.matrix(harris_model)

# Select specific parameters for visualization
param_labels_harris <- c(
  "(Intercept)" = "Intercept",
  "pollster" = "Pollster Effect",
  "is_national" = "National Poll Effect"
)

# Create posterior distribution plot
mcmc_areas(
  harris_model,
  pars = names(param_labels_harris),
  prob = 0.95  # 95% credible interval
) +
  ggtitle("Posterior Distributions of Model Parameters for Harris Support") +
  labs(
    x = "Coefficient Value",
    y = "Parameter"
  ) +
  scale_y_discrete(labels = param_labels_harris) +  # Apply custom labels
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major = element_line(color = "gray", linetype = "dotted")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8)  # Reference line at 0

# Plot predicted Harris support by poll type
ggplot(pred_data_harris, aes(x = as.factor(is_national), y = pred_pct, fill = as.factor(is_national))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), labels = c("State-Specific", "National")) +
  labs(
    title = "Predicted Support for Harris by Poll Type",
    x = "Poll Type",
    y = "Predicted Support (Percentage)",
    fill = "Poll Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right"
  )

# Plot predicted Harris support by pollster
ggplot(pred_data_harris, aes(x = pollster, y = pred_pct, fill = pollster)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Predicted Support for Harris by Pollster",
    x = "Pollster",
    y = "Predicted Support (Percentage)",
    fill = "Pollster"
  ) +
  scale_fill_viridis_d() +  # Use a visually distinct color palette
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

