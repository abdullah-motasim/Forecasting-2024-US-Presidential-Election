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
library(bayesplot)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

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

# Check parameter names for the linear model
linear_model_params <- names(linear_model$coefficients)
print(linear_model_params)

# Check parameter names for the GLM model
glm_model_params <- names(glm_model$coefficients)
print(glm_model_params)

# Visualizations for the Linear Model's EDA

# Extract MCMC draws into a data frame
mcmc_draws <- as.data.frame(as.matrix(linear_model))

# Select a limited number of parameters to improve clarity in the plot
selected_params <- c("(Intercept)", "sample_size", "stateAlaska", "stateArizona", "stateArkansas")
mcmc_draws_selected <- mcmc_draws %>%
  select(all_of(selected_params)) %>%
  rename(
    Intercept = "(Intercept)",
    `Sample Size` = "sample_size",
    `State: Alaska` = "stateAlaska",
    `State: Arizona` = "stateArizona",
    `State: Arkansas` = "stateArkansas"
  )

# Convert to long format for plotting with ggplot2
mcmc_draws_long <- mcmc_draws_selected %>%
  mutate(iteration = 1:nrow(mcmc_draws_selected)) %>%
  pivot_longer(-iteration, names_to = "Parameter", values_to = "Value")

# Trace plot using ggplot2
ggplot(mcmc_draws_long, aes(x = iteration, y = Value, color = Parameter)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(0, 4000, by = 500), labels = seq(0, 4000, by = 500)) +
  labs(
    title = "Trace Plot for Selected Linear Model Parameters",
    subtitle = "Convergence Diagnostics for MCMC Chains",
    x = "Iterations",
    y = "Parameter Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12)
  )

# Posterior distributions for selected parameters in the linear model
mcmc_areas(
  linear_model,
  pars = linear_model_params[1:5],  # Adjust as needed based on parameters available
  prob = 0.95
) +
  ggtitle("Posterior Distributions of Linear Model Coefficients")


# Create the posterior distributions plot
mcmc_areas(
  linear_model,
  pars = selected_params,
  prob = 0.95  # 95% credible intervals
) +
  ggtitle("Posterior Distributions of Linear Model Coefficients") +
  labs(
    x = "Coefficient Value",
    y = "Parameter"
  ) +
  scale_y_discrete(labels = selected_params) +  # Apply custom labels
  theme_minimal(base_size = 14) +  # Minimal theme for a cleaner look
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major = element_line(color = "gray", linetype = "dotted")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8)  # Add a reference line at 0

# Residuals vs Fitted values plot for the linear model
# Extract residuals and fitted values from the linear model
residuals <- residuals(linear_model)
fitted_vals <- fitted(linear_model)

# Create the residuals vs fitted values plot with enhanced aesthetics
ggplot(data = data.frame(Fitted = fitted_vals, Residuals = residuals), 
       aes(x = Fitted, y = Residuals)) +
  geom_point(aes(color = abs(Residuals)), alpha = 0.6, size = 2) +  # Color gradient based on residual size
  geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dotted", size = 1) +  # Add a loess smoother
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.8) +  # Reference line at 0
  scale_color_gradient(low = "lightblue", high = "darkblue") +  # Gradient from light to dark blue
  labs(
    title = "Residuals vs Fitted Values (Linear Model)",
    subtitle = "Assessing Homoscedasticity and Linear Fit",
    x = "Fitted Values",
    y = "Residuals",
    color = "Residual Size"
  ) +
  theme_minimal(base_size = 14) +  # Use minimal theme for a cleaner look
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray", linetype = "dotted")
  )


# Visualizations for Generalized Linear Model's EDA

# Extract MCMC draws into a data frame
mcmc_draws_glm <- as.data.frame(as.matrix(glm_model))

# Select specific parameters and rename them using custom labels
selected_params_glm <- c("(Intercept)", "sample_size", "stateAlaska", "stateArizona", "stateArkansas")
mcmc_draws_selected_glm <- mcmc_draws_glm %>%
  select(all_of(selected_params_glm)) %>%
  rename(
    Intercept = "(Intercept)",
    `Sample Size` = "sample_size",
    `State: Alaska` = "stateAlaska",
    `State: Arizona` = "stateArizona",
    `State: Arkansas` = "stateArkansas"
  )

# Convert to long format for plotting with ggplot2
mcmc_draws_long_glm <- mcmc_draws_selected_glm %>%
  mutate(iteration = 1:nrow(mcmc_draws_selected_glm)) %>%
  pivot_longer(-iteration, names_to = "Parameter", values_to = "Value")

# Trace plot using ggplot2
ggplot(mcmc_draws_long_glm, aes(x = iteration, y = Value, color = Parameter)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 2) +
  labs(
    title = "Trace Plot for Selected GLM Model Parameters",
    subtitle = "Convergence Diagnostics for MCMC Chains",
    x = "Iterations",
    y = "Parameter Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12)
  )

# Create the posterior distributions plot for the GLM model
mcmc_areas(
  glm_model,
  pars = selected_params_glm,  # Use the selected parameters
  prob = 0.95
) +
  ggtitle("Posterior Distributions of GLM Model Coefficients") +
  labs(
    x = "Coefficient Value",
    y = "Parameter"
  ) +
  scale_y_discrete(labels = selected_params_glm) +  # Apply custom labels
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major = element_line(color = "gray", linetype = "dotted")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8)  # Add a reference line at 0

# Generate predicted probabilities for the logistic regression model
predicted_probs <- posterior_predict(glm_model, type = "response")

# Ensure predicted probabilities are between 0 and 1
predicted_probs <- ifelse(predicted_probs < 0, 0, ifelse(predicted_probs > 1, 1, predicted_probs))

# Create a data frame of predictions
pred_data <- analysis_data %>%
  mutate(predicted_probs = colMeans(predicted_probs))

# Histogram of predicted probabilities
ggplot(pred_data, aes(x = predicted_probs, fill = factor(leading_candidate))) +
  geom_histogram(binwidth = 0.05, position = "dodge", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Predicted Probabilities",
    subtitle = "Distribution of GLM Model Predictions",
    x = "Predicted Probability",
    y = "Frequency",
    fill = "Actual Outcome"
  ) +
  scale_x_continuous(limits = c(0, 1)) +  # Limit x-axis to probability range
  scale_fill_manual(values = c("1" = "darkred", "0" = "darkblue")) +  # Use contrasting colors
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right"
  )









