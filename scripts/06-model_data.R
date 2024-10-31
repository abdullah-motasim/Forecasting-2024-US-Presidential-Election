# #### Preamble ####
# # Purpose: Models the forecast for the 2024 U.S. presidential election using "poll-of-polls"
# # Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# # Date: 4 November 2024
# # Contact: elizabethh.luong@mail.utoronto.ca
# # License: MIT
# # Pre-requisites: Cleaned data should be available at 'outputs/data/analysis_data.csv'
# # Any other information needed? Data must include relevant predictors like percentage, sample size, state, etc.
# 
#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(bayesplot)

### Read data ###
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

#### Prepare data for modeling ####
# Create a binary outcome indicating if a candidate is leading
# Replace "Candidate A" with the actual leading candidate's name
analysis_data <- analysis_data %>%
  mutate(
    winner = if_else(candidate == "Kamala Harris", 1, 0)  # Adjust "Candidate A" as needed
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

# #### Model summary ####
print(summary(election_model))
# 
# #### Save model ####
# saveRDS(
#   election_model,
#   file = "models/election_model.rds"
# )
# 
# 
# #### Bayesian Framework and Probability Distributions ####
# # Prepare the data for modeling
# # Convert 'state' to a binary indicator: 1 if national, 0 if state-specific
# analysis_data_testing <- analysis_data %>%
#   mutate(
#     is_national = if_else(is.na(state), 1, 0),  # 1 for national poll, 0 for state-specific poll
#     pollster = as.factor(pollster)  # Convert pollster to factor
#   )
# 
# # Filter data for Harris (assuming 'candidate' column is available)
# harris_data <- analysis_data_testing %>%
#   filter(candidate == "Harris") %>%
#   select(pct, pollster, is_national)
# 
# # Bayesian hierarchical model for Harris's support
# harris_model <- stan_glm(
#   pct ~ pollster + is_national,
#   data = harris_data,
#   family = gaussian(),
#   prior = normal(0, 2.5, autoscale = TRUE),
#   prior_intercept = normal(0, 2.5, autoscale = TRUE),
#   seed = 853
# )
# 
# # Extract predicted values from the model
# predicted_harris <- posterior_predict(harris_model)
# 
# # Create a data frame of predicted values
# pred_data_harris <- harris_data %>%
#   mutate(pred_pct = colMeans(predicted_harris))
# 
# # Visualizations
# 
# # Extract MCMC draws for the model parameters
# mcmc_draws_harris <- as.matrix(harris_model)
# 
# # Select specific parameters for visualization
# param_labels_harris <- c(
#   "(Intercept)" = "Intercept",
#   "pollster" = "Pollster Effect",
#   "is_national" = "National Poll Effect"
# )
# 
# # Create posterior distribution plot
# mcmc_areas(
#   harris_model,
#   pars = names(param_labels_harris),
#   prob = 0.95  # 95% credible interval
# ) +
#   ggtitle("Posterior Distributions of Model Parameters for Harris Support") +
#   labs(
#     x = "Coefficient Value",
#     y = "Parameter"
#   ) +
#   scale_y_discrete(labels = param_labels_harris) +  # Apply custom labels
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 12),
#     panel.grid.major = element_line(color = "gray", linetype = "dotted")
#   ) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8)  # Reference line at 0
# 
# # Plot predicted Harris support by poll type
# ggplot(pred_data_harris, aes(x = as.factor(is_national), y = pred_pct, fill = as.factor(is_national))) +
#   geom_boxplot(alpha = 0.7) +
#   scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue"), labels = c("State-Specific", "National")) +
#   labs(
#     title = "Predicted Support for Harris by Poll Type",
#     x = "Poll Type",
#     y = "Predicted Support (Percentage)",
#     fill = "Poll Type"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text.x = element_text(size = 12),
#     axis.text.y = element_text(size = 12),
#     legend.position = "right"
#   )
# 
# # Plot predicted Harris support by pollster
# ggplot(pred_data_harris, aes(x = pollster, y = pred_pct, fill = pollster)) +
#   geom_boxplot(alpha = 0.7) +
#   labs(
#     title = "Predicted Support for Harris by Pollster",
#     x = "Pollster",
#     y = "Predicted Support (Percentage)",
#     fill = "Pollster"
#   ) +
#   scale_fill_viridis_d() +  # Use a visually distinct color palette
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
#     axis.text.y = element_text(size = 12),
#     legend.position = "none"
#   )



#### Set-up ####
# Load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)


#### Prepare dataset ####
# Read in the data and clean variable names
data <- read_csv("data/01-raw_data/president_polls.csv") |>
  clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= 2.7 # Need to investigate this choice - come back and fix. 
    # Also need to look at whether the pollster has multiple polls or just one or two - filter out later
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state), # Hacky fix for national polls - come back and check
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21")) |> # When Harris declared
  mutate(
    num_harris = round((pct / 100) * sample_size, 0) # Need number not percent for some models
  )


#### Plot data ####
base_plot <- ggplot(just_harris_high_quality, aes(x = end_date, y = pct)) +
  theme_classic() +
  labs(y = "Harris percent", x = "Date")

# Plots poll estimates and overall smoothing
base_plot +
  geom_point() +
  geom_smooth()

# Color by pollster
# This gets messy - need to add a filter - see line 21
base_plot +
  geom_point(aes(color = pollster)) +
  geom_smooth() +
  theme(legend.position = "bottom")

# Facet by pollster
# Make the line 21 issue obvious
# Also - is there duplication???? Need to go back and check
base_plot +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(pollster))

# Color by pollscore
base_plot +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth() +
  theme(legend.position = "bottom")


#### Starter models ####
# Model 1: pct as a function of end_date
model_date <- lm(pct ~ end_date, data = just_harris_high_quality)

# Model 2: pct as a function of end_date and pollster
model_date_pollster <- lm(pct ~ end_date + pollster, data = just_harris_high_quality)

# Augment data with model predictions
just_harris_high_quality <- just_harris_high_quality |>
  mutate(
    fitted_date = predict(model_date),
    fitted_date_pollster = predict(model_date_pollster)
  )

# Plot model predictions
# Model 1
ggplot(just_harris_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date), color = "blue", linetype = "dotted") +
  theme_classic() +
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date")

# Model 2
ggplot(just_harris_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date_pollster), color = "blue", linetype = "dotted") +
  facet_wrap(vars(pollster)) +
  theme_classic() +
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date + pollster")

# All of the above would be in scripts - data cleaning scripts and modelling scripts. 
# This is an example of how you get a results table that you could put into your Quarto doc
modelsummary(models = list("Model 1" = model_date, "Model 2" = model_date_pollster))


#### Bayesian models ####
# Change 'pollster' and 'state' to factor variables
just_harris_high_quality <- just_harris_high_quality |>
  mutate(
    pollster = factor(pollster),
    state = factor(state)
  )

# Model 1
model_formula_1 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster)

# Model 2
model_formula_2 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster) + (1 | state)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit the models
bayesian_model_1 <- stan_glmer(
  formula = model_formula_1,
  data = just_harris_high_quality,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

bayesian_model_2 <- stan_glmer(
  formula = model_formula_2,
  data = just_harris_high_quality,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)
# Note the warnings here and follow the instructions for dealing with it - come back and fix.

# Posterior predictive checks
pp_check(bayesian_model_1)
pp_check(bayesian_model_2)

# Summarize the model
summary(bayesian_model_1)
summary(bayesian_model_2)

# Plot random effects
plot(bayesian_model_1, pars = "(Intercept)", prob = 0.95)
plot(bayesian_model_2, pars = "(Intercept)", prob = 0.95)

# Model summary works the same as above for Bayesian models.


#### Bayesian models and splines ####
# Change date to be number of days since she declared - it's a counter not a date
just_harris_high_quality <- just_harris_high_quality |>
  mutate(
    end_date_num = as.numeric(end_date - min(end_date))
  )

# Fit Bayesian model with spline and pollster as fixed effect
# cf bayesian_model_1 and bayesian_model_2 where it's a random effect - note the different interpretations
spline_model <- stan_glm(
  pct ~ ns(end_date_num, df = 5) + pollster, # Change df for the number of "bits" - higher numbers - more "wiggly" - but then need to worry about overfitting.
  data = just_harris_high_quality,
  family = gaussian(),
  prior = normal(0, 5),
  prior_intercept = normal(50, 10),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0
)

# Summarize the model
summary(spline_model)

# Posterior predictive checks
pp_check(spline_model)

# Predict and plot
# Create new data for prediction
new_data <- data.frame(
  end_date_num = seq(
    min(just_harris_high_quality$end_date_num),
    max(just_harris_high_quality$end_date_num),
    length.out = 100
  ),
  pollster = factor("YouGov", levels = levels(just_harris_high_quality$pollster))
)

# Predict posterior draws
posterior_preds <- posterior_predict(spline_model, newdata = new_data)

# Summarize predictions
pred_summary <- new_data |>
  mutate(
    pred_mean = colMeans(posterior_preds),
    pred_lower = apply(posterior_preds, 2, quantile, probs = 0.025),
    pred_upper = apply(posterior_preds, 2, quantile, probs = 0.975)
  )

# Plot the spline fit
ggplot(just_harris_high_quality, aes(x = end_date_num, y = pct, color = pollster)) +
  geom_point() +
  geom_line(
    data = pred_summary,
    aes(x = end_date_num, y = pred_mean),
    color = "blue",
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = pred_summary,
    aes(x = end_date_num, ymin = pred_lower, ymax = pred_upper),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Days since earliest poll",
    y = "Percentage",
    title = "Poll Percentage over Time with Spline Fit"
  ) +
  theme_minimal()

# Obviously this is just quick and dirty code but I hope it helps you with getting started. 
