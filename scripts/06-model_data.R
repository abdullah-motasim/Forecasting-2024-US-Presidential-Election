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

### Read data ###
analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

# Convert percentage to a proportion and ensure columns are in the correct data type
analysis_data <- analysis_data %>%
  mutate(
    poll_percentage = percentage / 100,  # Convert percentage to a proportion
    candidate_party = as.factor(party),
    pollster_name = as.factor(pollster_name),
    state = as.factor(state)
  )


# Fit a GLM model for poll percentage
glm_model <- stan_glm(
  formula = poll_percentage ~ sample_size + state + candidate_party,
  data = analysis_data,
  family = gaussian(),  # GLM for continuous outcome
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  seed = 853
)

# Model summary
print(summary(glm_model))

# Pollster reliability visualization
# Identify the top 5 most frequent pollsters
top_pollsters <- analysis_data %>%
  count(pollster_name, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(pollster_name)

# Filter data to include only the top 5 pollsters
top_pollster_data <- analysis_data %>%
  filter(pollster_name %in% top_pollsters)

# Enhanced visualization
ggplot(top_pollster_data, aes(x = pollster_name, y = numeric_grade)) +
  geom_boxplot(aes(fill = pollster_name), color = "black", width = 0.6, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(color = "darkgray", size = 1, alpha = 0.6, width = 0.2) +  # Add jittered points for individual grades
  labs(
    title = "Reliability Grades of Top 5 Pollsters by Frequency",
    subtitle = "Distribution of Numeric Grades for the Most Frequent Pollsters",
    x = "Pollster Name",
    y = "Numeric Grade"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Use a visually appealing color palette
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

# Filter data for Trump and Harris only
candidate_data <- analysis_data %>%
  filter(candidate %in% c("Donald Trump", "Kamala Harris"))

# Violin plot with overlaid boxplot for additional statistical information
ggplot(candidate_data, aes(x = candidate, y = percentage, fill = candidate)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "black") +  # Violin plot with no trimming
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +  # Boxplot overlay
  labs(
    title = "Distribution of Support Percentages for Trump and Harris",
    x = "Candidate",
    y = "Support Percentage"
  ) +
  scale_fill_manual(values = c("Donald Trump" = "red", "Kamala Harris" = "blue")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  )
