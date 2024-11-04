#### Preamble ####
# Purpose: Build linear and generalized linear models to forecast U.S. presidential election outcomes
# Author: Elizabeth Luong and Abdullah Motasim
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data should be available at 'outputs/data/analysis_data.csv'
# Any other information needed? Ensure predictors like percentage, sample size, etc., are available in the data

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(gt)
library(arrow)
library(kableExtra)

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

#### Prepare data for modeling ####
# Convert percentage to a proportion and ensure columns are in the correct data type
analysis_data <- analysis_data %>% 
  mutate(
    poll_percentage = percentage / 100, # Convert percentage to a proportion
    candidate_party = as.factor(party),
    pollster_name = as.factor(pollster_name),
    state = as.factor(state)
  )

# Fit a Generalize Linear Model for poll percentage
glm_model <- stan_glm(
  formula =  poll_percentage ~ sample_size + state +candidate,
  data = analysis_data,
  family = gaussian(),  # GLM for continuous outcome
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  seed = 853
)

#### Model summaries ####
print(summary(glm_model))

saveRDS(
  glm_model,
  file = "models/glm_model.rds"
)

# Calculate summary statistics for numeric variables
numeric_summary <- analysis_data %>%
  summarise(
    `Mean of Numeric Grade` = mean(numeric_grade, na.rm = TRUE),
    `Median of Numeric Grade` = median(numeric_grade, na.rm = TRUE),
    `SD of Numeric Grade` = sd(numeric_grade, na.rm = TRUE),
    `Mean of Percentage` = mean(percentage, na.rm = TRUE),
    `Median of Percentage` = median(percentage, na.rm = TRUE),
    `SD of Percentage` = sd(percentage, na.rm = TRUE),
    `Mean of Sample Size` = mean(sample_size, na.rm = TRUE),
    `Median of Sample Size` = median(sample_size, na.rm = TRUE),
    `SD of Sample Size` = sd(sample_size, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
  mutate(Value = round(Value, 2))  # Format the values to 2 decimal places

# Display the numeric summary table using kable
numeric_summary %>%
  kable("html", caption = "Summary Statistics for Numeric Variables") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")


# Filter data for Harris and Trump
harris_data <- analysis_data %>%
  filter(candidate == "Kamala Harris")

trump_data <- analysis_data %>%
  filter(candidate == "Donald Trump")

# Define a function to compute summary statistics
compute_summary_stats <- function(data) {
  data %>%
    summarise(
      Mean = mean(percentage, na.rm = TRUE),
      Median = median(percentage, na.rm = TRUE),
      SD = sd(percentage, na.rm = TRUE),
      Min = min(percentage, na.rm = TRUE),
      Max = max(percentage, na.rm = TRUE),
      Sample_Size_Mean = mean(sample_size, na.rm = TRUE),
      Sample_Size_SD = sd(sample_size, na.rm = TRUE)
    )
}

# Summary statistics for Harris
harris_summary <- compute_summary_stats(harris_data) %>%
  mutate(Candidate = "Kamala Harris")

# Summary statistics for Trump
trump_summary <- compute_summary_stats(trump_data) %>%
  mutate(Candidate = "Donald Trump")

# Combine the two tables and format values to 2 decimal places
summary_table <- bind_rows(harris_summary, trump_summary) %>%
  select(Candidate, everything()) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))  # Round numeric columns to 2 decimal places

# Rename columns in the summary table
summary_table <- summary_table %>%
  rename(
    `Sample Size Mean` = Sample_Size_Mean,
    `Sample Size SD` = Sample_Size_SD
  )

# Create a visual table using kable
summary_table %>%
  kable("html", caption = "Summary Statistics for Key Variables Comparing Kamala Harris and Donald Trump") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center") %>%
  add_header_above(c(" " = 1, "Percentage Statistics" = 5, "Sample Size Statistics" = 2)) %>%
  column_spec(2:8, width = "2em")  # Adjust column width if necessary

# Pollster reliability visualization
# Identifying the 5 most frequent pollsters for readability 
top_pollsters <- analysis_data %>%
  count(pollster_name, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(pollster_name)

# Filter data to include only the top 5 pollsters
top_pollsters_data <- analysis_data %>%
  filter(pollster_name %in% top_pollsters)

# Visualize numeric grade fort he 5 top pollsters
ggplot(top_pollsters_data, aes(x = pollster_name, y = numeric_grade)) +
  geom_boxplot(aes(fill = pollster_name), color = "black", width = 0.6, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(color = "skyblue", size = 1, alpha = 0.6, width = 0.2) +  # Add jittered points for individual grades
  labs(
    title = "Reliability Grades of Top 5 Pollsters by Frequency",
    subtitle = "Distribution of Numeric Grades for the Most Frequent Pollsters",
    x = "Pollster Name",
    y = "Numeric Grade",
    caption = "Fig 1: Each boxplot shows the distribution of numeric reliability grades for the top 5 pollsters, with individual poll grades represented by jittered points."
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
    legend.position = "none",
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 15))  # Adjust caption to fit better
  )

# Shows distribution of the range, outlines, and multiple categories


# Filter data for Trump and Harris only
candidate_data <- analysis_data %>%
  filter(candidate %in% c("Donald Trump", "Kamala Harris"))


# Violin plot with overlaid box plot for additional statistical information
ggplot(candidate_data, aes(x = candidate, y = percentage, fill = candidate)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "black") +  # Violin plot with no trimming
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +  # Box plot overlay
  labs(
    title = "Distribution of Support Percentages for Trump and Harris",
    x = "Candidate",
    y = "Support Percentage",
    caption = "Fig 2: The violin plot shows the distribution and density of support percentages for each candidate, with an overlaid box plot highlighting \n                                                    the median, interquartile range, and approximate outliers."
  ) +
  scale_fill_manual(values = c("Donald Trump" = "red", "Kamala Harris" = "blue")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    plot.caption = element_text(size = 8, hjust = 0)  # Adjust the caption styling here
  )


# Prepare the data by grouping and summarizing sample size per state
state_sample_data <- analysis_data %>%
  group_by(state) %>%
  summarize(total_sample_size = sum(sample_size, na.rm = TRUE)) %>%
  arrange(desc(total_sample_size))  # Order by sample size for better readability

# Horizontal bar plot for sample size by state
ggplot(state_sample_data, aes(x = reorder(state, total_sample_size), y = total_sample_size)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
  coord_flip() +  # Flip coordinates for a horizontal bar plot
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Sample Size by State",
    x = "State",
    y = "Sample Size",
    caption = "Fig 3: This chart displays the total sample size per state for survey data, ordered from highest to lowest sample size."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 7, hjust = 1),  # Adjust size for readability
    axis.text.x = element_text(size = 12),
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10))
  )

