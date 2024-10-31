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
library(gt)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

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

# Numeric summary table
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
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")

# Display numeric summary table using gt
numeric_summary %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics for Numeric Variables"
  ) %>%
  fmt_number(
    columns = vars(Value),
    decimals = 2
  ) %>%
  cols_label(
    Statistic = "Statistic",
    Value = "Value"
  )

# Frequency table for categorical variables
categorical_summary <- data.frame(
  Variable = c("State", "Party", "Candidate"),
  Count = c(length(unique(analysis_data$state)),
            length(unique(analysis_data$party)),
            length(unique(analysis_data$candidate)))
)

# Display categorical summary table using gt
categorical_summary %>%
  gt() %>%
  tab_header(
    title = "Frequency of Categorical Variables"
  ) %>%
  cols_label(
    Variable = "Variable",
    Count = "Count"
  )


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
  mutate(Candidate = "Kamala Harris")  # Add a column for candidate name

# Summary statistics for Trump
trump_summary <- compute_summary_stats(trump_data) %>%
  mutate(Candidate = "Donald Trump")  # Add a column for candidate name

# Combine the two tables
summary_table <- bind_rows(harris_summary, trump_summary)

# Create a visual table using gt
summary_table %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics for Key Variables",
    subtitle = "Comparing Kamala Harris and Donald Trump"
  ) %>%
  cols_label(
    Candidate = "Candidate",
    Mean = "Mean Percentage",
    Median = "Median Percentage",
    SD = "Standard Deviation (Percentage)",
    Min = "Minimum Percentage",
    Max = "Maximum Percentage",
    Sample_Size_Mean = "Mean Sample Size",
    Sample_Size_SD = "Sample Size SD"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 18,
    heading.subtitle.font.size = 16
  )

























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

# Shows distribution of the range, outliers, and mulitple categories


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
    y = "Sample Size"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

