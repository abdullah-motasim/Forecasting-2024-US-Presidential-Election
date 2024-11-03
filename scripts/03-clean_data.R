#### Preamble ####
# Purpose: Cleans the raw U.S. presidential poll data for analysis.
#          This script standardizes column names, selects relevant columns,
#          converts data types, and handles missing values.
# Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The raw poll data should be available at 'inputs/data/president_poll.csv'.
#                 Ensure 'tidyverse' and 'janitor' packages are installed.

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Clean data ####
# Load raw data from the specified path
raw_data <- read_csv("data/01-raw_data/president_polls.csv")

# Clean the data
cleaned_data <- 
  raw_data |>
  janitor::clean_names() |>  # Standardize column names to snake_case
  select(pollster, candidate_name, pct, party, sample_size, state, numeric_grade) |>  # Select relevant columns
  filter(!is.na(pct)) |>  # Remove rows where the percentage column is missing
  mutate(
    pct = as.numeric(gsub(",", ".", pct)),  # Convert percentage to numeric, handling commas
    sample_size = as.numeric(sample_size)   # Convert sample size to numeric
  ) |> 
  rename(
    pollster_name = pollster,
    candidate = candidate_name,
    percentage = pct
  ) |> 
  drop_na()  # Remove any remaining rows with NA values

#### Save data ####
# Save the cleaned data to a new file location within the project
write.csv(cleaned_data, "data/02-analysis_data/analysis_data.csv", row.names=FALSE)

# Confirm successful cleaning and saving
if (file.exists("data/02-analysis_data/analysis_data.csv")) {
  message("Test Passed: The cleaned dataset was saved successfully.")
} else {
  stop("Test Failed: The cleaned dataset could not be saved.")
}

