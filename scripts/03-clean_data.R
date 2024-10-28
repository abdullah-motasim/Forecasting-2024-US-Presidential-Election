#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Elizabeth Luong, Abdullah Motasim,and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Clean data ####
# Load raw data from the specified path
raw_data <- read_csv("inputs/data/president_poll.csv")

# Clean the data
cleaned_data <- 
  raw_data |>
  janitor::clean_names() |>  # Standardize column names to snake_case
  select(pollster, candidate_name, pct, party, sample_size, state) |>  # Select relevant columns
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
# Save the cleaned data to the specified output location
output_file <- "outputs/data/analysis_data.csv"
if (!dir.exists(dirname(output_file))) {
  dir.create(dirname(output_file), recursive = TRUE)
}

write_csv(cleaned_data, output_file)

# Confirm successful cleaning and saving
if (file.exists(output_file)) {
  message("Test Passed: The cleaned dataset was successfully saved to the output directory.")
} else {
  stop("Test Failed: The cleaned dataset could not be saved.")
}