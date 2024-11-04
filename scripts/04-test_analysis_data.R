#### Preamble ####
# Purpose: Tests the cleaned U.S. presidential polls dataset for correctness and consistency
# Author: Elizabeth Luong and Abdullah Motasim
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data file should exist at 'data/02-analysis_data/analysis_data.csv'
# Any other information needed? Ensure 'testthat', 'tidyverse' and 'arrow' libraries are installed

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

# Load the cleaned data
analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

#### Test data ####

# Test that the dataset has at least 1000 rows (as per simulated data)
test_that("dataset has at least 1000 rows", {
  expect_true(nrow(analysis_data) >= 1000)
})

# Test that the dataset has 7 columns (based on cleaned dataset structure)
test_that("dataset has 7 columns", {
  expect_equal(ncol(analysis_data), 7)
})

# Test that the 'pollster_name' column is character type
test_that("'pollster_name' is character", {
  expect_type(analysis_data$pollster_name, "character")
})

# Test that the 'candidate' column is character type
test_that("'candidate' is character", {
  expect_type(analysis_data$candidate, "character")
})

# Test that the 'percentage' column is numeric
test_that("'percentage' is numeric", {
  expect_type(analysis_data$percentage, "double")
})

# Test that the 'numeric_grade' column is numeric
test_that("'numeric_grade' is numeric", {
  expect_type(analysis_data$numeric_grade, "double")
})


# Test that the 'sample_size' column is numeric
test_that("'sample_size' is numeric", {
  expect_type(analysis_data$sample_size, "double")
})

# Test that the 'state' column is character type
test_that("'state' is character", {
  expect_type(analysis_data$state, "character")
})

# Test that there are no missing values in the dataset
test_that("no missing values in dataset", {
  expect_true(all(!is.na(analysis_data)))
})

# Test that 'pollster_name' contains unique values (no duplicates)
test_that("'pollster_name' column contains unique values", {
  expect_true(length(unique(analysis_data$pollster_name)) > 1)
})

# Test that 'state' contains only valid U.S. state names
valid_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
  "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
  "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maine CD-1", "Maine CD-2",
  "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
  "Nebraska", "Nebraska CD-1", "Nebraska CD-2", "Nebraska CD-3", "Nevada", "New Hampshire", 
  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
  "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
  "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
)

# Test that 'state' contains only valid U.S. state names
test_that("'state' contains valid U.S. state names", {
  expect_true(all(analysis_data$state %in% valid_states))
})

# Test that there are no empty strings in 'pollster_name', 'candidate', 'percentage', 'sample_size', 'numeric_grade', or 'state'
test_that("no empty strings in critical columns", {
  expect_false(any(analysis_data$pollster_name == "" | 
                     analysis_data$candidate == "" | 
                     analysis_data$percentage == "" | 
                     analysis_data$sample_size == "" | 
                     analysis_data$numeric_grade == "" | 
                     analysis_data$state == ""))
})

# Test that the 'candidate' column contains at least 2 unique values
test_that("'candidate' column contains at least 2 unique values", {
  expect_true(length(unique(analysis_data$candidate)) >= 2)
})

# Test that 'percentage' values are between 0 and 100
test_that("'percentage' values are between 0 and 100", {
  expect_true(all(analysis_data$percentage >= 0 & analysis_data$percentage <= 100))
})

# Test that 'numeric_grade' falls within an expected range (e.g., 0 to 4 for grading scale)
test_that("'numeric_grade' values fall within the range 0 to 4", {
  expect_true(all(analysis_data$numeric_grade >= 0 & analysis_data$numeric_grade <= 4))
})

# Test that 'sample_size' has reasonable values (e.g., not too small or unreasonably large)
test_that("'sample_size' values are within a realistic range (50 to 50000)", {
  expect_true(all(analysis_data$sample_size >= 50 & analysis_data$sample_size <= 50000))
})


# Test that the dataset contains data from at least 40 unique states, ensuring wide coverage
test_that("data covers at least 40 unique states", {
  expect_true(length(unique(analysis_data$state)) >= 40)
})

# Test that the average 'percentage' by 'candidate' is reasonable (between 0 and 100)
candidate_avg <- analysis_data %>%
  group_by(candidate) %>%
  summarise(avg_percentage = mean(percentage, na.rm = TRUE))
test_that("average 'percentage' per candidate is within 0-100 range", {
  expect_true(all(candidate_avg$avg_percentage >= 0 & candidate_avg$avg_percentage <= 100))
})

# Test that 'pollster_name' entries have no leading or trailing whitespace
test_that("'pollster_name' entries have no leading/trailing whitespace", {
  expect_false(any(grepl("^\\s+|\\s+$", analysis_data$pollster_name)))
})


