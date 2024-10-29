#### Preamble ####
# Purpose: Tests the cleaned U.S. presidential polls dataset for correctness and consistency
# Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data file should exist at 'data/02-analysis_data/analysis_data.csv'
# Any other information needed? Ensure 'testthat' and 'tidyverse' libraries are installed

#### Workspace setup ####
library(tidyverse)
library(testthat)

# Load the cleaned data
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

#### Test data ####

# Test that the dataset has at least 1000 rows (as per simulated data)
test_that("dataset has at least 1000 rows", {
  expect_true(nrow(analysis_data) >= 1000)
})

# Test that the dataset has 5 columns (based on cleaned dataset structure)
test_that("dataset has 6 columns", {
  expect_equal(ncol(analysis_data), 6)
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

# Test that there are no empty strings in 'pollster_name', 'candidate', 'percentage', 'sample_size', or 'state'
test_that("no empty strings in critical columns", {
  expect_false(any(analysis_data$pollster_name == "" | 
                     analysis_data$candidate == "" | 
                     analysis_data$percentage == "" | 
                     analysis_data$sample_size == "" | 
                     analysis_data$state == ""))
})

# Test that the 'candidate' column contains at least 2 unique values
test_that("'candidate' column contains at least 2 unique values", {
  expect_true(length(unique(analysis_data$candidate)) >= 2)
})