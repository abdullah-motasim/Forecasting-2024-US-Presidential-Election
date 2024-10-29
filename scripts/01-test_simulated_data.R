#### Preamble ####
# Purpose: Tests the structure and validity of the simulated American electoral divisions dataset.
#  Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run and saved the simulated data properly


#### Workspace setup ####
library(tidyverse)

# Load the simulated data for the American election
simulated_data <- read.csv("data/00-simulated_data/simulated_data.csv")

# Test if the data was successfully loaded
if (exists("simulated_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

#### Test data ####

# Check if the dataset has 1000 rows (as specified in the simulation)
if (nrow(simulated_data) == 1000) {
  message("Test Passed: The dataset has 1000 rows.")
} else {
  stop("Test Failed: The dataset does not have 1000 rows.")
}

# Check if the dataset has 5 columns
if (ncol(simulated_data) == 5) {
  message("Test Passed: The dataset has 5 columns.")
} else {
  stop("Test Failed: The dataset does not have 5 columns.")
}

# Check if the 'state' column contains only valid U.S. state names
valid_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
  "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
  "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
  "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
  "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
  "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
  "Wyoming"
)

if (all(simulated_data$state %in% valid_states)) {
  message("Test Passed: The 'state' column contains only valid U.S. state names.")
} else {
  stop("Test Failed: The 'state' column contains invalid state names.")
}

# Check if the 'party' column contains only valid party names
valid_parties <- c("Democrat", "Republican", "Independent", "Other")

if (all(simulated_data$party %in% valid_parties)) {
  message("Test Passed: The 'party' column contains only valid party names.")
} else {
  stop("Test Failed: The 'party' column contains invalid party names.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(simulated_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if there are no empty strings in all columns
if (all(simulated_data$poll_average != "" &simulated_data$state != "" & 
        simulated_data$party != "" & simulated_data$economic_factors != "" & 
        simulated_data$voter_turnout != "")) {
  message("Test Passed: There are no empty strings in any column.")
} else {
  stop("Test Failed: There are empty strings in one or more columns.")
}

# Check if the 'party' column has at least two unique values
if (n_distinct(simulated_data$party) >= 2) {
  message("Test Passed: The 'party' column contains at least two unique values.")
} else {
  stop("Test Failed: The 'party' column contains less than two unique values.")
}

# Check if 'poll_average' is within the expected range (40 to 60)
if (all(simulated_data$poll_average >= 40 & simulated_data$poll_average <= 60)) {
  message("Test Passed: All 'poll_average' values are within the expected range.")
} else {
  stop("Test Failed: 'poll_average' values are out of the expected range.")
}

# Check if 'voter_turnout' has reasonable values (e.g., between 0 and 100)
if (all(simulated_data$voter_turnout >= 0 & simulated_data$voter_turnout <= 100)) {
  message("Test Passed: All 'voter_turnout' values are within the expected range.")
} else {
  stop("Test Failed: 'voter_turnout' values are out of the expected range.")
}
