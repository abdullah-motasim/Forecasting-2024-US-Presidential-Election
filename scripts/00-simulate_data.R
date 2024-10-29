#### Preamble ####
# Purpose: Simulates a dataset of American electoral divisions, including the state and party that won each division.
# Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `dplyr` package must be installed


#### Workspace setup ####
library(dplyr)
set.seed(853)


#### Simulate data ####
# State names
states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
  "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
  "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
  "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
  "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
  "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
  "Wyoming"
)

# Political parties
parties <- c("Democrat", "Republican", "Independent", "Other")

# Define probabilities for state distribution (equal probability for simplicity)
state_probs <- rep(1/51, 51)

# Define probabilities for party distribution
party_probs <- c(0.45, 0.45, 0.05, 0.05)

# Generate poll averages (e.g., support percentages)
poll_averages <- runif(1000, min = 40, max = 60)  # Simulate polling between 40% and 60%

# Generate state and party affiliations
states_sampled <- sample(states, size = 1000, replace = TRUE, prob = state_probs)
parties_sampled <- sample(parties, size = 1000, replace = TRUE, prob = party_probs)

# Generate other predictors (e.g., economic factors, voter turnout)
economic_factors <- rnorm(1000, mean = 0, sd = 1)  # Economic index (e.g., GDP growth)
voter_turnout <- rnorm(1000, mean = 60, sd = 5)    # Voter turnout in percentage

# Create a data frame
simulated_data <- tibble(
  poll_average = poll_averages,
  state = states_sampled,
  party = parties_sampled,
  economic_factors = economic_factors,
  voter_turnout = voter_turnout
)

# Display first few rows of the data
print(head(simulated_data))

# Save data as CSV
write.csv(simulated_data, "data/00-simulated_data/simulated_US_election_data.csv", row.names=FALSE)
