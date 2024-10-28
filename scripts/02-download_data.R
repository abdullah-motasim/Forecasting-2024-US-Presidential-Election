#### Preamble ####
# Purpose: Downloads and saves the data from Presidential general election polls (current cycle)
# Website: https://projects.fivethirtyeight.com/polls/president-general/2024/national/
# Author: Elizabeth Luong, Abdullah Motasim, and Yuanting Han
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Ensure the CSV file is available and accessible.
# Any other information needed? None.


#### Workspace setup ####
library(tidyverse)

# Set a relative path to make it reproducible
project_dir <- getwd()  
output_dir <- file.path(project_dir, "inputs", "data")

#### Load data ####
# Adjust the file path to match the uploaded file location
input_file <- "C:/Users/eliza/Downloads/president_polls.csv"

# Load the data from the provided CSV file
if (file.exists(input_file)) {
  president_polls <- read_csv(input_file)
  message("Data successfully loaded from the input file.")
} else {
  stop("Input file not found. Please ensure the file exists in the specified location.")
}

#### Create directory if it doesn't exist ####
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Output directory created.")
} else {
  message("Output directory already exists.")
}

#### Save data ####
# Define the output file path
output_file <- file.path(output_dir, "president_poll.csv")

# Save the data to a new file location
write_csv(president_polls, output_file)

# Test if the data was successfully saved
if (file.exists(output_file)) {
  message("Test Passed: The dataset was successfully saved to the output directory.")
} else {
  stop("Test Failed: The dataset could not be saved.")
}
