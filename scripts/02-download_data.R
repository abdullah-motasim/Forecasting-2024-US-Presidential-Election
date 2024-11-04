#### Preamble ####
# Purpose: Downloads and saves the data from Presidential general election polls (current cycle)
# Website: https://projects.fivethirtyeight.com/polls/president-general/2024/national/
# Author: Elizabeth Luong and Abdullah Motasim
# Date: 4 November 2024
# Contact: elizabethh.luong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)

#### Load data ####
# Read data directly from weblink
president_polls <- read_csv("https://projects.fivethirtyeight.com/polls/data/president_polls.csv")


#### Save data ####
# Save the raw data to a new file location within the project
write.csv(president_polls, "data/01-raw_data/president_polls.csv", row.names=FALSE)

# Test if the data was successfully saved
if (file.exists("data/01-raw_data/president_polls.csv")) {
  message("Test Passed: The dataset was saved successfully.")
} else {
  stop("Test Failed: The dataset could not be saved.")
}
