# Forecasting 2024 US Presidential Election

## Overview

This paper presents a statistical analysis and forecasting model for the 2024 U.S. Presidential Election, focusing on predicting candidate support for Kamala Harris and Donald Trump based on polling data across states. Utilizing Bayesian Generalized Linear Models (GLMs) and Monte Carlo simulations, we analyze support percentages for each candidate and estimate the probability of winning. The results indicate that Harris holds a 63% chance of winning, while Trump has a 37% probability of winning, as derived from simulated election outcomes. Key battleground states, such as Pennsylvania, Florida, and Ohio, show close margins, with support percentages that suggest a competitive race. In Democratic strongholds like California, Harris leads with substantial predicted support, while Trump is strongly favored in Republican-leaning states such as Texas. 


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from FiveThirtyEight.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other` contains details about LLM chat interactions and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

Aspects of the abstract, title, and code such as the simulation script, cleaning script, testing script, and the Quarto paper were written with the help of chatGPT-4o and the entire chat history is available in other/llms/usage.txt.
