#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)

#### Read data ####
analysis_data <- read_parquet("starter_folder-main/data/analysis_data/analysis_data.parquet")

set.seed(853)

analysis_data <- 
  ces2020 |> 
  slice_sample(n = 1000)


### Model data ####
first_model <-
  stan_glm(
    formula = voted_for ~ gender + education,
    data = analysis_data,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )

prior_summary(first_model)

#### Save model ####
saveRDS(
  first_model,
  file = "starter_folder-main/models/first_model.rds"
)

