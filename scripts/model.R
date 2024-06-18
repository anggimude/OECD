#### Preamble ####
# Purpose: Models the correlation between worker productivity and hourly wage
# Author: Hyuk Jang
# Date: today
# Contact: hyuk.jang@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(ggplot2)
library(modelsummary)
library(bayesplot)
library(parameters)
library(broom)
library(kableExtra)
library(gt)
library(broom.mixed)
library(dplyr)
library(tidyr)

long_data <- cleaned_table |>
  pivot_longer(cols = c(`GDP per head of population`, `GDP per hour worked`, `GDP per person employed`, `Hourly Wage`),
               names_to = "Metric",
               values_to = "Value")

ggplot(long_data, aes(x = Time, y = Value, color = Metric)) +
  geom_point() +
  facet_wrap(~ Country) + # To show data for each country separately
  ggtitle("Economic Metrics Over Time") +
  xlab("Time") +
  ylab("Value") +
  theme_minimal()

# Center the Hourly Wage variable
cleaned_table$Hourly_Wage_Centered <- scale(cleaned_table$`Hourly Wage`, center = TRUE, scale = FALSE)

# Fit the model with centered Hourly Wage
econ_norm_data_centered <- stan_glm(
  `GDP per hour worked` ~ `Hourly_Wage_Centered` * `Country`,
  data = cleaned_table,
  family = gaussian(link = "identity"),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  seed = 123
)

# create a model summary
modelsummary(
  list(
    "Gaussian(Normal)" = econ_norm_data_centered
  ),
  statistic = "mad",
  fmt = 2
) 

# create a credibility interval of 95%
modelplot(econ_norm_data, conf_level = 0.95) +
  labs(x = "95 per cent credibility interval")

# Save the linear model
saveRDS(
  econ_norm_data_centered,
  file = "~/oecd/models/econ_norm_model.rds"
)
