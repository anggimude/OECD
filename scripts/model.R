#### Preamble ####
# Purpose: Models the correlation between worker productivity and hourly wage
# Author: Hyuk Jang
# Date: 7 Nov 2024
# Contact: hyuk.jang@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(ggplot2)
library(modelsummary)
library(broom)
library(kableExtra)
library(gt)
library(dplyr)
library(tidyr)

long_data <- cleaned_table |>
  pivot_longer(cols = c(`GDP per head of population`, `GDP per hour worked`, `GDP per person employed`, `Hourly Wage`),
               names_to = "Metric",
               values_to = "Value")

ggplot(long_data, aes(x = Time, y = Value, color = Metric)) +
  geom_jitter(width = 0.2, height = 0) +
  facet_wrap(~ Country) + # To show data for each country separately
  ggtitle("Economic Metrics Over Time") +
  xlab("Time") +
  ylab("Value") +
  theme_minimal()

short_data <- cleaned_table |>
  pivot_longer(cols = c(`GDP per hour worked`, `Hourly Wage`),
               names_to = "Metric",
               values_to = "Value") |>
  filter(Metric %in% c("GDP per hour worked", "Hourly Wage"))

ggplot(short_data, aes(x = Time, y = Value, color = Metric)) +
  geom_point() +
  facet_wrap(~ Country) + # To show data for each country separately
  ggtitle("GDP per hour worked compared to Hourly wage") +
  xlab("Time") +
  ylab("Value") +
  theme_minimal()

# Calculate yearly averages
yearly_averages <- short_data %>%
  group_by(Time, Metric) %>%
  summarize(Value = mean(Value, na.rm = TRUE)) %>%
  mutate(Country = "Average")  # Label these as 'Average' to distinguish in the plot

# Combine yearly averages with the original data
combined_data <- bind_rows(short_data, yearly_averages)

# Calculate a rescaling factor
wage_scale <- max(combined_data %>% filter(Metric == "GDP per hour worked") %>% pull(Value)) /
  max(combined_data %>% filter(Metric == "Hourly Wage") %>% pull(Value))

# Plot with separate scales and averages
ggplot(combined_data, aes(x = Time)) +
  # GDP per hour worked
  geom_point(
    data = combined_data %>% filter(Metric == "GDP per hour worked" & Country != "Average"),
    aes(y = Value, color = "GDP per hour worked")
  ) +
  geom_point(
    data = combined_data %>% filter(Metric == "GDP per hour worked" & Country == "Average"),
    aes(y = Value, color = "GDP per hour worked", linetype = "Average")  # Line for average
  ) +
  # Hourly Wage with rescaling
  geom_point(
    data = combined_data %>% filter(Metric == "Hourly Wage" & Country != "Average"),
    aes(y = Value * wage_scale, color = "Hourly Wage")
  ) +
  geom_point(
    data = combined_data %>% filter(Metric == "Hourly Wage" & Country == "Average"),
    aes(y = Value * wage_scale, color = "Hourly Wage", linetype = "Average")
  ) +
  facet_wrap(~ Country) +
  scale_y_continuous(
    name = "GDP per hour worked",
    sec.axis = sec_axis(~ . / wage_scale, name = "Hourly Wage")  # Rescale back to original
  ) +
  scale_color_manual(values = c("GDP per hour worked" = "red", "Hourly Wage" = "blue")) +
  scale_linetype_manual(values = c("Average" = "dashed"), guide = "none") +  # Make averages dashed
  ggtitle("GDP per Hour Worked against Hourly Wage") +
  xlab("Time") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue")
  )

extractae <- ct_avg[ct_avg$Country == "Ae", ] |>
  select(c(-`GDP per head of population`, -'GDP per person employed', -'Hourly Wage'))
growthrate <- extractae |>
  mutate(growth_rate = (`GDP per hour worked` - lag(`GDP per hour worked`)) / lag(`GDP per hour worked`) * 100) |>
  drop_na(growth_rate)|>
  select(c(-`Country`, -`GDP per hour worked`))

avggr <- growthrate |>
  reframe(average_growth_rate = mean(growth_rate, na.rm = TRUE))

library(ggplot2)
ggplot(growthrate, aes(x = Time, y = growth_rate)) +
  geom_line() +
  labs(title = "Average Annual Growth Rate of GDP per Hour Worked",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal()

extractaehw <- ct_avg[ct_avg$Country == "Ae", ] |>
  select(c(-`GDP per head of population`, -'GDP per person employed', -'GDP per hour worked'))
hwgr <- extractaehw |>
  mutate(growth_rate = (`Hourly Wage` - lag(`Hourly Wage`)) / lag(`Hourly Wage`) * 100) |>
  drop_na(growth_rate)|>
  select(c(-`Country`, -`Hourly Wage`))

avggrhw <- hwgr |>
  reframe(average_growth_rate = mean(growth_rate, na.rm = TRUE))

library(ggplot2)
ggplot(hwgr, aes(x = Time, y = growth_rate)) +
  geom_line() +
  labs(title = "Average Annual Growth Rate of Hourly wage",
       x = "Year",
       y = "Growth Rate (%)") +
  theme_minimal()


# Center the Hourly Wage variable
ct_avg$Hourly_Wage_Centered <- scale(ct_avg$`Hourly Wage`, center = TRUE, scale = FALSE)

# Fit the model with centered Hourly Wage
econ_norm_data_centered <- stan_glm(
  `GDP per hour worked` ~ `Hourly_Wage_Centered` * `Country`,
  data = ct_avg,
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
modelplot(econ_norm_data_centered, conf_level = 0.95) +
  labs(x = "95 per cent credibility interval")

# Save the linear model
saveRDS(
  econ_norm_data_centered,
  file = "~/oecd/models/econ_norm_model.rds"
)
