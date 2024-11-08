#### Preamble ####
# Purpose: Cleans the raw plane data 
# Author: Hyuk Jang
# Date: 11 Jun 2024
# Contact: hyuk.jang@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(dplyr)
library(purrr)

#### Clean data ####
oecddata <- read_csv("data/raw_data/oecddata.csv")
minwage <- read_csv("data/raw_data/minwageoecd.csv")


minwage <- minwage |>
  select(c(-COUNTRY, -TIME))|>
  filter(Country != "Germany")

oecddata <- oecddata[oecddata$Measure == 'USD, current prices, current PPPs', ]
oecddata <- oecddata |>
  select(c(-LOCATION, -SUBJECT, -MEASURE, -TIME, -Unit, -`PowerCode Code`, -PowerCode, -`Reference Period Code`, -`Reference Period`, -`Flag Codes`, -Flags))|>
  filter(Country != "Germnany")

oecdgdphead <- oecddata[oecddata$Subject == 'GDP per head of population', ]
oecdgdphworked <- oecddata[oecddata$Subject == 'GDP per hour worked', ]
oecdgdpppe <- oecddata[oecddata$Subject == 'GDP per person employed', ]

oecdcountries <- unique(oecddata$Country)
countries <- unique(minwage$Country)
common_countries <- intersect(oecdcountries, countries)
common_tb <- data.frame(Country = common_countries)


table1 <- inner_join(common_tb, oecdgdphead, by = "Country")
table2 <- inner_join(common_tb, oecdgdphworked, by = "Country")
table3 <- inner_join(common_tb, oecdgdpppe, by = "Country")
minwagetb <- inner_join(common_tb, minwage, by = "Country")

t1 <- unique(table1$Country)
t2 <- data.frame(Country = unique(table2$Country))
t3 <- unique(table3$Country)
t4 <- unique(minwagetb$Country)

ta1 <- inner_join(t2, oecdgdphead, by = "Country")
ta2 <- inner_join(t2, oecdgdphworked, by = "Country")
ta3 <- inner_join(t2, oecdgdpppe, by = "Country")
minwagetb <- inner_join(t2, minwage, by = "Country")

tab1 <- ta1|>
  rename(`GDP per head of population` = Value)|>
  select(c(-`Unit Code`, -Measure, -Subject))
tab2 <- ta2|>
  rename(`GDP per hour worked` = Value)|>
  select(c(-`Unit Code`, -Measure, -Subject))

tab3 <- ta3|>
  rename(`GDP per person employed` = Value)|>
  select(c(-`Unit Code`, -Measure, -Subject))

tab4 <- minwagetb|>
  rename(`Hourly Wage` = Value)|>
  select(c(-`Unit Code`, -Series, -`Pay period`))

cleaned_list <- list(tab1, tab2, tab3, tab4)
cleaned_table <- reduce(cleaned_list, inner_join, by = c("Country", "Time"))
cleaned_table <- cleaned_table |>
  mutate(
    `GDP per head of population` = round(`GDP per head of population`, 2),
    `GDP per hour worked` = round(`GDP per hour worked`, 2),
    `GDP per person employed` = round(`GDP per person employed`, 2),
    `Hourly Wage` = round(`Hourly Wage`, 2)
  )


# Calculate the yearly averages
yearly_avg <- cleaned_table %>%
  group_by(Time) %>%
  summarise(
    Country = "Ae",  
    `GDP per head of population` = mean(`GDP per head of population`, na.rm = TRUE),
    `GDP per hour worked` = mean(`GDP per hour worked`, na.rm = TRUE),
    `GDP per person employed` = mean(`GDP per person employed`, na.rm = TRUE),
    `Hourly Wage` = mean(`Hourly Wage`, na.rm = TRUE)
  )

# Bind the averages to the original table
ct_avg <- bind_rows(cleaned_table, yearly_avg)
ct_avg <- ct_avg |>
  arrange(Country)


#### Save data ####
write_csv(cleaned_table, "~/oecd/data/analysis_data/cleaned_table.csv")
write_parquet(cleaned_table, "~/oecd/data/analysis_data/cleaned_table.parquet")
write_csv(ct_avg, "~/oecd/data/analysis_data/ct_avg.csv")
write_parquet(ct_avg, "~/oecd/data/analysis_data/ct_avg.parquet")