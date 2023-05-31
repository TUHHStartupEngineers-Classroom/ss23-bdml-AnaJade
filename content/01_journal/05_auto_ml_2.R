# Import libraries ----
library(tidyverse)

# Challenge 1 -----
# 1.1 Load data ----
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# 1.2 Data wrangling ----
total_cases_over_time <- covid_data_tbl %>%
  # Select appropriate columns
  select(iso_code,location, date, total_cases) %>%
  # Filter based on country
  filter(location %in% c("Germany", "United Kingdom", "France", "Spain", "United States")) %>%
  # Remove blank values
  filter(!is.na(total_cases)) # %>%

# 1.3 Plot ----
total_cases_over_time %>%
  # Canvas
  ggplot(aes(x = date, y = total_cases, color = location)) +
  # Geometries 
  geom_line(size = 1) +
  # Formatting
  expand_limits(y = 0) +
  scale_x_date(date_breaks = '1 month', date_labels = "%B-%Y") + 
  scale_y_continuous(labels=function(x) sprintf("%.0fM", x/1e6)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Change colour
  scale_color_brewer(palette = "Set1") + 
  # Set legend location
  theme(legend.position = "bottom") +
  # Labels
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 19/04/2022",
    x = "Date",
    y = "Total cases",
    color = "Countries" # Legend text
  )

# Challenge 2 ----
# Visualize the distribution of the mortality rate (deaths/population)
# 2.1 Data wrangling ----
# Example: https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
mortality_tbl <- covid_data_tbl %>%
  # Keep only CAN and DE + others
  # filter(location == c("Canada", "Germany", "United Kingdom", "United States", "Democratic Republic of Congo")) %>%
  # Select relevant columns
  select(location, new_deaths_per_million) %>%
  # Replace all NAs by zeros
  replace_na(list(new_deaths_per_million = 0)) %>%
  # Get sums
  group_by(location) %>%
  summarise(total_deaths_per_million = sum(new_deaths_per_million)) %>%
  # Add death percentage
  mutate(total_deaths_perc = total_deaths_per_million/1e6) %>%
  # Change location names to match the world map
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location)) %>%
  distinct()

mortality_tbl

# Change location column name to match world map
colnames(mortality_tbl)[colnames(mortality_tbl) == "location"] = "region"

# 2.2 Plot ----
# Get world map
world <- map_data("world")
# Join map and data
deaths_map <- left_join(mortality_tbl, world, by="region")
# Plot results
ggplot(deaths_map, aes(long, lat, group=group)) +
  geom_polygon(aes(fill = total_deaths_perc), color = "white")+
  # Format legend
  scale_fill_continuous(name="Mortality rate", labels=scales::percent) +
  # Labels
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "Around 6.2 Million confirmed COVID-19 deaths worldwide",
    color = "Mortality rate" # Legend text
  )
