# Data Science at TUHH ------------------------------------------------------
# TIDYVERSE CHALLENGE ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bike_orderlines_tbl <- read_excel(path = "content/01_journal/data/bike_orderlines.xlsx")

# 3.0 Examining Data ----
bike_orderlines_tbl
glimpse(bike_orderlines_tbl)

# 4.0 Split city and state Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_tbl %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ")

# 5.0 Revenue by states ----
# Manipulate
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  # Keep state and total_price columns
  select(state, total_price) %>%
  # Group by state (kinda like sort)
  group_by(state) %>% 
  # Sum sales per state
  summarize(sales = sum(total_price)) %>%
  # Convert the column into the currency format
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
glimpse(sales_by_state_tbl)

# Visualize
sales_by_state_tbl %>%
  
  # Plot with sales(year)
  ggplot(aes(x = state, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  
  # Formatting
  # Adjust from $ to €
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(
    title    = "Revenue by state",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.0 Revenue by states and year ----
# Manipulate
sales_by_state_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select state, date and total price columns + add a year
  select(order_date, state, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

glimpse(sales_by_state_year_tbl)

# Visualize
sales_by_state_year_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  theme(legend.position = "bottom") +
  labs(
    title = "Revenue by year and state",
    fill = "State" # Changes the legend name
  )



