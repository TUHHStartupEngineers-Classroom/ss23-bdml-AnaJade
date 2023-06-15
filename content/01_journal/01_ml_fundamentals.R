# Data Science at TUHH ------------------------------------------------------
# Machine Learning Fundamentals ----

# 1.0 Load libraries ----
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)

# 2.0 Load data ----
# STOCK PRICES
sp_500_prices_tbl <- read_rds("content/01_journal/data/01_data/sp_500_prices_tbl.rds")
sp_500_prices_tbl

# SECTOR INFORMATION
sp_500_index_tbl <- read_rds("content/01_journal/data/01_data/sp_500_index_tbl.rds")
sp_500_index_tbl

# 3.0 Which stock prices behave similarly ----
## Step 1 - Convert stock prices to a standardized format (daily returns)
### Load expected results
e_sp_500_daily_returns_tbl <- read_rds("content/01_journal/data/01_data/sp_500_daily_returns_tbl.rds")
e_sp_500_daily_returns_tbl

### Make the required modifications
# Save as a variable named `sp_500_daily_returns_tbl`
sp_500_daily_returns_tbl <- sp_500_prices_tbl %>%
  # Select the `symbol`, `date` and `adjusted` columns
  select(symbol, date, adjusted) %>%
  # Filter to dates beginning in the year 2018 and beyond. 
  filter(date > '2018-01-01') %>%
  # Compute a Lag of 1 day on the adjusted stock price
  group_by(symbol)  %>%
  # mutate(lag_1day=adjusted) %>%
  mutate(lag_1day = lag(adjusted, n = 1L)) %>%
  # Remove a `NA` values from the lagging operation
  filter(!is.na(lag_1day)) %>%
  # Compute the difference between adjusted and the lag
  mutate(diff=adjusted-lag_1day) %>%
  # Compute the percentage difference by dividing the difference by that lag. Name this column `pct_return`.
  mutate(pct_return = diff/lag_1day) %>%
  ungroup() %>%
  # Return only the `symbol`, `date`, and `pct_return` columns
  select(symbol, date, pct_return)
  
## Step 2 - Convert to User-Item Format
### Load expected results
e_stock_date_matrix_tbl <- read_rds("content/01_journal/data/01_data/stock_date_matrix_tbl.rds")

### Make the required modifications
# Save the result as `stock_date_matrix_tbl`
stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>% 
  # Spread the `date` column to get the values as percentage returns. 
  # Make sure to fill an `NA` values with zeros. 
  pivot_wider(names_from = date, values_from = pct_return, values_fill = 0) %>%
  arrange(symbol) %>%
  ungroup()
  
## Step 3 - Perform K-Means Clustering
# Create kmeans_obj for 4 centers
# Save the result as `kmeans_obj`
kmeans_obj <- stock_date_matrix_tbl %>%
  # Drop the non-numeric column 'symbol'
  select(-symbol) %>%
  # Perform kmeans with centers=4 and nstart=20
  kmeans(centers = 4, nstart = 20)

# Apply glance() to get the tot.withinss
broom::glance(kmeans_obj)

# 4.0 Find the optimal value of K ----
# kmeans_mapper function
kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}

# Save the output as `k_means_mapped_tbl` 
# Create a tibble containing column called `centers` that go from 1 to 30
k_means_mapped_tbl <- tibble(centers = 1:30) %>%
  # Add k_means column to map the centers to kmeans_mapper
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  # Add glance
  mutate(glance  = k_means %>% map(glance))

# View the results
k_means_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  
  # Visualization
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  # Add labels (which are repelled a little)
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") + 
  
  # Formatting
  labs(title = "Skree Plot",
       subtitle = "",
       caption = "")

## 5.0 - Apply UMAP ----
umap_results <- stock_date_matrix_tbl %>%
  # De-select the symbol column
  select(-symbol) %>%
  umap()

# Convert umap results to tibble with symbols
# Start with `umap_results$layout`
umap_results_tbl <- umap_results$layout %>%
  # Convert from a `matrix` data type to a `tibble` with `as_tibble()`
  as_tibble(.name_repair = "unique") %>% # argument is required to set names in the next step
  set_names(c("V1", "V2")) %>%
  # Bind the columns of the umap tibble with the `symbol` column from the `stock_date_matrix_tbl`
  bind_cols(stock_date_matrix_tbl %>% select(symbol))

# Visualize UMAP results
umap_results_tbl %>%
  ggplot(aes(V1, V2)) +
  geom_point(alpha = 0.5) + 
  theme_tq(base_size = 11, base_family = "") +
  # Formatting
  labs(title = "UMAP Projection",
       subtitle = "",
       caption = "")

## 6.0 - Combine K-Means and UMAP ----
e_k_means_mapped_tbl <- read_rds("content/01_journal/data/01_data/k_means_mapped_tbl.rds")
e_umap_results_tbl   <- read_rds("content/01_journal/data/01_data/umap_results_tbl.rds")

# Get the k_means_obj from the 10th center
kmeans_10_obj <- k_means_mapped_tbl %>%
  pull(k_means) %>%
  pluck(10)

# Convert it to a tibble with broom
kmeans_10_clusters_tbl <- kmeans_10_obj %>% 
  augment(stock_date_matrix_tbl) %>%
  # Select the data we need
  select(symbol, .cluster)

# Bind data together
umap_kmeans_results_tbl <- umap_results_tbl %>%
  left_join(kmeans_10_clusters_tbl, by = "symbol") %>%
  left_join(sp_500_index_tbl %>% select(symbol, company, sector), by = "symbol")


# Plot results
umap_kmeans_results_tbl %>%
  mutate(label_text = str_glue("Customer: {symbol}
                                 Cluster: {.cluster}")) %>%
  
  ggplot(aes(V1, V2, color = .cluster)) +
  
  # Geometries
  geom_point(alpha = 0.5) +
  
  # Formatting
  # scale_color_manual(values=c("#2d72d6", "#2dc6d6", "#2dd692")) +
  scale_color_manual(values = palette_light() %>% rep(3)) + 
  labs(title = "Visualize the combined K-Means and UMAP results",
       subtitle = "",
       caption = "") +
  theme(legend.position = "none")


