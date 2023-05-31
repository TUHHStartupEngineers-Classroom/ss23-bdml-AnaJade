# Data Science at TUHH ------------------------------------------------------
# DATA ACQUISITION CHALLENGE ----

# 0.0 Install necessary libraries ----
# devtools::install_github("PMassicotte/gtrendsR") 


# 1.0 Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(gtrendsR)
library(rvest)     # HTML Hacking & Web Scraping

# 2.0 Get some data via an API ----
# grends documentation
#   https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf
#   Tutorial: http://lab.rady.ucsd.edu/sawtooth/business_analytics_in_r/DataApi.html
#   Website: https://trends.google.com/trends?geo=DE&hl=en-US
# Get Google trends data for Eurovision
res <- gtrends(c("Eurovision Song Contest"),
               geo=c("DE", "FI", "NO", "SI", "HR"),
               time = "today 3-m")
# ISO country codes
#   DE: Deutschland
#   FI: Finland
#   NO: Norway
#   SI: Slovenia
#   HR: Croatia
plot(res)

# 3.0 Scrape data from a competitor website ----
# Define URL
url_hardtail <- "https://www.radon-bikes.de/mountainbike/hardtail/"
html_hardtail    <- read_html(url_hardtail)

# Extract bike info
bike_names <- list(html_hardtail %>% html_nodes(css = ".a-heading--medium") %>%  html_text())
bike_names
bike_prices <- list(html_hardtail %>% html_nodes(css = ".currentPrice") %>%  html_text())
bike_prices
# Remove empty elements from bike_prices
bike_prices <- lapply(bike_prices, function(z){ z[!is.na(z) & z != ""]})
bike_prices

# Create table
bike_infos <- map2_dfr(bike_names, bike_prices, ~ tibble(Name = .x, Price = .y))
bike_infos
