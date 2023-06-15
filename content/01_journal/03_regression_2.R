# Load libraries ----
library(tidymodels)  # for the parsnip package, along with the rest of tidymodels
# Standard
library(tidyverse)
# Preprocessing & Sampling
library(recipes)
library(rsample)
# Modeling Error Metrics
library(yardstick)


# 1 Build a model ----
# Load data from files
bike_features_tbl <- readRDS("content/01_journal/data/bike_features_tbl.rds")
# Modify to remove gravel
# bike_features_tbl <- bike_features_tbl %>% 
#   filter(category_1 %in% c("E-Bikes", "Hybrid / City", "Mountain", "Road"))

glimpse(bike_features_tbl)


bike_features_tbl <- bike_features_tbl %>% 
  select(model:url)%>% 
  mutate(id = row_number()) %>% 
  select(id, everything(), -url)

# Create train/test split
set.seed(321)
# Put 3/4 of the data into the training set 
data_split <- initial_split(bike_features_tbl, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Create model
linear_lm_simple <- linear_reg(mode = "regression") %>%
  set_engine("lm")

# 2 Create features with recipes package ----
# Create recipe
bikes_rec <- recipe(price ~ ., data = train_data) %>%
  update_role(id, model, new_role = "ID")%>%
  step_rm(model) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  prep()

summary(bikes_rec)

train_transformed_tbl <- bake(bikes_rec, train_data)
test_transformed_tbl  <- bake(bikes_rec, test_data)    


# 3 Bundle the model and recipe with the workflow package ----
# Create the workflow
bikes_wflow <- 
  workflow() %>% 
  add_model(linear_lm_simple) %>% 
  add_recipe(bikes_rec)
bikes_wflow

# Train the model
bikes_fit <- 
  bikes_wflow %>% 
  fit(data = train_data)

# Show the model
bikes_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Evaluate the model with the yardstick package ----
# Get predictions
bikes_pred <- predict(bikes_fit, test_data)
bikes_pred

# Get metrics
bikes_pred %>%
  bind_cols(test_data %>% select(price)) %>%
  yardstick::metrics(truth = price, estimate = .pred)


