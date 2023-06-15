# Load libraries
library(tidyverse)
library(readxl)
library(rsample)
library(recipes)
library(h2o)

# Load the training & test dataset ----
product_backorders_tbl <- read_csv("content/01_journal/data/05_data/product_backorders.csv")
set.seed(seed = 1113)
split_obj              <- rsample::initial_split(product_backorders_tbl, prop = 0.85)
train_tbl              <- training(split_obj)
test_tbl               <- testing(split_obj)

# Specifiy the response and predictor variables ----
recipe_obj <- recipe(went_on_backorder ~ ., data = train_tbl) %>%
  step_zv(all_predictors()) %>%
  # step_center(all_numeric()) %>%
  # step_scale(all_numeric()) %>%
  # step_dummy(all_nominal()) %>% 
  
  # prepare the final recipe
  prep()

train_tbl <- bake(recipe_obj, new_data = train_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_tbl)

# Run AutoML specifying the stopping criterion ----
h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproductability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)

# View the leaderboard ----
# View model leaderboard
automl_models_h2o@leaderboard

# View leader model
leader <- automl_models_h2o@leader

# Predicting using Leader Model ----
predictions <- h2o.predict(leader, newdata = as.h2o(test_tbl))

# Convert to tibble and view results
predictions_tbl <- predictions %>% as_tibble()
predictions_tbl

# Save the leader model ----
h2o.getModel(leader@model_id) %>% 
 h2o.saveModel(path = "content/01_journal/05_Modeling/h2o_models/")

