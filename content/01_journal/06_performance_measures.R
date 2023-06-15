# Run 05_auto_ml_2.R first

# Leaderboard visualization ----
automl_models_h2o@leaderboard %>% 
  as_tibble() %>% 
  select(-c(mean_per_class_error, rmse, mse))

data_transformed_tbl <- automl_models_h2o@leaderboard %>%
  as_tibble() %>%
  select(-c(aucpr, mean_per_class_error, rmse, mse)) %>% 
  mutate(model_type = str_extract(model_id, "[^_]+")) %>%
  slice(1:15) %>% 
  rownames_to_column(var = "rowname") %>%
  # Visually this step will not change anything
  # It reorders the factors under the hood
  mutate(
    model_id   = as_factor(model_id) %>% reorder(auc),
    model_type = as.factor(model_type)
  ) %>% 
  pivot_longer(cols = -c(model_id, model_type, rowname), 
               names_to = "key", 
               values_to = "value", 
               names_transform = list(key = forcats::fct_inorder)
  ) %>% 
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev())

# Plot results
data_transformed_tbl %>%
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  
  # Facet to break out logloss and auc
  facet_wrap(~ key, scales = "free_x") +
  labs(title = "Leaderboard Metrics",
       subtitle = paste0("Ordered by: ", "auc"),
       y = "Model Postion, Model ID", x = "") + 
  theme(legend.position = "bottom")

# Tune a model with grid search ----
gbm_h2o <- h2o.loadModel("content/01_journal/05_Modeling/h20_models/GBM_3_AutoML_3_20230615_121714")
h2o.performance(gbm_h2o, newdata = as.h2o(test_tbl))

# GBM hyperparameters
gbm_params_01 <- list(learn_rate = c(0.01, 0.1),
                      max_depth = c(3, 5, 9),
                      sample_rate = c(0.8, 1.0),
                      col_sample_rate = c(0.2, 0.5, 1.0))

# Perform grid search
gbm_grid_01 <- h2o.grid(
  
  # See help page for available algos
  algorithm = "gbm",
  
  # I just use the same as the object
  grid_id = "gbm_grid_01",
  
  # predictor and response variables
  x = x,
  y = y,
  
  # training and validation frame and crossfold validation
  training_frame   = train_h2o,
  validation_frame = valid_h2o,
  nfolds = 5,
  
  # Hyperparamters: Use deeplearning_h2o@allparameters to see all
  hyper_params = list(ntrees = c(1, 2, 3),
                      max_depth = c(3, 5, 9))
)

# View results
summary(gbm_grid_01)
gbm_grid_01

# Visualize the trade of between the precision and the recall and the optimal threshold ----
gbm_grid_01_model_1 <- h2o.getModel("gbm_grid_01_model_12")

gbm_grid_01_model_1 %>% h2o.auc(train = T, valid = T, xval = T)

# Run it on test data
gbm_grid_01_model_1 %>% h2o.performance(newdata = as.h2o(test_tbl))

# Get the confusion matrix
h2o.confusionMatrix(gbm_grid_01_model_1)

# Create performance object
performance_h2o <- h2o.performance(gbm_grid_01_model_1, newdata = as.h2o(test_tbl))

performance_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as.tibble() 

performance_tbl %>% 
  glimpse()

# New theme
theme_new <- theme(
  legend.position  = "bottom",
  legend.key       = element_blank(),
  panel.background = element_rect(fill   = "transparent"),
  panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
  panel.grid.major = element_line(color = "grey", size = 0.333)
) 

performance_tbl %>%
  filter(f1 == max(f1))

performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  
  # Insert line where precision and recall are harmonically optimized
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")) +
  labs(title = "Precision vs Recall", y = "value") +
  theme_new

# ROC Plot ----
# Create function to get performance metrics
load_model_performance_metrics <- function(path, test_tbl) {
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
  
  perf_h2o %>%
    h2o.metric() %>%
    as_tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc, precision, recall)
  
}


h2o.getGrid(grid_id = "gbm_grid_01", sort_by = "auc", decreasing = TRUE)

model_metrics_tbl <- fs::dir_info(path = "content/01_journal/05_Modeling/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest(cols = metrics)

model_metrics_tbl %>%
  mutate(
    # Extract the model names
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(fpr, tpr, color = path, linetype = auc)) +
  geom_line(size = 1) +
  
  # just for demonstration purposes
  geom_abline(color = "red", linetype = "dotted") +
  
  theme_new +
  theme(
    legend.direction = "vertical",
  ) +
  labs(
    title = "ROC Plot",
    subtitle = ""
  )

# Precision vs Recall Plot----
model_metrics_tbl <- fs::dir_info(path = "content/01_journal/05_Modeling/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest(cols = metrics)

model_metrics_tbl %>%
  mutate(
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(recall, precision, color = path, linetype = auc)) +
  geom_line(size = 1) +
  theme_new + 
  theme(
    legend.direction = "vertical",
  ) +
  labs(
    title = "Precision vs Recall Plot",
    subtitle = ""
  )

# Gain and lift ----
ranked_predictions_tbl <- predictions_tbl %>%
  bind_cols(test_tbl) %>%
  select(predict:Yes, went_on_backorder) %>%
  # Sorting from highest to lowest class probability
  arrange(desc(Yes))

ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarise(
    cases = n(),
    responses = sum(went_on_backorder == "Yes")
  ) %>%
  arrange(desc(ntile))

calculated_gain_lift_tbl <- ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarise(
    cases = n(),
    responses = sum(went_on_backorder == "Yes")
  ) %>%
  arrange(desc(ntile)) %>%
  
  # Add group numbers (opposite of ntile)
  mutate(group = row_number()) %>%
  select(group, cases, responses) %>%
  
  # Calculations
  mutate(
    cumulative_responses = cumsum(responses),
    pct_responses        = responses / sum(responses),
    gain                 = cumsum(pct_responses),
    cumulative_pct_cases = cumsum(cases) / sum(cases),
    lift                 = gain / cumulative_pct_cases,
    gain_baseline        = cumulative_pct_cases,
    lift_baseline        = gain_baseline / cumulative_pct_cases
  )

calculated_gain_lift_tbl 

gain_lift_tbl <- performance_h2o %>%
  h2o.gainsLift() %>%
  as.tibble()

# Gain Chart ----

gain_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain     = cumulative_capture_rate) %>%
  # prepare the data for the plotting (for the color and group aesthetics)
  pivot_longer(cols = c(gain, baseline), values_to = "value", names_to = "key")

gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  labs(
    title = "Gain Chart",
    x = "Cumulative Data Fraction",
    y = "Gain"
  ) +
  theme_new

# Lift Plot ----

lift_transformed_tbl <- gain_lift_tbl %>% 
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("capture")) %>%
  mutate(baseline = 1) %>%
  rename(lift = cumulative_lift) %>%
  pivot_longer(cols = c(lift, baseline), values_to = "value", names_to = "key")

lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  labs(
    title = "Lift Chart",
    x = "Cumulative Data Fraction",
    y = "Lift"
  ) +
  theme_new

# Dashboard with cowplot ----
library(cowplot)
library(glue)


# set values to test the function while building it
h2o_leaderboard <- automl_models_h2o@leaderboard
newdata <- test_tbl
order_by <- "auc"
max_models <- 4
size <- 1

plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
  
  # Inputs
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as_tibble()
  
  # Selecting the first, if nothing is provided
  order_by      <- tolower(order_by[[1]]) 
  
  # Convert string stored in a variable to column name (symbol)
  order_by_expr <- rlang::sym(order_by)
  
  # Turn of the progress bars ( opposite h2o.show_progress())
  h2o.no_progress()
  
  # 1. Model metrics
  
  get_model_performance_metrics <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as.tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
    
  }
  
  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
    unnest(cols = metrics) %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        # programmatically reorder factors depending on order_by
        fct_reorder(!! order_by_expr, 
                    .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc      = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss  = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    )
  
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes(fpr, tpr, color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    theme_new +
    labs(title = "ROC", x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical") 
  
  
  # 1B. Precision vs Recall
  
  p2 <- model_metrics_tbl %>%
    ggplot(aes(recall, precision, color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    theme_new +
    labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
    theme(legend.position = "none") 
  
  
  # 2. Gain / Lift
  
  get_gain_lift <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
    
    perf_h2o %>%
      h2o.gainsLift() %>%
      as.tibble() %>%
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
    
  }
  
  gain_lift_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
    unnest(cols = metrics) %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, 
                    .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    ) %>%
    rename(
      gain = cumulative_capture_rate,
      lift = cumulative_lift
    ) 
  
  # 2A. Gain Plot
  
  p3 <- gain_lift_tbl %>%
    ggplot(aes(cumulative_data_fraction, gain, 
               color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size,) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 color = "red", size = size, linetype = "dotted") +
    theme_new +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Gain",
         x = "Cumulative Data Fraction", y = "Gain") +
    theme(legend.position = "none")
  
  # 2B. Lift Plot
  
  p4 <- gain_lift_tbl %>%
    ggplot(aes(cumulative_data_fraction, lift, 
               color = model_id, linetype = !! order_by_expr)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                 color = "red", size = size, linetype = "dotted") +
    theme_new +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Lift",
         x = "Cumulative Data Fraction", y = "Lift") +
    theme(legend.position = "none") 
  
  
  # Combine using cowplot
  
  # cowplot::get_legend extracts a legend from a ggplot object
  p_legend <- get_legend(p1)
  # Remove legend from p1
  p1 <- p1 + theme(legend.position = "none")
  
  # cowplot::plt_grid() combines multiple ggplots into a single cowplot object
  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
  
  # cowplot::ggdraw() sets up a drawing layer
  p_title <- ggdraw() + 
    
    # cowplot::draw_label() draws text on a ggdraw layer / ggplot object
    draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
               color = "#2C3E50")
  
  p_subtitle <- ggdraw() + 
    draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,  
               color = "#2C3E50")
  
  # Combine everything
  ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                   
                   # Adjust the relative spacing, so that the legends always fits
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
  
  h2o.show_progress()
  
  return(ret)
  
}

automl_models_h2o@leaderboard %>%
  plot_h2o_performance(newdata = test_tbl, order_by = "logloss", 
                       size = 0.5, max_models = 4)

