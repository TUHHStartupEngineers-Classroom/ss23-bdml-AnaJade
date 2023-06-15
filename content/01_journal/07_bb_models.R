# Load Libraries 
library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)
library(rsample)

# Load Data
employee_attrition_tbl <- read_csv("content/01_journal/data/04_data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("content/01_journal/data/04_data/data_definitions.xlsx", sheet = 1, col_names = FALSE)

# Processing Pipeline
source("scripts/data_processing_pipeline.R")

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>% 
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_2_AutoML_1_20230615_93604")
automl_leader

# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

predictions_tbl

test_tbl %>%
  slice(1) %>%
  glimpse()

# 3.2 Single Explanation ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model           = automl_leader,
    bin_continuous  = TRUE,
    n_bins          = 4,
    quantile_bins   = TRUE
  )

explainer

explanation <- test_tbl %>%
  slice(1) %>%
  select(-Attrition) %>%
  lime::explain(
    
    # Pass our explainer object
    explainer = explainer,
    # Because it is a binary classification model: 1
    n_labels   = 1,
    # number of features to be returned
    n_features = 8,
    # number of localized linear models
    n_permutations = 5000,
    # Let's start with 1
    kernel_width   = 1
  )

explanation

# Part 1 ----
explanation %>%
  as.tibble() %>%
  select(feature:prediction) 

# Expected result
# plot_features(explanation = explanation, ncol = 1)
# From scratch version
# Copy function
label_both_upper <- function(labels, multi_line = TRUE, sep = ': ') {
  # names(labels) <- toTitleCase(names(labels))
  label_both(labels, multi_line, sep)
}

# Copy necessary variables
ncol = 1
type_pal <- c('Supports', 'Contradicts')
explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 1, type_pal[1], type_pal[2]), levels = type_pal)
description <- paste0(explanation$case, '_', explanation[['label']])
desc_width <- max(nchar(description)) + 1
description <- paste0(format(description, width = desc_width), explanation$feature_desc)
explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])
explanation$case <- factor(explanation$case, unique(explanation$case))
explanation$`Explanation fit` <- format(explanation$model_r2, digits = 2)
description <- paste0(explanation$case, '_', explanation[['label']])
desc_width <- max(nchar(description)) + 1
description <- paste0(format(description, width = desc_width), explanation$feature_desc)
explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])

# For classification
explanation$probability <- format(explanation$label_prob, digits = 2)
explanation$label <- factor(explanation$label, unique(explanation$label[order(explanation$label_prob, decreasing = TRUE)]))

ggplot(explanation) +
  facet_wrap(~ case + label + probability + `Explanation fit`, labeller = label_both_upper, scales = 'free_y', ncol = ncol) +
  geom_col(aes_(~description, ~feature_weight, fill = ~type)) +
  coord_flip() +
  scale_fill_manual(values = c('steelblue', 'firebrick'), drop = FALSE) +
  scale_x_discrete(labels = function(lab) substr(lab, desc_width + 1, nchar(lab))) +
  labs(y = 'Weight', x = 'Feature', fill = '') # +
  # theme_lime()

# Part 2 ----

ggplot(explanation, aes_(~case, ~feature_desc)) +
  geom_tile(aes_(fill = ~feature_weight)) +
  scale_x_discrete('Case', expand = c(0, 0)) +
  scale_y_discrete('Feature', expand = c(0, 0)) +
  scale_fill_gradient2('Feature\nweight', low = 'firebrick', mid = '#f7f7f7', high = 'steelblue') +
  # theme_lime() +
  theme(panel.border = element_rect(fill = NA, colour = 'grey60', size = 1),
        panel.grid = element_blank(),
        legend.position = 'right',
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

