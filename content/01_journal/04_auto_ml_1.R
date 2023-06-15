# Libraries 
library(tidyverse)
library(readxl)
library(skimr)
library(GGally)

# Load Data data definitions
employee_attrition_tbl <- read_csv("data/04_data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
path_data_definitions <- "data/04_data/data_definitions.xlsx"
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

employee_attrition_tbl

# Descriptive Features
employee_attrition_tbl %>% select(Age, DistanceFromHome, Gender, MaritalStatus, NumCompaniesWorked, Over18)

# Employment Features
employee_attrition_tbl %>% select(Department, EmployeeCount, EmployeeNumber, JobInvolvement, JobLevel, JobRole, JobSatisfaction)

# Compensation Features
employee_attrition_tbl %>% select(DailyRate, HourlyRate, MonthlyIncome, MonthlyRate, PercentSalaryHike, StockOptionLevel)

# Survery Results
employee_attrition_tbl %>% select(EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance)

# Performance Data
employee_attrition_tbl %>% select(JobInvolvement, PerformanceRating)

# Work-Life Features
employee_attrition_tbl %>% select(BusinessTravel, OverTime)

# Training & Education
employee_attrition_tbl %>% select(Education, EducationField, TrainingTimesLastYear)

# Time-Based Features
employee_attrition_tbl %>% select(TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)


# Step 1: Data Summarization -----

skim(employee_attrition_tbl)

# Character Data Type
employee_attrition_tbl %>%
  select_if(is.character) %>%
  glimpse()

# Get "levels"
employee_attrition_tbl %>%
  select_if(is.character) %>%
  map(unique)

# Proportions    
employee_attrition_tbl %>%
  select_if(is.character) %>%
  map(~ table(.) %>% prop.table())

# Numeric Data
employee_attrition_tbl %>%
  select_if(is.numeric) %>%
  map(~ unique(.) %>% length())

employee_attrition_tbl %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  # Select all columns
  pivot_longer(everything()) %>%
  arrange(value) %>%
  filter(value <= 10)

# Step 2: Data Visualization ----
employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  ggpairs(aes(color = Attrition), lower = "blank", legend = 1,
          diag  = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

# Create data tibble, to potentially debug the plot_ggpairs function (because it has a data argument)
data <- employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome)

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank") 
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", 
                                            alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  
  return(g)
  
}

employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  plot_ggpairs(color = Attrition)

# Explore Features by Category

#   1. Descriptive features: age, gender, marital status 
employee_attrition_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  plot_ggpairs(Attrition)

#   2. Employment features: department, job role, job level
employee_attrition_tbl %>%
  select(Attrition, contains("employee"), contains("department"), contains("job")) %>%
  plot_ggpairs(Attrition) 

#   3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel 
employee_attrition_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)

#   4. Survey Results: Satisfaction level, WorkLifeBalance 
employee_attrition_tbl %>%
  select(Attrition, contains("satisfaction"), contains("life")) %>%
  plot_ggpairs(Attrition)

#   5. Performance Data: Job Involvment, Performance Rating
employee_attrition_tbl %>%
  select(Attrition, contains("performance"), contains("involvement")) %>%
  plot_ggpairs(Attrition)

#   6. Work-Life Features 
employee_attrition_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_ggpairs(Attrition)

#   7. Training and Education 
employee_attrition_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_ggpairs(Attrition)

#   8. Time-Based Features: Years at company, years in current role
employee_attrition_tbl %>%
  select(Attrition, contains("years")) %>%
  plot_ggpairs(Attrition)







