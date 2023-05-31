# Import libraries ----
# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)


# 1.0 Load data -----
# Patent
col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "content/01_journal/data/Patent_data_reduced/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
setDT(patent_tbl)
patent_tbl

# Assignee
col_types <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "content/01_journal/data/Patent_data_reduced/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
setDT(assignee_tbl)
assignee_tbl

# Patent_assignee
col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "content/01_journal/data/Patent_data_reduced/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
setDT(patent_assignee_tbl)
patent_assignee_tbl

# USPC
col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "content/01_journal/data/Patent_data_reduced/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
setDT(uspc_tbl)
uspc_tbl

# 2.0 Data wrangling ----
# 2.1 What US company/corporation has the most patents
# Create new table with assignee_id | # patents
patent_nb <- patent_assignee_tbl[, .N, by = .(assignee_id)]
colnames(patent_nb)[colnames(patent_nb) == "N"] <- "nb_patents"
patent_nb

# Merge patent_nb with assignee table
merged_assignee_tbl <- assignee_tbl[patent_nb, on=c(id = "assignee_id")]
merged_assignee_tbl <- merged_assignee_tbl[!is.na(id)]
setorder(merged_assignee_tbl, cols=-"nb_patents")
merged_assignee_tbl

# Sort and filter -> US Company/Corporation -> type = 2
top_US_tbl <- merged_assignee_tbl[type==2]
top_US_tbl[1:10]

# 2.2 Recent patent activity
# Merge patent + patent_assignee by patent_id
merged_patent_assignee_tbl <- patent_tbl[patent_assignee_tbl, on=c(id = "patent_id")]
merged_patent_assignee_tbl

# Filter to keep only August patents
august_patents <- merged_patent_assignee_tbl[format(date, "%m") == "08"]
august_patents

# Count nb of patents per company
patent_nb_august <- august_patents[, .N, by = .(assignee_id)]
colnames(patent_nb_august)[colnames(patent_nb_august) == "N"] <- "nb_patents"
patent_nb_august

# Merge patent_nb_august + assignee table by assignee_id
merged_assignee_august_tbl <- assignee_tbl[patent_nb_august, on=c(id = "assignee_id")]
merged_assignee_august_tbl <- merged_assignee_august_tbl[!is.na(id)]
setorder(merged_assignee_august_tbl, cols=-"nb_patents")
merged_assignee_august_tbl

# Filter to keep US companies/corporations
top_US_august_tbl <- merged_assignee_august_tbl[type==2]
top_US_august_tbl[1:10]

# 2.3 Innovation in tech
# 2.3.1 - Top 10 companies (worldwide) with the most patents
merged_assignee_tbl[1:10]

# 2.3.2 - Top 5 USPTO tech main classes
nb_mainclass_tbl <- uspc_tbl[, .N, by = .(mainclass_id)]
colnames(nb_mainclass_tbl)[colnames(nb_mainclass_tbl) == "N"] <- "nb_patents"
setorder(nb_mainclass_tbl, cols=-"nb_patents")
nb_mainclass_tbl[1:5]

