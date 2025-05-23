# Load necessary libraries
library(tidyverse)
library(janitor)
library(googlesheets4)

# -------------------------------------------------------------------------
# Load and Prepare Data
# -------------------------------------------------------------------------

# Load dataset from Google Sheets
url <- 'https://docs.google.com/spreadsheets/d/1r_ait8M3KpA5iF-Ysq_9hiqQhuTQmkpwUgQozdqm7Ho/edit?gid=236606241#gid=236606241'
df_raw <- read_sheet(url, sheet = 1)

# Columns to propagate from first entry per DOI
columns_to_fill <- c(
  "location_detail", "country", "study_object", "land_use_context",
  "modified_condition_of_the_population_by_human_intervention_prior_to_the_study",
  "intervention_in_the_study_has_to_be_an_activity_that_modifies_the_population_study_for_the_sake_of_the_study",
  "ncs_category_griscom_2020", "study_design", "comparator",
  "statistical_analysis"
)

# Clean and prepare base dataframe
df_clean <- df_raw %>%
  clean_names() %>%
  mutate(doi = str_trim(doi))

# Fill down missing values based on first occurrence of each DOI
doi_lookup <- df_clean %>%
  group_by(doi) %>%
  slice(1) %>%
  ungroup() %>%
  select(doi, all_of(columns_to_fill))

df_filled <- df_clean %>%
  left_join(doi_lookup, by = "doi", suffix = c("", "_first")) %>%
  mutate(across(all_of(columns_to_fill),
                ~ coalesce(.x, get(paste0(cur_column(), "_first"))))) %>%
  select(-ends_with("_first"))

# -------------------------------------------------------------------------
# Standardize and Clean Fields
# -------------------------------------------------------------------------

df_outcomes <- df_filled %>%
  select(-timestamp, -column_19) %>%
  mutate(across(everything(), ~ na_if(.x, ""))) %>%
  mutate(across(where(is.character), str_squish))

# -------------------------------------------------------------------------
# Classify Outcome Groups
# -------------------------------------------------------------------------

# Load long-format outcome keywords
keywords_long <- read_csv("data_extraction_inputs/outcome_keywords.csv")

df_outcomes <- df_outcomes %>%
  rowwise() %>%
  mutate(outcome_group = {
    match <- keywords_long %>%
      filter(str_detect(outcome, regex(keyword, ignore_case = TRUE))) %>%
      pull(outcome_group)
    if (length(match) > 0) match[1] else "Other"
  }) %>%
  ungroup()

# -------------------------------------------------------------------------
# Classify Method Groups
# -------------------------------------------------------------------------

# Load long-format method keywords
method_keywords <- read_csv("data_extraction_inputs/methods_keywords.csv")

df_outcomes <- df_outcomes %>%
  rowwise() %>%
  mutate(method_group = {
    match <- method_keywords %>%
      filter(str_detect(data_collection_methodology_method, regex(keyword, ignore_case = TRUE))) %>%
      pull(method_group)
    if (length(match) > 0) match[1] else "Other"
  }) %>%
  ungroup()

# viz
# count group outcome types
df_outcomes %>%
  count(outcome_group, sort = TRUE) %>%
  ggplot(aes(x = fct_rev(outcome_group), y = n)) +
  geom_col(fill = "#e76f51") +
  coord_flip() +
  labs(title = "Grouped Outcome Types", x = "Outcome Category", y = "Count")

library(tidyr)

library(ggplot2)
# Methodology vs Outcome Group
df_outcomes %>%
  count(method_category, outcome_group) %>%
  ggplot(aes(x = method_category, y = n, fill = outcome_group)) +
  geom_col(position = "dodge") +
  labs(
    title = "Method Category vs Outcome Group",
    x = "Method Category",
    y = "Count",
    fill = "Outcome Group"
  ) +
  theme_minimal()

# Methodology vs Ecosystem Type
df_outcomes %>%
  count(method_group, land_use_context) %>%
  ggplot(aes(x = method_group, y = n, fill = land_use_context)) +
  geom_col(position = "dodge") +
  labs(
    title = "Method Category vs Ecosystem Type",
    x = "Method Category",
    y = "Count",
    fill = "Ecosystem Type"
  ) +
  theme_minimal()

# Heatmap of Outcome × Method × Ecosystem
df_outcomes %>%
  count(method_group, outcome_group, land_use_context) %>%
  ggplot(aes(x = outcome_group, y = land_use_context, fill = n)) +
  geom_tile(color = "white") +
  facet_wrap(~ method_group) +
  scale_fill_viridis_c() +
  labs(
    title = "Outcome vs Ecosystem by Method Type",
    x = "Outcome Group",
    y = "Ecosystem Type",
    fill = "Count"
  ) +
  theme_minimal()

