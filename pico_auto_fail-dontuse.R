# -------------------------
# 1. Install and load necessary packages
# -------------------------
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(tidytext)

library(text2vec)
library(e1071)
library(stringr)
library(quanteda)
library(udpipe)

setwd("search-database-result-copies/Griscom-7/project/")
# -------------------------
# 2. Load and preprocess text data (systematic review abstracts)
# -------------------------
# Replace 'your_data.csv' and 'abstract_column' with your dataset and column name

df <- read.csv("search-database-result-copies/Griscom-7/grisom-7-full-final.csv")

# Specify the columns to clean
cols_to_clean <- c("inclusion", "labels1", "labels2", "labels3", "exclusion_reasons1", "exclusion_reasons2")

# Apply cleaning only to those columns
df_clean <- df %>%
  mutate(across(all_of(cols_to_clean), ~ str_trim(.))) %>%      # Remove leading/trailing spaces
  mutate(across(all_of(cols_to_clean), ~ str_to_lower(.))) %>%  # Convert to lowercase
  mutate(across(all_of(cols_to_clean), ~ na_if(., "na"))) %>%       # Convert "na" (as text) to actual NA
  mutate(across(all_of(cols_to_clean), ~ na_if(., "#n/a"))) %>% 
  filter(inclusion == "included")

abstracts_metadata <- df_clean %>%
  mutate(
    abstract_clean = sapply(abstract, clean_text)
  ) %>%
  select(title, abstract = abstract_clean) %>%
  mutate(abstract_id = row_number())

# Save metadata (title + abstract only)
write_csv(abstracts_metadata, "abstract_metadata.csv")
abstracts <- df_clean$abstract

# Simple text cleaning
clean_text <- function(text) {
  text <- str_to_lower(text)
  text <- str_replace_all(text, "[^a-zA-Z0-9 ]", "")  # Remove punctuation, keep spaces
  text <- str_squish(text)  # Collapse multiple spaces into single space
  return(text)
}

abstracts_clean <- sapply(abstracts, clean_text)

abstracts_df <- df_clean %>%
  mutate(
    abstract_clean = sapply(abstract, clean_text)
  ) %>%
  select(title, abstract = abstract_clean) %>%
  mutate(abstract_id = row_number()) %>%
  select(abstract_id, title, abstract)

abstracts_sentences <- abstracts_df %>%
  unnest_tokens(sentence, abstract, token = "sentences") %>%
  filter(!is.na(sentence), sentence != "")  # Remove empty sentences

write_csv(abstracts_sentences, "processed/sentences.csv")


# Create keyword tag ------------------------------------------------------
apply_pico_keywords <- function(sentences_file, output_file) {
  sentences <- read_csv(sentences_file)
# Define the keywords for each category
keywords <- list(
  Population = c("protected area", "forest", "agroforestry", "grazing", "communities", "smallholder farmers"),
  Intervention = c("reforestation", "restoration", "afforestation", "agroforestry", "selective logging", "sustainability certification"),
  # Comparator = c("no change", "different management", "no intervention", "time", "baseline", "different location"),
  Outcome = c("biodiversity", "carbon sequestration", "species richness", "biomass", "soil organic carbon", "yield")
)

detect_category <- function(text, category_keywords) {
  matches <- sapply(category_keywords, function(kws) {
    any(str_detect(str_to_lower(text), paste0("\\b", kws, "\\b")))
  })
  categories <- names(category_keywords)[matches]
  if (length(categories) == 0) "Unknown" else paste(categories, collapse = ";")
}

sentences <- sentences %>%
  rowwise() %>%
  mutate(initial_label = detect_category(sentence, keywords))

write_csv(sentences, output_file)
cat("Keyword-matched labels saved to:", output_file, "\n")
}

apply_pico_keywords("processed/sentences.csv", "sentences_labeled.csv")

# -------------------------
# 3. Train SVM classifier to predict PICO on sentences
# -------------------------
train_svm_classifier <- function(labeled_sentences_file, model_output) {
  labeled_sentences <- read_csv(labeled_sentences_file)
  
  # Filter out 'Unknown' to train only on known
  train_data <- labeled_sentences %>% filter(initial_label != "Unknown")
  
  it <- itoken(train_data$sentence, progressbar = FALSE)
  vectorizer <- vocab_vectorizer(create_vocabulary(it, stopwords = stopwords("en")))
  dtm <- create_dtm(it, vectorizer)
  
  # We'll just use first category if multiple
  y <- sapply(str_split(train_data$initial_label, ";"), function(x) x[1])
  
  svm_model <- svm(x = as.matrix(dtm), y = as.factor(y), kernel = "linear", probability = TRUE)
  saveRDS(list(model = svm_model, vectorizer = vectorizer), model_output)
  cat("SVM classifier saved to:", model_output, "\n")
}

train_svm_classifier("sentences_labeled.csv", "processed/svm_pico_model.rds")

# -------------------------
# 4. Predict new sentences using trained model
# -------------------------
predict_pico_sentences <- function(new_sentences_file, svm_model_file, output_file) {
  new_sentences <- read_csv(new_sentences_file)
  model_list <- readRDS(svm_model_file)
  
  it <- itoken(new_sentences$sentence, progressbar = FALSE)
  dtm <- create_dtm(it, model_list$vectorizer)
  
  preds <- predict(model_list$model, as.matrix(dtm))
  
  new_sentences$predicted_label <- preds
  write_csv(new_sentences, output_file)
  cat("Predictions saved to:", output_file, "\n")
}

predict_pico_sentences("processed/sentences.csv", "processed/svm_pico_model.rds", "predictions.csv")

# -------------------------
# 5. Aggregate sentence-level predictions into abstract-level PICO summary
# -------------------------
aggregate_pico_per_abstract <- function(predictions_file, output_file) {
  predictions <- read_csv(predictions_file)
  
  # For each abstract, concatenate unique sentences matching each PICO category
  pico_summary <- predictions %>%
    group_by(abstract_id) %>%
    summarise(
      Population = paste(unique(sentence[predicted_label == "Population"]), collapse = " | "),
      Intervention = paste(unique(sentence[predicted_label == "Intervention"]), collapse = " | "),
      Outcome = paste(unique(sentence[predicted_label == "Outcome"]), collapse = " | "),
      .groups = "drop"
    )
  
  write_csv(pico_summary, output_file)
  cat("Abstract-level PICO summary saved to:", output_file, "\n")
}

aggregate_pico_per_abstract("predictions.csv", "abstract_pico_summary.csv")


aggregate_pico_with_metadata <- function(predictions_file, abstract_metadata_file, output_file) {
  predictions <- read_csv(predictions_file)
  metadata <- read_csv(abstract_metadata_file)
  
  # Define the keywords for each category again
  keywords <- list(
    Population = c("protected area", "forest", "agroforestry", "grazing", "communities", "smallholder farmers"),
    Intervention = c("reforestation", "restoration", "afforestation", "agroforestry", "selective logging", "sustainability certification"),
    Outcome = c("biodiversity", "carbon sequestration", "species richness", "biomass", "soil organic carbon", "yield")
  )
  
  # Function to extract matched keywords from sentence
  extract_keywords <- function(text, keyword_list) {
    matches <- unlist(lapply(keyword_list, function(k) {
      if (str_detect(str_to_lower(text), paste0("\\b", k, "\\b"))) return(k)
      else return(NULL)
    }))
    return(unique(matches))
  }
  
  # Extract keywords per sentence + predicted label
  predictions_keywords <- predictions %>%
    rowwise() %>%
    mutate(
      matched_terms = paste(extract_keywords(sentence, keywords[[predicted_label]]), collapse = ";")
    ) %>%
    filter(matched_terms != "") %>%
    ungroup()
  
  # Aggregate unique keywords per abstract per PICO category
  pico_summary <- predictions_keywords %>%
    group_by(abstract_id, predicted_label) %>%
    summarise(keywords = paste(unique(unlist(str_split(matched_terms, ";"))), collapse = " | "), .groups = "drop") %>%
    pivot_wider(names_from = predicted_label, values_from = keywords, values_fill = "")
  
  # Merge back metadata (title + abstract)
  final_table <- metadata %>%
    left_join(pico_summary, by = "abstract_id") %>%
    select(title, abstract, Population, Intervention, Outcome)
  
  write_csv(final_table, output_file)
  cat("Final Title + Abstract + PICO table (phrases only) saved to:", output_file, "\n")
}

aggregate_pico_with_metadata("predictions.csv", "processed/abstract_metadata.csv", "abstract_pico_final.csv")
