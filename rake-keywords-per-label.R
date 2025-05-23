# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(tidyr)
library(textrank)   # For RAKE-style keyword extraction
library(stopwords)

# Read the cleaned CSV file
df <- read.csv("cleaned_rayyan_final.csv", header = TRUE)

# Combine abstract and title columns into one text column
df <- df %>%
  mutate(text = paste(title, abstract))

# Separate included and excluded data
included_df <- df %>% filter(inclusion == "Included")
excluded_df <- df %>% filter(inclusion == "Excluded")

# ===============================
# Step 1: Label frequency graphs
# ===============================

# Included articles - label frequency
included_labels <- included_df %>%
  separate_rows(labels, sep = ",") %>%
  count(labels) %>%
  arrange(desc(n)) %>%
  na.omit()

ggplot(included_labels, aes(x = reorder(labels, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Label Frequency in Included Articles", x = "Labels", y = "Frequency")

# Excluded articles - reason frequency
excluded_reasons <- excluded_df %>%
  count(exclusion_reasons) %>%
  arrange(desc(n)) %>%
  na.omit()

ggplot(excluded_reasons, aes(x = reorder(exclusion_reasons, n), y = n)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() +
  labs(title = "Exclusion Reasons Frequency", x = "Reason", y = "Frequency")

# =============================================
# Step 2: RAKE-like keyword analysis with textrank
# =============================================

# Ensure stopwords are loaded
data("stop_words")

# Step 1: Combine title + abstract if not already
included_df <- included_df %>%
  mutate(text = paste(title, abstract, sep = " "))

# Tokenize and filter relevant words per document
relevant_words <- included_df %>%
  mutate(doc_id = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "[[:punct:]]")) %>%
  group_by(doc_id) %>%
  summarise(relevant = list(unique(word)), .groups = "drop")

# Make sure relevant_words is in the same order as included_df
included_df <- included_df %>%
  mutate(doc_id = row_number()) %>%
  left_join(relevant_words, by = "doc_id")

# Extract keywords using textrank
included_df$keywords <- purrr::map2_chr(
  included_df$text,
  included_df$relevant,
  ~ {
    tryCatch({
      keywords <- textrank_keywords(x = .x, relevant = .y)
      paste(keywords$keywords$keyword, collapse = ", ")
    }, error = function(e) NA_character_)
  }
)

# View result
head(included_df$keywords, 10)

# Step 1: Unnest the comma-separated keywords
keyword_counts <- included_df %>%
  select(labels, relevant) %>%
  separate_rows(relevant, sep = ",\\s*") %>%  # split keywords into rows
  filter(!is.na(relevant), relevant != "") %>%
  group_by(labels, relevant) %>%
  summarise(count = n(), .groups = "drop")

# Step 2: Get top 10 keywords per label
top_keywords <- keyword_counts %>%
  group_by(labels) %>%
  slice_max(order_by = count, n = 10) %>%
  separate_rows(labels, sep = ",\\s*") %>%   # splits on commas and optional whitespace
  ungroup()

library(tidyr)
library(dplyr)

# Separate the labels into multiple rows
clean_keywords <- top_keywords %>%
  separate_rows(labels, sep = ",\\s*")  # splits on commas and optional whitespace

top_3_keywords <- clean_keywords %>%
  group_by(labels) %>%
  slice_max(order_by = count, n = 3, with_ties = FALSE) %>%
  summarise(top_keywords = paste(relevant, collapse = ", ")) %>%
  ungroup()

ggplot(top_3_keywords, aes(x = reorder(labels, -nchar(top_keywords)), y = top_keywords)) +
  geom_point(aes(size = nchar(top_keywords), color = "darkred"), alpha = 0.7) +
  scale_size_continuous(range = c(3, 10)) +  # Adjust size of points
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Top 3 Keywords for Each Label",
    x = "Labels",
    y = "Top Keywords",
    size = "Keyword Length"
  ) +
  theme(legend.position = "none")  # Hide legend

# Ensure labels are separated
keywords_long <- top_3_keywords %>%
  separate_rows(labels, sep = ",\\s*") %>%   # Split multiple labels into separate rows
  group_by(labels, top_keywords) %>%
  summarise(count = n(), .groups = "drop")    # Count occurrences of each keyword for each label

# Plot faceted word clouds
ggplot(keywords_long, aes(label = top_keywords, size = count)) +
  geom_text_wordcloud_area() +
  facet_wrap(~ labels) +                                # Facet by label
  scale_size_area(max_size = 5) +                      # Control the maximum size of words
  theme_minimal() +                                     # Minimal theme
  labs(title = "Most Common Keywords per Label")

write.csv(top_keywords, "top_keywords.csv")
