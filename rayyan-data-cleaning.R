# Load necessary libraries
library(dplyr)
library(stringr)
library(tidyr)

# Read the CSV file (adjust file path if needed)
df <- read.csv("search-database-result-copies/Griscom-7/grisom-7-full-final.csv")

# Check the first few rows of the data to verify column names
head(df)

# Function to extract inclusion, labels, notes, and exclusion reasons from the 'notes' column
extract_info <- function(text) {
  # Ensure the text is treated as a string (in case of NA or non-character data)
  text <- as.character(text)
  
  # Initialize variables
  inclusion <- NA
  labels <- NA
  notes <- NA
  exclusion_reasons <- NA
  
  # Extract inclusion decision (e.g., Included, Excluded, Maybe)
  inclusion_match <- str_match(text, 'RAYYAN-INCLUSION:\\s*\\{"[A-Za-z ]+"\\s*=>\\s*"([^"]+)"')
  if (!is.na(inclusion_match[2])) inclusion <- inclusion_match[2]
  
  # Extract labels (everything after RAYYAN-LABELS:)
  labels_match <- str_match(text, 'RAYYAN-LABELS:\\s*([^|]+)')
  if (!is.na(labels_match[2])) labels <- labels_match[2]
  
  # Extract user notes (if present) and remove surrounding quotes
  notes_match <- str_match(text, 'USER-NOTES:\\s*\\{"[A-Za-z ]+"\\s*=>\\s*\\[([^\\]]+)\\]')
  if (!is.na(notes_match[2])) {
    # Remove surrounding quotes if present
    notes <- str_replace_all(notes_match[2], '^"|"$', '')  # Remove leading and trailing quotes
  }  
  
  # Extract exclusion reasons (if present)
  exclusion_match <- str_match(text, 'RAYYAN-EXCLUSION-REASONS:\\s*([^|]+)')
  if (!is.na(exclusion_match[2])) exclusion_reasons <- exclusion_match[2]
  
  # Return a list of extracted values (as a named list)
  return(list(inclusion = inclusion, labels = labels, notes = notes, exclusion_reasons = exclusion_reasons))
}

# Apply the function to the 'notes' column to extract inclusion, labels, notes, and exclusion reasons
extracted_info <- lapply(df$notes, extract_info)

# Convert the extracted info to a data frame
extracted_df <- do.call(rbind, lapply(extracted_info, as.data.frame))

# Add the extracted columns to the original data frame
df$inclusion <- extracted_df$inclusion 
df$labels <- extracted_df$labels 
df$notes <- extracted_df$notes
df$exclusion_reasons <- extracted_df$exclusion_reasons 


# View the final cleaned data
head(df)

# data-cleaning -----------------------------------------------------------
df <- df %>%
  separate(labels, into = c("labels1", "labels2", "labels3"), sep = ",", fill = "right", extra = "drop") %>%
  mutate(across(starts_with("labels"), ~ str_trim(.)))  # Trim spaces from split parts

# Clean & split exclusion_reason
df <- df %>%
  mutate(exclusion_reasons = str_squish(exclusion_reasons),
         exclusion_reasons = str_replace_all(exclusion_reasons, ",\\s*", ","),
         exclusion_reasons = str_replace_all(exclusion_reasons, "\\s*,", ",")) %>%
  separate(exclusion_reasons, into = c("exclusion_reasons1", "exclusion_reasons2"), sep = ",", fill = "right", extra = "drop") %>%
  mutate(across(starts_with("exclusion_reasons"), ~ str_trim(.)))

library(dplyr)
library(stringr)

# Specify the columns to clean
cols_to_clean <- c("inclusion", "labels1", "labels2", "labels3", "exclusion_reasons1", "exclusion_reasons2")

# Apply cleaning only to those columns
df_clean <- df %>%
  mutate(across(all_of(cols_to_clean), ~ str_trim(.))) %>%      # Remove leading/trailing spaces
  mutate(across(all_of(cols_to_clean), ~ str_to_lower(.))) %>%  # Convert to lowercase
  mutate(across(all_of(cols_to_clean), ~ na_if(., "na"))) %>%       # Convert "na" (as text) to actual NA
  mutate(across(all_of(cols_to_clean), ~ na_if(., "#n/a")))

#Save the final cleaned data to a new CSV file
write.csv(df_clean, "search-database-result-copies/Griscom-7/Griscom-7-rayyan-export-completed/articles-griscom7-completed-rayyanexport-cleaned.csv")

# Columns to combine
cols_to_count <- c("labels1", "labels2", "labels3")


# Combine + Count
category_counts <- df %>%
  filter(inclusion == c("Included")) %>% 
  select(all_of(cols_to_count)) %>%
  pivot_longer(cols = everything(), names_to = "source_column", values_to = "category") %>%
  filter(!is.na(category)) %>%
  count(category, sort = TRUE)

# View result
category_counts

df %>%
  count(inclusion, sort = TRUE)
