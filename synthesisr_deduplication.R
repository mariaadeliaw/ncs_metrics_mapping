# based on https://rpubs.com/Linh-LTP/1039346

pacman::p_load(rio,
               here,
               dplyr,
               magrittr,
               tidyr,
               metagear,
               bibliometrix,
               rscopus, 
               synthesisr, 
               revtools,
               data.table)
library(synthesisr)
# WOS
bibfiles_allfields <- list.files(
  "search-database-result-copies/Griscom-7/wos/",
  full.names = TRUE
)

imported_files_allfields <- read_refs(
  filename = bibfiles_allfields,
  return_df = TRUE)

df_doi_allfields <- deduplicate(
  imported_files_allfields,
  match_by = "doi",
  method = "exact"
)

df_title_allfields <- deduplicate(
  df_doi_allfields,
  match_by = "title",
  method = "exact"
)

a_wos <- df_title_allfields

#scopus
bibfiles_allfields <- list.files(
  "search-database-result-copies/Griscom-7/Scopus/",
  full.names = TRUE
)

imported_files_allfields <- read_refs(
  filename = bibfiles_allfields,
  return_df = TRUE)

df_doi_allfields <- deduplicate(
  imported_files_allfields,
  match_by = "doi",
  method = "exact"
)

df_title_allfields <- deduplicate(
  df_doi_allfields,
  match_by = "title",
  method = "exact"
)

a_scopus <- df_title_allfields


a_df <- rbindlist(list(a_wos, a_scopus), fill = TRUE)

# depublication 
df_doi_m <- deduplicate(
  a_df,
  match_by = "doi",
  method = "exact"       # 3571
)

df_title_m <- deduplicate(
  df_doi_m,
  match_by = "title",
  method = "exact"       # 3457
)

df_title_m.1 <- deduplicate(
  df_title_m, 
  "title",
  method = "string_osa",
  rm_punctuation = TRUE,
  to_lower = TRUE)       # 3386 

# Find duplicate values in a dataframe 
dups.2 <- df_doi_m[duplicated(df_doi_m$doi)|duplicated(df_doi_m$doi, fromLast=TRUE),]

# write_refs(df_title_m.1, format = "bib", tag_naming = "synthesisr", file = "search-database-result-copies/Griscom-7/dedup_synthesisr.bib")

