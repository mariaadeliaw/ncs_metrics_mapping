library(revtools)    # import and manage bibliographic data :contentReference[oaicite:0]{index=0}  
library(ASySD)       # automated deduplication functions :contentReference[oaicite:1]{index=1}  
library(dplyr)       # data manipulation verbs :contentReference[oaicite:2]{index=2}  
library(readr)       # fast CSV import/export :contentReference[oaicite:3]{index=3}  
library(synthesisr)
library(purrr)
library(litsearchr)
library(fuzzyjoin)                                                           # fuzzy matching :contentReference[oaicite:14]{index=14}  
library(igraph)
library(ggplot2)
library(ggraph)
library(VennDiagram)

# dedup on R
griscom <- read_refs("search-database-result-copies/Griscom-2020-3_31032025/griscom-2020-3_zotero/griscom-2020-3_zotero.ris")   # reads into a data frame :contentReference[oaicite:4]{index=4}  
# combined_new_search <- c(
#   "search-database-result-copies/Griscom-2020-4-refined_29042025/griscom_4ref_savedrecs.ris",
#   "search-database-result-copies/Griscom-2020-4-refined_29042025/griscom_4ref_savedrecs_1.ris",
#   "search-database-result-copies/Griscom-2020-4-refined_29042025/griscom_4ref_scopus.ris"
# ) %>%
#   map_dfr(read_refs) 

griscom_dedup <- dedup_citations(griscom, merge_citations = TRUE, user_input = 1)$unique   # unique Griscom entries :contentReference[oaicite:6]{index=6}
# new_dedup    <- dedup_citations(combined_new_search, merge_citations = TRUE, user_input = 1)$unique   # unique new-search entries :contentReference[oaicite:7]{index=7}  

#pre-dedup
# new_dedup_zotero <- read_refs("search-database-result-copies/Griscom-2020-4-refined_29042025/griscom_4ref_dedup_zotero_v2.ris")
new_dedup_rayyan <- read_ref("search-database-result-copies/Griscom-7/dedup_synthesisr.ris")

# combined <- bind_rows(griscom_dedup, new_dedup)                              # combine both lists :contentReference[oaicite:8]{index=8}  
# combined_dedup <- dedup_citations(combined, merge_citations = TRUE)$unique   # remove duplicates across sets :contentReference[oaicite:9]{index=9}  

# Ensure DOI columns are consistently named, e.g., "doi"
griscom_dois <- select(griscom_dedup, doi)                                     # original DOIs :contentReference[oaicite:11]{index=11}  
unique_new <- anti_join(new_dedup_rayyan, griscom_dois, by = "doi")                    # exclude matching DOIs :contentReference[oaicite:12]{index=12}  
# write_csv(unique_new, "search-database-result-copies/Griscom-7/Unique_New_Records.csv")                                # final unique records :contentReference[oaicite:13]{index=13}

no_doi_new <- filter(new_dedup_rayyan, is.na(doi)) %>%                                    # records lacking DOI :contentReference[oaicite:15]{index=15}  
  tidyr::drop_na(title)
no_doi_old <- filter(griscom_dedup, is.na(doi))                               # original records lacking DOI :contentReference[oaicite:16]{index=16}  

# compute pairwise title distances and set threshold
matches_clean <- stringdist_inner_join(no_doi_new, no_doi_old, by = "title", max_dist = 2)  
fuzzy_unique <- anti_join(no_doi_new, matches_clean, by = c("title" = "title.x"))
final_unique <- bind_rows(unique_new, fuzzy_unique)                            # combine DOI and title uniques  
# write_csv(final_unique, "search-database-result-copies/Griscom-7/Unique_New_Records.csv")                              # comprehensive unique set
write_refs(final_unique, format = "bib", tag_naming = "synthesisr",  file = "search-database-result-copies/Griscom-7/griscom7-final-unique.bib")
# 1. Compare records WITH DOI from old but not in new
new_dois <- select(new_dedup_rayyan, doi)  # extract DOIs from new search
unique_old <- anti_join(griscom_dedup, new_dois, by = "doi")  # old records not in new by DOI

fuzzy_unique_old <- anti_join(no_doi_old, matches_clean, by = c("title" = "title.x"))

# 3. Combine old-unique results
final_unique_old <- bind_rows(unique_old, fuzzy_unique_old)

# 4. Export
# write_csv(final_unique_old, "Unique_Old_Records_Not_in_New.csv")

# Combine both sets of DOIs and titles to determine overlap
dois_old <- griscom_dedup %>% filter(!is.na(doi)) %>% pull(doi) %>% unique()
dois_new <- new_dedup_rayyan %>% filter(!is.na(doi)) %>% pull(doi) %>% unique()

titles_old <- griscom_dedup %>% filter(is.na(doi)) %>% pull(title) %>% unique()
titles_new <- new_dedup_rayyan %>% filter(is.na(doi)) %>% pull(title) %>% unique()

# Overlap computation (DOI intersection + fuzzy title matches)
doi_overlap <- intersect(dois_old, dois_new)
fuzzy_title_overlap <- matches_clean$title.x %>% unique()

# Counts
n_old_only <- length(setdiff(dois_old, doi_overlap)) + length(setdiff(titles_old, fuzzy_title_overlap))
n_new_only <- length(setdiff(dois_new, doi_overlap)) + length(setdiff(titles_new, fuzzy_title_overlap))
n_both     <- length(doi_overlap) + length(fuzzy_title_overlap)
# Venn Diagram
venn.plot <- draw.pairwise.venn(
  area1 = n_old_only + n_both,
  area2 = n_new_only + n_both,
  cross.area = n_both,
  category = c("Old Search", "New Search"),
  fill = c("skyblue", "lightgreen"),
  alpha = c(0.6, 0.6),
  cat.pos = c(-20, 20),
  cat.dist = 0.05,
  scaled = T
)

# Check themes of the new search string in a glance using litsearc --------

## what is the new publications in the new search string mainly talks about? use litsearchr
titles_naive <- paste(final_unique_old$title)

clinpsy_stopwords <- read_lines("keywords1.txt")
all_stopwords <- unique(c(get_stopwords("English"), clinpsy_stopwords))

raked_keywords <- extract_terms(
  text = titles_naive,
  method = "fakerake",min_freq=3, min_n=2,
  stopwords=all_stopwords
)

# real_keywords <- extract_terms(keywords=naive_result_clean[,"keywords"], method="tagged", min_n=1)

# all_keywords <- unique(c(raked_keywords, real_keywords))
final_unique_old_filtered <- final_unique_old %>%
  filter(!is.na(title) & title != "" & !is.na(abstract) & abstract != "")

# Create docs variable
docs <- paste(final_unique_old_filtered$title, final_unique_old_filtered$abstract)

dfm <- create_dfm(elements=docs, features=raked_keywords)
g <- create_network(dfm, min_studies=3)

ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)

strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

cutoff_cum <- find_cutoff(g, method="cumulative", percent=0.9)

cutoff_cum

cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")

search_terms <- get_keywords(reduce_graph(g, cutoff_cum))

# write.csv(search_terms, "search_terms_Griscom-2020-4.csv")
# write.csv(term_strengths, "terms_strengths_Griscom-2020-4.csv")
