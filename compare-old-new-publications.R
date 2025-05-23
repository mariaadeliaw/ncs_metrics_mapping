library(litsearchr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
library(dplyr)


# NEW-REF -----------------------------------------------------------------
new_ref <- read_ref("search-database-result-copies/Griscom-7/dedup_synthesisr.ris")

# all_keywords <- unique(c(raked_keywords, real_keywords))
clinpsy_stopwords <- read_lines("keywords1.txt")
all_stopwords <- unique(c(get_stopwords("English"), clinpsy_stopwords))
new_ref_title <- paste(new_ref$title)
new_ref_abs <- paste(new_ref$abstract)

raked_keywords <- extract_terms(
  text = new_ref_title,
  method = "fakerake",min_freq=3, min_n=2,
  stopwords=all_stopwords)

docs <- paste(new_ref_abs, new_ref_title)

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


# OLD-REF -----------------------------------------------------------------
old_ref <- read_refs("search-database-result-copies/Griscom-2020-3_31032025/griscom-2020-3_zotero/griscom-2020-3_zotero.ris")

old_ref_title <- paste(old_ref$title)
old_ref_abs <- paste(old_ref$abstract)

raked_keywords_old <- extract_terms(
  text = old_ref_title,
  method = "fakerake",min_freq=3, min_n=2,
  stopwords=all_stopwords)

docs_old <- paste(old_ref_title, old_ref_abs)

dfm_old <- create_dfm(elements=docs_old, features=raked_keywords_old)
g_old <- create_network(dfm_old, min_studies=3)

ggraph(g_old, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)

strengths_old <- strength(g_old)

data.frame(term=names(strengths_old), strength=strengths_old, row.names=NULL) %>%
  mutate(rank=rank(strengths_old, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths_old

term_strengths_old

cutoff_fig_old <- ggplot(term_strengths_old, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths_old, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig_old

cutoff_cum_old <- find_cutoff(g_old, method="cumulative", percent=0.9)

cutoff_cum_old

cutoff_fig +
  geom_hline(yintercept=cutoff_cum_old, linetype="dashed")

search_terms_old <- get_keywords(reduce_graph(g, cutoff_cum))

# Compare term_strength values
term_strength_common <- inner_join(term_strengths_old, term_strengths, by = "term")
term_strength_only_old <- anti_join(term_strengths_old, term_strengths, by = "term")
term_strength_only_new <- anti_join(term_strengths, term_strengths_old, by = "term")

# Combine for output
term_strength_comparison <- bind_rows(
  mutate(term_strength_common, source = "Both"),
  mutate(term_strength_only_old, source = "Old Only"),
  mutate(term_strength_only_new, source = "New Only")
)
write_csv(term_strength_comparison, "Term_Strength_Comparison_griscom-3vs4.csv")
