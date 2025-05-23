library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
library(dplyr)
library(litsearchr)

# native_result <- import_results(directory = "Griscom-2020-2_24032025")
# native_result <- c(
#   "search-database-result-copies/Griscom-2020-4-refined_29042025/griscom_4ref_savedrecs.ris",
#   "search-database-result-copies/Griscom-2020-4-refined_29042025/griscom_4ref_savedrecs_1.ris",
#   "search-database-result-copies/Griscom-2020-4-refined_29042025/griscom_4ref_scopus.ris"
# ) %>%
#   map_dfr(read_refs)

# naive_result_clean <- remove_duplicates(df = native_result, field = "doi", method = "exact") 
# naive_result_clean <- remove_duplicates(df = naive_result_clean, field = "title", method = "exact")
naive_result_clean <- read_csv("search-database-result-copies/Griscom-7/Griscom-7-rayyan-export-completed/articles-griscom7-completed-rayyanexport-cleaned.csv") %>% 
  filter(!is.na(title) & title != "" & !is.na(abstract) & abstract != "") %>% 
  dplyr::filter(inclusion == "included")

titles_naive <- paste(naive_result_clean$title)
keyword_naive <- paste(naive_result_clean$keywords)

clinpsy_stopwords <- read_lines("keywords1.txt")
all_stopwords <- unique(c(get_stopwords("English"), clinpsy_stopwords))

raked_keywords <- extract_terms(
  text = titles_naive,
  method = "fakerake",min_freq=3, min_n=2,
  stopwords=all_stopwords
)

# real_keywords <- extract_terms(keywords=naive_result_clean[,"keywords"], method="tagged", min_n=1)

# all_keywords <- unique(c(raked_keywords, real_keywords))

docs <- paste(naive_result_clean$title, naive_result_clean$abstract)

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

# write.csv(search_terms, "search_terms_Griscom-2020-2.csv")
# write.csv(term_strengths, "terms_strengths_Griscom-2020-2.csv")
