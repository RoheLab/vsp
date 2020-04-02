library(vsp)
library(igraph)
library(tidygraph)

citations
papers

graph <- graph_from_adjacency_matrix(citations)

fa <- vsp(graph, k = 25)

screeplot(fa) +
  ylim(0.5, 0.8)

# 636 papers in t
four_core <- graph %>%
  as_tbl_graph() %>%
  mutate(
    coreness = node_coreness(mode = "all")
  ) %>%
  filter(coreness >= 4) %>%
  left_join(papers, by = c("name" = "DOI")) %>%
  filter(!is.na(abstract))

four_core_fa <- vsp(four_core, k = 25)

screeplot(four_core_fa) +
  ylim(0.4, 0.7)

# get the features matrix

library(tm)
library(tidytext)

# don't mind the eigengap -- remove words that appear in fewer than 10
# documents

dtm <- four_core %>%
  activate(nodes) %>%
  as_tibble() %>%
  unnest_tokens(token, abstract, token = "tweets") %>%
  group_by(name) %>%
  count(token) %>%
  cast_sparse(name, token, n)

frequent_words <- colSums(sign(dtm)) > 10

length(frequent_words)
sum(frequent_words)

dtm_freq <- dtm[, which(frequent_words)]

bff(four_core_fa$Z, dtm_freq, 5)

dim(four_core_fa$Z)
dim(dtm_freq)

papers %>%
  filter(!is.na)
  count(is.na(abstract))

tm

# consider the 4 core
coreness(graph)

