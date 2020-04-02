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
  left_join(papers, by = c("name" = "DOI"))

four_core_fa <- vsp(four_core, k = 25)

screeplot(four_core_fa) +
  ylim(0.4, 0.7)

# TODO: add the BFF function

# consider the 4 core
coreness(graph)

