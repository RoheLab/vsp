library(igraph)

set.seed(27)

pm <- cbind(
  c(.1, .001),
  c(.001, .05)
)

sbm_igraph <- sample_sbm(1000, pref.matrix = pm, block.sizes = c(300, 700))
sbm_adjacency_matrix <- get.adjacency(sbm_igraph)

# weighted edge example

usethis::use_data(sbm_igraph, sbm_adjacency_matrix, overwrite = TRUE)
