
##### documentation from the original README

# authorList.txt      -- the complete list of 3607 author names
#
# authorPaperBiadj.txt-- the bipartite (authorship) adjacency matrix between authors(rows)
# and papers (columns); the element at (i,j) is 1 iff author i
# is the author or one of the coauthors of paper j, and 0 otherwise
#
# paperCitAdj.txt     -- the 3248x3248 adjacency matrix for citations between papers;
# the element at (i,j) is 1 iff paper i is cited by paper j, and 0 otherwise
#
# paperList.txt       -- the complete list of 3248 papers including DOI, year, title, citation count and abstract  for each

##### load and clean

library(here)
library(tidyverse)
library(Matrix)
library(usethis)

authors <- read_tsv(
  here("data-raw/SCC2016/Data/authorList.txt"),
  col_names = "author"
)

bipartite_df <- read_tsv(
  here("data-raw/SCC2016/Data/authorPaperBiadj.txt"),
  col_names = FALSE
)

bipartite <- bipartite_df %>%
  as.matrix() %>%
  Matrix()

citations_df <- read_table(
  here("data-raw/SCC2016/Data/paperCitAdj.txt"),
  col_names = FALSE
)

citations <- citations_df %>%
  as.matrix() %>%
  Matrix()

papers <- read_csv(
  here("data-raw/SCC2016/Data/paperList.txt")
)

rownames(citations) <- papers$DOI
colnames(citations) <- papers$DOI

use_data(citations, papers, overwrite = TRUE)
