library(igraph)
library(plyr)
slash_table <- read.table("Slashdot0902.txt")

slash_graph <- graph.data.frame(slash_table, directed = T)
slash_sample_graph <- random_walk_sample(slash_graph)
d_statistic(slash_graph, slash_sample_graph)