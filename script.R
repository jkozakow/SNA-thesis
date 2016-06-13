library(igraph)
library(plyr)
slash_table <- read.table("Slashdot0902.txt")

slash_graph <- graph.data.frame(slash_table, directed = T)
slash_deg <- degree(slash_graph, mode="all")
slash_dd <- degree.distribution(slash_graph, cumulative = T, mode="all")
slash_dd_in <- degree.distribution(slash_graph, cumulative = T, mode="in")
slash_dd_out <- degree.distribution(slash_graph, cumulative = T, mode="out")
slash_wcc = clusters(slash_graph, mode="weak")
slash_scc = clusters(slash_graph, mode="strong")
slash_wcc_dist <- count(slash_scc$csize)
slash_scc_dist <- count(slash_scc$csize)

slash_closeness <- closeness(slash_graph, mode = "all", normalized = TRUE)

random_nodes_vector <- sample(c(0,1), prob = c(0.85, 0.15), replace=TRUE, size = length(V(slash_graph)))
slash_graph_RN <- delete_vertices(slash_graph, which(random_nodes_vector==0)) #Random Nodes
plot(density(slash_closeness))

slash_closeness_RN <- closeness(slash_graph_RN, mode = "all", normalized = TRUE)
plot(density(slash_closeness_RN))

