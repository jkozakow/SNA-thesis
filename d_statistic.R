d_statistic = function(source_graph, sample_graph){
  #Degree-All Distribution
  source_graph_dd <- degree.distribution(source_graph, cumulative = T, mode="all")
  sample_graph_dd <- degree.distribution(sample_graph, cumulative = T, mode="all")
  test_degree_all <- ks.test(source_graph_dd, sample_graph_dd)
  print("Degree-All")
  print(test_degree_all$statistic)
  
  #Degree-In Distribution
  source_dd_in <- degree.distribution(source_graph, cumulative = T, mode="in")
  sample_dd_in <- degree.distribution(sample_graph, cumulative = T, mode="in")
  test_degree_in <- ks.test(source_dd_in, sample_dd_in)
  print("Degree-In")
  print(test_degree_in$statistic)
  
  #Degree-Out Distribution
  source_dd_out <- degree.distribution(source_graph, cumulative = T, mode="out")
  sample_dd_out <- degree.distribution(sample_graph, cumulative = T, mode="out")
  test_degree_out <- ks.test(source_dd_out, sample_dd_in)
  print("Degree-Out")
  print(test_degree_out$statistic)
  
  #Weakly Connected Components Distribution
  source_wcc = cluster.distribution(source_graph, mode="weak", cumulative=TRUE)
  sample_wcc = cluster.distribution(sample_graph, mode="weak", cumulative=TRUE)
  test_wcc <- ks.test(source_wcc, sample_wcc)
  print("Weakly Connected Components")
  print(test_wcc$statistic)
  
  #Strongly Connected Components Distribution
  source_scc = cluster.distribution(source_graph, mode="strong", cumulative=TRUE)
  sample_scc = cluster.distribution(sample_graph, mode="strong", cumulative=TRUE)
  test_scc <- ks.test(source_scc, sample_scc)
  print("Strongly Connected Components")
  print(test_scc$statistic)
  
  #Clustering Coefficient Distribution
  source_cd <- transitivity(source_graph, type="local")
  sample_cd <- transitivity(sample_graph, type="local")
  test_cd <- ks.test(source_cd, sample_cd)
  print("Clustering Coefficient")
  print(test_cd$statistic)
}