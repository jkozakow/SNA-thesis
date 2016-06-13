random_walk_sample = function(graph, sample_size = 0.05, fly_back = FALSE) {
  sample_size <- sample_size * length(V(graph))
  list_to_create_graph <- vector()
  previous_list <- vector()
  source_node <- sample(V(graph)$name, 1)
  stuck_count <- 0
  current_node <- source_node
  while(length(list_to_create_graph) < sample_size){
    if (setequal(previous_list, list_to_create_graph)){
      stuck_count <- stuck_count + 1
    }
    else{}
    previous_list <- list_to_create_graph
    if(!(current_node %in% list_to_create_graph)){
      list_to_create_graph <- append(list_to_create_graph, current_node)
    }
    else{}
    #print(list_to_create_graph)
    if(fly_back){
      to_go_or_not_to_go <- sample(x = c(TRUE, FALSE), size = 1, prob = c(0.85, 0.15))
    }
    else{
      to_go_or_not_to_go <- TRUE
    }
    if(to_go_or_not_to_go){
      node_ego_graph <- induced.subgraph(graph,vids=unlist(neighborhood(graph,order=1,nodes=current_node)))
      random_neighbour <- sample(V(node_ego_graph)[name !=current_node]$name, 1)
      current_node <- random_neighbour
    }
    else{
      current_node <- source_node
    }

    neighbourhood_size <- neighborhood.size(graph, order = 1, nodes=V(graph)[current_node], mode="all")
    if (stuck_count > neighbourhood_size * 10){
      current_node <- sample(V(graph)$name, 1)
      stuck_count <- 0
    }
    else{}
    #print(list_to_create_graph)
    print(paste(length(list_to_create_graph)))
  }
  print(list_to_create_graph)
  sample_graph <- induced_subgraph(graph, v = list_to_create_graph)
  return(sample_graph)
}

random_jump_sample = function(graph, sample_size = 0.05) {
  sample_size <- sample_size * length(V(graph))
  list_to_create_graph <- vector()
  previous_list <- vector()
  source_node <- sample(V(graph)$name, 1)
  stuck_count <- 0
  current_node <- source_node
  while(length(list_to_create_graph) < sample_size){
    if (setequal(previous_list, list_to_create_graph)){
      stuck_count <- stuck_count + 1
    }
    else{}
    previous_list <- list_to_create_graph
    if(!(current_node %in% list_to_create_graph)){
      list_to_create_graph <- append(list_to_create_graph, current_node)
    }
    else{}
    #print(list_to_create_graph)
    
    to_jump_or_not_to_jump <- sample(x = c(TRUE, FALSE), size = 1, prob = c(0.85, 0.15))
    
    if(!to_jump_or_not_to_jump){
      node_ego_graph <- induced.subgraph(graph,vids=unlist(neighborhood(graph,order=1,nodes=current_node)))
      random_neighbour <- sample(V(node_ego_graph)[name !=current_node]$name, 1)
      current_node <- random_neighbour
    }
    else{
      current_node <- sample(V(graph)$name, 1)
    }
    
    neighbourhood_size <- neighborhood.size(graph, order = 1, nodes=V(graph)[current_node], mode="all")
    if (stuck_count > neighbourhood_size * 10){
      current_node <- sample(V(graph)$name, 1)
      stuck_count <- 0
    }
    else{}
    #print(list_to_create_graph)
    print(paste(length(list_to_create_graph)))
  }
  sample_graph <- induced_subgraph(graph, v = list_to_create_graph)
  return(sample_graph)
}


random_node_sample = function(graph, sample_size = 0.15){
  random_nodes_vector <- sample(c(0,1), prob = c(1 - sample_size, sample_size), replace=TRUE, size = length(V(graph)))
  sample_graph <- delete_vertices(graph, which(random_nodes_vector==0)) #Random Nodes
  return(sample_graph)
}

random_page_rank_node_sample = function(graph, sample_size = 0.15){
  sample_len <- sample_size * length(V(graph))
  page_rank <- page.rank(graph, vids = V(graph))
  nodes_vector <- sample(V(graph)$name, prob = page_rank$vector, replace=FALSE, size = sample_len)
  sample_graph <- induced.subgraph(graph=graph, vids=nodes_vector)
  return(sample_graph)
}


forest_fire_sample = function(graph, sample_size = 0.15, pf = 0.7){
  list_to_delete_from_graph <- vector()
  visited_list <- vector() 
  to_cut <- length(V(graph)) * (1 - sample_size)
  while(length(list_to_delete_from_graph) < to_cut){
    current_node <- sample(V(graph)$name, 1)
    list_to_delete_from_graph <- ff_spread(graph, current_node, visited_list = list_to_delete_from_graph, pf=pf, pb=pb, to_cut=to_cut)
  }
  
  sample_graph <- delete_vertices(graph, list_to_delete_from_graph)
  return(sample_graph)
}
ff_spread = function(graph, current_node, pf = 0.7, pb = 0, visited_list, to_cut){
  if (!(current_node %in% visited_list)){
    visited_list <- append(visited_list, current_node)
  }
  if(length(visited_list) > to_cut){
    return(visited_list)
  }
  node_ego_graph <- induced.subgraph(graph,vids=unlist(neighborhood(graph,order=1,nodes=current_node)))
  neighbours <- V(node_ego_graph)[name != current_node && !(name %in% visited_list)]$name
  geom_prob <- geom_distr(pf)
  neighbours_vector <- 0:(length(geom_prob)-1)
  neighbours_num <- sample(x = neighbours_vector, prob = geom_prob, size = 1, replace = FALSE)
  no_visit_neighbours <- neighbours[!neighbours %in% visited_list]
  if ((length(no_visit_neighbours)>0) && (neighbours_num>0) && (length(no_visit_neighbours) > neighbours_num)){
    burnt_neighbours <- sample(x = no_visit_neighbours, size = neighbours_num)
    visited_list <- append(visited_list, burnt_neighbours)
  }
  else if((length(no_visit_neighbours>0)) && (neighbours_num>0) && (length(no_visit_neighbours) <= neighbours_num)){
    burnt_neighbours <- neighbours
  }
  else{
    burnt_neighbours <- c()
  }
  
  for (neighbour in burnt_neighbours){
    visited_list <- ff_spread(graph=graph, current_node = neighbour, visited_list = visited_list, to_cut=to_cut, pf=pf, pb=pb)
  }
  
  return(visited_list)
}

geom_distr = function(pf){
  mean = pf/(1-pf)
  y = 0:(2*round(mean))
  py = dgeom(y,pf)
  return(py)
}