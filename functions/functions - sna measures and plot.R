

# Function 1 -- undirected centrality measures function -----

# sna measures undirected is a function that takes a list column data frame with graph objects for sex-specific dyads by year
# and calculates individual's network positions for betweenness, ec, degree, and transitivity
# the output of the function can be a data frame of observations of individual network score by year, or a sociogram ("graph")
# that produces a visualization of the network, with the sna measure defining the size of the vertex 

#tester
#g <- graph_from_data_frame(d = g_data_gm_sex_sep$data[[1]], directed = FALSE)


sna_measures_undir <- function(g, year = NULL, network_sex = NULL, bt_weight = TRUE, ec_weight = TRUE, deg_weight = TRUE, trans_weight = TRUE, output = c("graph", "data.frame")){ # c("male", "female", "any_combination")
  #lapply(list("tidyverse","igraph"), require, character.only = TRUE)
  require(tidyverse)
  require(igraph)
  
  if( grepl("gm", edge_attr(g) %>% names())){
    behavior <- "total_grooming"  
  }
  if( grepl("prox", edge_attr(g) %>% names())){
    behavior <- "prox"
  }
  
  #creates vector for removing edges = 0
  remove <- edge_attr(g)[[1]] == 0 
  #removes edges, preserves all nodes/vertices
  g <- delete_edges(g, E(g)[remove]) 
  
  #creates edge weight vector for remaining edges to feed into function
  g_weights <- edge_attr(g)[[1]]
  
  #betweeness
  if(bt_weight == TRUE){
    gb <- betweenness(g, directed = FALSE, normalized = FALSE, weights = g_weights)
  }
  if(bt_weight == FALSE){
    gb <- betweenness(g, directed = FALSE, normalized = FALSE, weights = NULL)
  }
  
  #eigenvector centrality
  if(ec_weight == TRUE){
  ge <- eigen_centrality(g, weights = g_weights)$vector #is default undirected
  }
  if(ec_weight == FALSE){
  ge <- eigen_centrality(g)$vector
  }
  
  #degree and strength (weighted degree)
  if(deg_weight == TRUE){
  gd <- strength(g, weights = g_weights) }
  
  if(deg_weight == FALSE){
  gd <- degree(g) }
  
  #local transitivity
  if(trans_weight == TRUE){
    gt <- transitivity(g, vids = vertex_attr(g)[[1]] , type = "weighted", weights = g_weights) 
  }
  if(trans_weight == FALSE){
    gt <- transitivity(g, vids = vertex_attr(g)[[1]] , type = "local")
  }
  
  gt <- ifelse(is.nan(gt), 0, gt)
  
  # store vertex names to re-add among attributes later, all vertices present, even if unconnected
  v_names <- vertex_attr(g)$name
  source("functions/functions - data preparation.R")
  
  # create various attributes df to add as vertex attributes
  v_attrs <- add_individ_attr(df = data.frame(v_names, stringsAsFactors = F), ID1 = "v_names")
#mutate(year = year) %>% add_age(dyad = F)
  
  
  if(output == "graph"){
    # have to reinclude "name" or that attr overwritten
    vertex_attr(g) <- list(name = v_names, sex = as.factor(v_attrs$sex),
                           bt = gb, ec = ge, deg = gd, trans = gt) #age_mid_year = attrs$age_mid_year # doesn't want to add
    return(g)
  }
  if(output == "data.frame"){
    
    df <- data.frame(chimp_id = names(gb), year, network_sex, behavior, network_type = "undirected", bt = gb, ec = ge, deg = gd, trans = gt, stringsAsFactors = FALSE)
    #? remove year & dyad sex, see if in final can just remove list cols "data" and "graph" and then unnest the df that remains
    return(df)
  }
}


# Function 2 -- directed centrality measures function -------


# change each to directed



sna_measures_dir <- function(g, year = NULL, network_sex = NULL, bt_weight = TRUE, ec_weight = TRUE, deg_weight = TRUE, output = c("graph", "data.frame")){
  #lapply(list("tidyverse","igraph"), require, character.only = TRUE)
  require(tidyverse)
  require(igraph)
  
  behavior <- "grooming"
  
  #creates vector for removing edges = 0
  remove <- edge_attr(g)[[1]] == 0 
  #removes
  g <- delete_edges(g, E(g)[remove]) 
  
  #creates edge weight vector
  g_weights <- edge_attr(g)[[1]]
  
  #betweeness (never used)
  if(bt_weight == TRUE){
    gb <- betweenness(g, directed = TRUE, normalized = FALSE, weights = g_weights)
  }
  if(bt_weight == FALSE){
    gb <- betweenness(g, directed = TRUE, normalized = FALSE, weights = NULL)
  }
  
  #eigenvector centrality (never used, directional ec too confusing, if ever want directed ec use pagerank - good measure of prestige)
  if(ec_weight == TRUE){
    ge <- eigen_centrality(g, weights = g_weights, directed = TRUE)$vector
  }
  if(ec_weight == FALSE){
    ge <- eigen_centrality(g, directed = TRUE)$vector
  }
  
  #IN degree and strength (weighted degree)
  if(deg_weight == TRUE){
    gd_in <- strength(g, mode = "in", weights = g_weights) }
  
  if(deg_weight == FALSE){
    gd_in <- degree(g, mode = "in") }
  
  #OUT degree and strength (weighted degree)
  if(deg_weight == TRUE){
    gd_out <- strength(g, mode = "out", weights = g_weights) }
  
  if(deg_weight == FALSE){
    gd_out <- degree(g, mode = "out") }
  
  
  # store vertex names to re-add among attributes later
  v_names <- vertex_attr(g)$name
  source("functions/functions - data preparation.R")
  
  # create various attributes df to add as vertex attributes
  attrs <- add_individ_attr(df = data.frame(v_names, stringsAsFactors = F), ID1 = "v_names")
  #mutate(year = year) %>% add_age(dyad = F)
  
  
  if(output == "graph"){
    # have to reinclude "name" or that attr overwritten
    vertex_attr(g) <- list(name = v_names, sex = as.factor(attrs$sex),
                           bt = gb, ec = ge, deg_in = gd_in, deg_out = gd_out) #age_mid_year = attrs$age_mid_year # doesn't want to add
    return(g)
  }
  if(output == "data.frame"){
    
    df <- data.frame(chimp_id = names(gb), year, network_sex, behavior, network_type = "directed", bt = gb, ec = ge, deg_in = gd_in, deg_out = gd_out, stringsAsFactors = FALSE)
    return(df)
  }
}



# Function 3 -- plotting -------

# the plot graph function takes the graph/sociogram output from the sna_measures_undir function and creates
# a sociogram, arguments year, dyads, size centrality supplied manually, & behavior taken from graph object itself

#maybe change where function gets these arguments - maybe instead supply df, c



plot_graph <- function(g, year, dyads, behavior = c("total_grooming", "grooming", "prox"),
                       size_centrality = c("ec", "bt", "deg", "trans"),
                        scale_edge_weight = 5, scale_vertex_size = 1) { # layout = c("fr","circle","nicely"),
  lapply(list("tidyverse","igraph"), require, character.only = TRUE)
  
  source("functions/functions - data preparation.R") # for adding ages
  library(RColorBrewer)
  
  #set vertex size
  if(size_centrality == "ec"){ #& scale_vertex_size == 1
    scale_vertex_size <- 10
  }
  
  if(size_centrality == "bt" & dyads == "any_combo"){
    scale_vertex_size <- 0.1
  }
  
  if(size_centrality == "bt" & dyads != "any_combo"){
    scale_vertex_size <- 0.3
  }
  
  #vertex size
  #size <- g %>% vertex_attr() %>% .[[size_centrality]]*scale_vertex_size
 
  #set edge weight
  w <- g %>% edge_attr() %>% .[[1]]/scale_edge_weight 
 
 # # extract behavior for plot title
 #  if( grepl("gm", edge_attr(gdf$graph[[1]]) %>% names())){
 #    behavior <- "total_grooming"  
 #  }
 #  if( grepl("prox", edge_attr(gdf$graph[[1]]) %>% names())){
 #    behavior <- "prox"
 #  }
  
  #set up layout setting
  if(layout == "fr"){
    layout_setting <- layout_with_fr(g)
  }
  if(layout == "circle"){
    layout_setting <- layout_in_circle(g)
  }
  if(layout == "nicely"){
    layout_setting <- layout_nicely(g)
  }
  
  # add ages to vertex attributes
  ages <- data.frame(ID1 = vertex_attr(g)$name, year = year) %>%
    add_individ_attr() %>%
    add_age(., dyad = FALSE) %>%
    pull(age_mid_year)
  vertex_attr(g)$age <- ages
  
  color_order <- as.numeric(as.factor(ages))
  
  # create color for ages, assign them in ascending order!
  colors <- colorRampPalette(brewer.pal(n = 9, "YlOrRd"))(length(unique(ages)))[color_order]
       
  # add vertex shape by sex
  vertex_attr(g)$shape <- ifelse(vertex_attr(g)$sex == "M", "square", "circle")      
  
  #plot sociogram
  pg <- plot(g, 
             #vertex.label.color = "black", 
             #vertex.label = vertex_attr(g, "names"),
             vertex.label = NA,
             vertex.size = 7,
              vertex.color = colors,
              vertex.shape = vertex_attr(g, "shape"),
             edge.color = "grey", 
             edge.width = w, 
             layout = layout_setting,
             main = paste(dyads, behavior, year, layout, sep = " "))
  print(pg)
  
}

