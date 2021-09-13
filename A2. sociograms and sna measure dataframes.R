library(tidyverse)
library(purrr)
library(igraph)
select <- dplyr::select
source("functions/functions - sna measures and plot.R")

# options for shiny plots
# sexes
# year(s)
# weighted, yes/no
# filter edge level
# graph layout


# Graph function: steps each year:
#  extract data subset and create graph
#  SNA measures  
#  plot - weight edges, size node by sna measure


# 1. Calculate SNA measures -----
# 1a. weighted - Transform list column data into graph objects - graph_w_sna weighted sna measures ------
load("data/list column dyadic data prox & gm by year & dyad-sex year.Rdata", verbose = T)
# loading data to create graphs ^
# see data where "data" column is a list of dataframes of dyadic association indices from either that year (sex_comb for sexes combined)
# of for dyads of a given sex (sex_sep for sexes separated) in a given year

names(g_data_gm_sex_sep)
names(g_data_gm_sex_comb)


gdf_gmgmd_sex_sep <- g_data_gmgmd_sex_sep %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna weighted measures
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))


gdf_gmgmd_sex_comb <- g_data_gmgmd_sex_comb %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna weighted measures
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

  
gdf_gm_sex_sep <- g_data_gm_sex_sep %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = TRUE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna weighted measures
  mutate(graph_w_sna = map(graph, sna_measures_dir, network_sex = dyad_sex, output = "graph")) #warnings will be about EC, ignore bc not using directed EC

gdf_gm_sex_comb <- g_data_gm_sex_comb %>% # fixed, g_data was gmgmd 7.2.21
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = TRUE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna weighted measures
  mutate(graph_w_sna = map(graph, sna_measures_dir, network_sex = dyad_sex, output = "graph"))  #warnings will be about EC, ignore bc not using directed EC


gdf_prox_sex_sep <- g_data_prox_sex_sep %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna weighted measures
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

gdf_prox_sex_comb <- g_data_prox_sex_comb %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna weighted measures
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

# vertex attributes are visualization --
gdf_gm_sex_sep$graph_w_sna[[1]] %>% vertex_attr()
# weighted sna measures, where is trans? Never bothered to calculate directed transitivity, so not added as an attribute in directed graphs.
gdf_gmgmd_sex_sep$graph_w_sna[[1]] %>% vertex_attr()
# ec, bt, deg, trans (undirected so just deg)
gdf_gm_sex_sep$data
gdf_prox_sex_sep$data

# save graph w sna as weighted for visualization

# save(gdf_gmgmd_sex_comb,
#       gdf_gm_sex_comb,
#      gdf_prox_sex_comb,
#      gdf_gmgmd_sex_sep,
#      gdf_gm_sex_sep,
#      gdf_prox_sex_sep , file = "data/sna graphs - graph_w_sna has name & weighted sna measures as vector attributes and index as edge weight.Rdata")

# 1b. unweighted - Transform list column data into graph objects - graph_w_sna unweighted sna measures ----

gdf_gmgmd_sex_sep_uw <- g_data_gmgmd_sex_sep %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna UNweighted measures
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, bt_weight = FALSE,
                           ec_weight = FALSE, deg_weight = FALSE, trans_weight = FALSE, output = "graph"))

gdf_gmgmd_sex_comb_uw <- g_data_gmgmd_sex_comb %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna UNweighted measures
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, bt_weight = FALSE,
                           ec_weight = FALSE, deg_weight = FALSE, trans_weight = FALSE, output = "graph"))


gdf_gm_sex_sep_uw <- g_data_gm_sex_sep %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = TRUE))) %>% #changed directed to TRUE 11.25.20
  #add sna attributes to vertices for viz in sociogram, including sna UNweighted measures
  mutate(graph_w_sna = map(graph, sna_measures_dir, network_sex = dyad_sex, bt_weight = FALSE,
                           ec_weight = FALSE, deg_weight = FALSE, output = "graph"))

gdf_gm_sex_comb_uw <- g_data_gm_sex_comb %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = TRUE))) %>% ##changed directed to TRUE 11.25.20
  #add sna attributes to vertices for viz in sociogram, including sna UNweighted measures
  mutate(graph_w_sna = map(graph, sna_measures_dir, network_sex = dyad_sex, bt_weight = FALSE,
                           ec_weight = FALSE, deg_weight = FALSE,  output = "graph"))

gdf_prox_sex_sep_uw <- g_data_prox_sex_sep %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna UNweighted measures
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, bt_weight = FALSE,
                           ec_weight = FALSE, deg_weight = FALSE, trans_weight = FALSE, output = "graph"))

gdf_prox_sex_comb_uw <- g_data_prox_sex_comb %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices for viz in sociogram, including sna UNweighted measures
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, bt_weight = FALSE,
                           ec_weight = FALSE, deg_weight = FALSE, trans_weight = FALSE, output = "graph"))

gdf_gm_sex_sep_uw$graph_w_sna[[12]] %>% vertex_attr()

gdf_gm_sex_sep$graph_w_sna[[12]] %>% vertex_attr()


# save(gdf_gmgmd_sex_comb_uw,
#     gdf_gm_sex_comb_uw,
#     gdf_prox_sex_comb_uw,
#     gdf_gmgmd_sex_sep_uw,
#     gdf_gm_sex_sep_uw,
#     gdf_prox_sex_sep_uw, file = "data/sna graphs - graph_w_sna has name & unweighted sna measures as vector attributes and index as edge weight.Rdata")


# 2. SNA measure data frames (turn graph objects in column "graph" into dataframes of chimp-years) ----
source("functions/functions - data preparation.R")
source("functions/functions - sna measures and plot.R")
# load this and just use graph object to create sna measures, not measures from graph_w_sna (weighted and unweighted is just talking about vector/node attributes available for visualization)
load("data/sna graphs - graph_w_sna has name & weighted sna measures as vector attributes and index as edge weight.Rdata", verbose = T)
load("data/attribute data alone.Rdata", verbose = T)

gdf_gmgmd_sex_comb$graph[[1]] %>% vertex_attr() # vertex is just name
gdf_gmgmd_sex_comb$graph[[1]] %>% edge_attr() # edge attributes are total groom/groomed indices

#create master data frame with all individual sna measures by year

#list of list columns
undir_graph_list <- list(gdf_gmgmd_sex_sep, gdf_gmgmd_sex_comb, gdf_prox_sex_sep, gdf_prox_sex_comb) 

all_sna_measure_df_list_weighted <- vector("list", length(undir_graph_list))
all_sna_measure_df_list_unweighted <- vector("list", length(undir_graph_list))

# --- Undirected SNA loop -----
for(j in 1:length(undir_graph_list)){
  gdf <- undir_graph_list[[j]] 
  n <- nrow(gdf) # each row is an annual network
  measures_list_weighted <- vector("list", length = n)
  measures_list_unweighted <- vector("list", length = n)
  
  #unweighted measures (never get used if undirected) ----
  for(i in seq(n)) {
    g <- gdf$graph[[i]]
    y <- gdf$year[[i]]
    s <- gdf$dyad_sex[[i]]
    
    measures_list_unweighted[[i]] <- sna_measures_undir(g, year = y, network_sex = s,
                                                        bt_weight = FALSE, ec_weight = FALSE, 
                                                        deg_weight = FALSE, trans_weight = FALSE, output = "data.frame")
  }
  df_uw <- do.call("rbind", measures_list_unweighted)
  all_sna_measure_df_list_unweighted[[j]] <- df_uw
  
  #weighted measures (all used except undirected grooming) ----
  for(i in seq(n)) {
    g <- gdf$graph[[i]]
    y <- gdf$year[[i]]
    s <- gdf$dyad_sex[[i]]
    measures_list_weighted[[i]] <- sna_measures_undir(g, year = y, network_sex = s, output = "data.frame")
  }
  df_w <- do.call("rbind", measures_list_weighted)
  all_sna_measure_df_list_weighted[[j]] <- df_w
  
}

all_sna_measure_df_uw <- do.call("rbind", all_sna_measure_df_list_unweighted) %>%
  left_join(attr %>% select(chimp_id, sex, ends_with("id"), dobc, dfs, dls), by = "chimp_id") %>%
  add_age(dyad = FALSE) %>%
  add_individ_ann_attr()

all_sna_measure_df_w <- do.call("rbind", all_sna_measure_df_list_weighted) %>%
  left_join(attr %>% select(chimp_id, sex, ends_with("id"), dobc, dfs, dls), by = "chimp_id") %>%
  add_age(dyad = FALSE) %>%
  add_individ_ann_attr()

# --- Directed SNA loop -----
dir_graph_list <- list(gdf_gm_sex_sep, gdf_gm_sex_comb)
dir_sna_measure_df_list_weighted <- vector("list", length(dir_graph_list))
dir_sna_measure_df_list_unweighted <- vector("list", length(dir_graph_list))


for(j in 1:length(dir_graph_list)){
  gdf <- dir_graph_list[[j]] 
  n <- nrow(gdf)
  measures_list_weighted <- vector("list", length = n)
  measures_list_unweighted <- vector("list", length = n)
  
  #unweighted measures (only use in and out degree from this) -----
  for(i in seq(n)) {
    g <- gdf$graph[[i]]
    y <- gdf$year[[i]]
    s <- gdf$dyad_sex[[i]]
    measures_list_unweighted[[i]] <- sna_measures_dir(g, year = y, network_sex = s,
                                                        bt_weight = FALSE, ec_weight = FALSE, 
                                                        deg_weight = FALSE, output = "data.frame")
  }
  df_uw <- do.call("rbind", measures_list_unweighted)
  dir_sna_measure_df_list_unweighted[[j]] <- df_uw
  
  #weighted measures (only use in and out strength from this) -----
  for(i in seq(n)) {
    g <- gdf$graph[[i]]
    y <- gdf$year[[i]]
    s <- gdf$dyad_sex[[i]]
    measures_list_weighted[[i]] <- sna_measures_dir(g, year = y, network_sex = s, output = "data.frame")
  }
  df_w <- do.call("rbind", measures_list_weighted) 
  dir_sna_measure_df_list_weighted[[j]] <- df_w
  
} # warnings are for directed EC, never use

dir_sna_measure_df_uw  <- do.call("rbind", dir_sna_measure_df_list_unweighted) %>%
  left_join(attr %>% select(chimp_id, sex, ends_with("id"), dobc, dfs, dls), by = "chimp_id") %>%
  add_age(dyad = FALSE) %>%
  add_individ_ann_attr()

dir_sna_measure_df_w <- do.call("rbind", dir_sna_measure_df_list_weighted) %>%
  left_join(attr %>% select(chimp_id, sex, ends_with("id"), dobc, dfs, dls), by = "chimp_id") %>%
  add_age(dyad = FALSE) %>%
  add_individ_ann_attr()

nrow(all_sna_measure_df_uw) # 800
nrow(all_sna_measure_df_w)
nrow(dir_sna_measure_df_uw) # 400 - is half number because only grooming is directed, not prox
nrow(dir_sna_measure_df_w) # 400

all_sna_measure_df_uw %>%
  filter(is.na(rank_class))

  
# When avg_rank is NA -> 0 and when rank class is NA -> "lo"
all_sna_measure_df_uw %<>%
  mutate(avg_rank = ifelse(is.na(avg_rank), 0, avg_rank), 
         rank_class = ifelse(is.na(rank_class), "lo", rank_class))
all_sna_measure_df_w %<>%
  mutate(avg_rank = ifelse(is.na(avg_rank), 0, avg_rank), 
         rank_class = ifelse(is.na(rank_class), "lo", rank_class))
dir_sna_measure_df_uw %<>%
  mutate(avg_rank = ifelse(is.na(avg_rank), 0, avg_rank), 
         rank_class = ifelse(is.na(rank_class), "lo", rank_class))
dir_sna_measure_df_w %<>%
  mutate(avg_rank = ifelse(is.na(avg_rank), 0, avg_rank), 
         rank_class = ifelse(is.na(rank_class), "lo", rank_class))  

# save(all_sna_measure_df_uw, dir_sna_measure_df_uw, file = "data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata")
# save(all_sna_measure_df_w, dir_sna_measure_df_w, file = "data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata")
# write.csv(all_sna_measure_df_w, file = "data/sna measures undirected weighted.csv", row.names = F)
# write.csv(dir_sna_measure_df_w, file = "data/sna measures directed weighted.csv", row.names = F)
# write.csv(all_sna_measure_df_uw, file = "data/sna measures undirected unweighted.csv", row.names = F)
# write.csv(dir_sna_measure_df_uw, file = "data/sna measures directed unweighted.csv", row.names = F)

all_sna_measure_df_uw %>%
  filter(behavior == "total_grooming", year == "2010")

# ISSUES WITH WEIGHTED DIRECTED EC: 
# https://igraph-help.nongnu.narkive.com/38qXaASY/warning-message-in-eigen-centrality-function-of-package-igraph
# more reason to only use directed graphs to understand in vs out degree

#resaved rank class as character 5.1.20
#resaved after prox index * 100 on 11.25.20
#resaved after fix gm code 7.2.21



# 3. Create pdf sociograms from igraphs in list column  ----
# not updated for some time...
library(tidyverse)
library(igraph)

load("data/sna graphs - graph_w_sna has name & weighted sna measures as vector attributes and index as edge weight.Rdata", verbose = T)
#load("data/sna graphs - graph_w_sna has name & unweighted sna measures as vector attributes and index as edge weight.Rdata", verbose = T)
source("functions/functions - data preparation.R")
source("functions/functions - sna measures and plot.R")

#list of list column dfs
#for each df, go through each year and print

vertex_attr(gdf_gmgmd_sex_sep$graph[[1]])
edge_attr(gdf_gmgmd_sex_sep$graph[[1]])

#each element of this list is list col data of given behavior and dyad sexes for each year of focal data
gm_graph_list<- list(gdf_gm_sex_comb, gdf_gm_sex_sep) # gdf_prox_sex_sep, gdf_prox_sex_comb

gmgmd_graph_list<- list(gdf_gmgmd_sex_comb, gdf_gmgmd_sex_sep) # gdf_prox_sex_sep, gdf_prox_sex_comb


#did not update these pdfs after change to Prox index on 11/25/20
#or after updating grooming codes on 7/2/21
#settings for title and plot graph function, ec bt trans deg
size_centrality <- "ec"
layout <- "fr"


for (i in seq(length(gmgmd_graph_list))){ # for each graph type
  
  # choose set of graphs - network behavior and sex composition
  gdf <- gmgmd_graph_list[[i]]
  
  # determine behavior from edge attribute name
  edge_name <- names(gdf$data[[1]][,3]) 
  if(edge_name == "gmgmdi"){
    behavior <- "total_grooming"
  }
  if(edge_name == "gmi"){
    behavior <- "grooming"
  }
  if(grepl("prox", names(gdf$data[[1]][,3]))){
    behavior <- "prox"
  }
  
  #behavior = ifelse( grepl("gm", names(gdf$data[[1]][,3])), "total_grooming", "prox") #determine behavior type
  sex = ifelse( "any_combo" %in% gdf$dyad_sex, "Sexes_combined", "Sexes_separate")
  #subfolder = ifelse( "any_combo" %in% gdf$dyad_sex, "MF_combined/", "MM_FF_separate/") #and save location
  
  title <- paste(sex, behavior, layout, sep = "_")
  pdf(paste0("sociograms/", title, ".pdf"))
  
for (j in seq(nrow(gdf))) { #for each sex-year graph

    #and plot sociogram
  #plot_title <- paste( gdf$dyad_sex[[j]], gdf$year[[j]], behavior, size_centrality_x, sep = "_")
  
  g <- gdf$graph_w_sna[[j]]
  plot_graph(g, behavior = behavior, dyads = gdf$dyad_sex[[j]],
                                      year = gdf$year[[j]], size_centrality = size_centrality,
                                      scale_edge_weight = 1, scale_vertex_size = 1)
  
  #layout = layout_setting
  # add sex and age to vertices x
  # need to add shape for sex x
  # color scale for age
  
  # play w vertex size to see females
  # --- take log of EC for vertex size?
  # --- remove size assignment?
  
  }
  dev.off()
}


# peak inside tings - handy igraph functions -----
g1 <- gdf$graph_w_sna[[1]]
edge_attr(g1) %>% names()
vertex_attr(g1)
V(g1)
E(g1)
gorder(g1)
gsize(g1)





# g yard ----

# map for ploting fail
# socs <- gdf_prox_sex_comb %>%
#   mutate( behavior = ifelse( grepl("gm", names(.data$data[[1]][,3])), "total_grooming", "prox")) %>%
#    map(graph_w_sna, plot_graph, year = year, behavior = behavior, layout = "nicely", size_centrality = "ec")




# compare old and new graphs after groom recode
load("data/sna graphs - graph_w_sna has name & weighted sna measures as vector attributes and index as edge weight.Rdata", verbose = T)
load("data/sna graphs - graph_w_sna has name & unweighted sna measures as vector attributes and index as edge weight.Rdata", verbose = T)

# old_gdf_gm_sex_comb <- gdf_gm_sex_comb
# old_gdf_gm_sex_sep <- gdf_gm_sex_sep
# old_gdf_gm_sex_comb_uw <- gdf_gm_sex_comb_uw
# old_gdf_gm_sex_sep_uw <- gdf_gm_sex_sep_uw
# save(old_gdf_gm_sex_comb, old_gdf_gm_sex_sep, 
#      old_gdf_gm_sex_comb_uw, old_gdf_gm_sex_sep_uw,
#      file = "data/sna graphs - old gm sex comb & sex sep.Rdata")

load("data/sna graphs - old gm sex comb & sex sep.Rdata", verbose = T)

gdf_gm_sex_comb$graph[[1]] %>% vertex_attr()
old_gdf_gm_sex_comb$graph[[1]] %>% vertex_attr()

gdf_gm_sex_comb$data[[1]] %>% head()
old_gdf_gm_sex_comb$data[[1]] %>% head()

# Add annual atributes to GRAPHS (not data frames) ----
load("data/sna graphs - graph_w_sna has name & weighted sna measures as vector attributes and index as edge weight.Rdata", verbose = T)

load("data/sna graphs - graph_w_sna has name & unweighted sna measures as vector attributes and index as edge weight.Rdata", verbose = T)

load("data/annual average standardized ranks.Rdata", verbose = T)

#Not currently done, bc moving on w analysis of network positions, not visualization of attributes.

g <- gdf_gmgmd_sex_comb_uw %>%
  filter(year == 2010) %>%
  pull(graph_w_sna) %>%
  .[[1]]

vertex_attr(g)
# pull names of nodes
# filter rank for year[[i]] and individuals in those nodes
# then filter rank file


gdf_gm_sex_comb_uw
gdf_prox_sex_comb_uw 
gdf_gmgmd_sex_sep_uw
gdf_gm_sex_sep_uw
gdf_prox_sex_sep_uw

gdf_gmgmd_sex_comb
gdf_gm_sex_comb
gdf_prox_sex_comb
gdf_gmgmd_sex_sep
gdf_gm_sex_sep
gdf_prox_sex_sep


# adding attributes piecemeal ------

# undirected gm_gmd network, year X, male
names(male_gmgmdi)
years <- unique(male_gmgmdi$year)
years

sub <- male_gmgmdi %>%
  filter(year == 2009)

# prep
mpairs <- sub %>%
  select(ID1, ID2) %>%
  as.matrix()
weights <- sub %>%
  pull(gmgmdi)


g <- graph.edgelist(mpairs, directed = F)
g <- set_edge_attr(g, "gm_time", value = weights) #set_vertex_attr


g <- grg.game(100, 0.2)
g

# gave up on making sna measure a color
V(g1)$color <- vertex_attr(g1)$bt
V(g1)$color <- magma()


years <- unique(df$year)
sexes <- c("M", "F", "both")

i <- 1

for (i in (seq(sexes))){
  
  annual_graphs <- vector("list", length = length(years))
  
  for (j in seq(years)){
    
    yr <- years[j]
    
    if (sexes[i] == "M" | sexes[i] == "F"){
      g  <- df %>%
        filter(sexA == sexes[i] & sexB == sexes[i]) %>%
        filter(year == yr) %>%
        select(ID1,ID2, gmgmdi)
    }
    if (sex == "both")  {
      g <- df %>% 
        filter(year == year) %>%
        select(ID1,ID2, gmgmdi)
    }
    
    annual_graphs[[j]] <- 
  }
}


# labels
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x))) }

lab.locs <- radian.rescale(x=1:gorder(g1), direction= -1, start=0)

plot(g1, vertex.label.color = "black",
     vertex.label.dist = 1,
     edge.color = "blue", edge.width = w, layout = layout_in_circle(g))
