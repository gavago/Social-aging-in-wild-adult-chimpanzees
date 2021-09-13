library(tidyverse)
library(igraph)
library(lmerTest)
source("functions/functions - age sex modeling.R")
z. <- function(x) scale(x)
# within years, randomly sample and replace nodes of networks
# create 1000 randomized versions of 2009 - 2017 networks
# save to extract network measures on
# turn network measures into distrubtions to then test for signicance in e.g. integration in a given year,
# changes in integration from year to year, etc.


# 8.16.21 all resaved after gm minutes updated
# 8.22.21 all resaved with only prox updated for biased pairs.

load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = T)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = T)

# 1. Node randomized graphs ----


# 1a. undirected networks -------------
# weighted - for model both sexes ----
set.seed(100)
list_ran_undir_sna_measure_both_sex_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, rank, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_both_sex_w[[i]] <- all_sna_measure_df_w %>%
    group_by(year, network_sex, behavior) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), sex = sample(sex), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 8.23.20 added prop_cyc to random
# 11.25.20 resaved after prox index *100

# save(list_ran_undir_sna_measure_both_sex_w, file = "data/ran1 - both sexes w - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata")

# weighted - for model sex sep ----
# undir ran for each sex in mixed sex net, and for same sex, either way sexes are analyzed separately
# and randomizations of relevant variables are done within sex
set.seed(100)
list_ran_undir_sna_measure_sex_sep_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, rank, and prop_cyc randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_sex_sep_w[[i]] <- all_sna_measure_df_w %>%
    group_by(year, network_sex, behavior, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 11.25.20 resaved after prox index *100

#save(list_ran_undir_sna_measure_sex_sep_w, file = "data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata")

# unweighted - for model both sexes ---- 
set.seed(100)
list_ran_undir_sna_measure_both_sex_uw <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_both_sex_uw[[i]] <- all_sna_measure_df_uw %>%
    group_by(year, network_sex, behavior) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), sex = sample(sex), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 8.23.20 added prop_cyc to rando
# 11.25.20 resaved after prox index *100

# save(list_ran_undir_sna_measure_both_sex_uw, file = "data/ran3 - both sexes uw - node (sex age rank chimp_id)randomized sna measures undirected prox and gmgmd unweighted.Rdata")

# unweighted - for model sex sep ---- 
set.seed(100)
list_ran_undir_sna_measure_sex_sep_uw <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_sex_sep_uw[[i]] <- all_sna_measure_df_uw %>%
    group_by(year, network_sex, behavior, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 11.25.20 resaved after prox index *100

# save(list_ran_undir_sna_measure_sex_sep_uw, file = "data/ran4 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd unweighted.Rdata")


# 1b. directed networks ------

# weighted - for model both sexes -----
set.seed(100)
list_ran_dir_sna_measure_both_sex_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_both_sex_w[[i]] <- dir_sna_measure_df_w %>%
    group_by(year, network_sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), sex = sample(sex), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 8.23.20 added prop_cyc to rando
# 11.25.20 resaved after prox index *100
# 7.3.21 resaved after gm codes updated

#save(list_ran_dir_sna_measure_both_sex_w, file = "data/ran5 - both sexes w - node (sex age rank chimp_id) randomized sna measures directed gm weighted.Rdata")

# weighted - for model sex sep -----
set.seed(100)
list_ran_dir_sna_measure_sex_sep_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_sex_sep_w[[i]] <- dir_sna_measure_df_w %>%
    group_by(year, network_sex, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 11.25.20 resaved after prox index *100
# 7.3.21 resaved after gm codes updated, and group by sex


#save(list_ran_dir_sna_measure_sex_sep_w, file = "data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata")


# unweighted - for model both sexes -----
set.seed(100)
list_ran_dir_sna_measure_both_sex_uw <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_both_sex_uw[[i]] <- dir_sna_measure_df_uw %>%
    group_by(year, network_sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), sex = sample(sex), prop_cyc =sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 8.23.20 added prop_cyc to rando
# 11.25.20 resaved after prox index *100
# 7.3.21 resaved after gm codes updated, and group by sex

#save(list_ran_dir_sna_measure_both_sex_uw, file = "data/ran7 - both sexes uw - node (sex age rank chimp_id) randomized sna measures directed gm unweighted.Rdata")

# unweighted - for model sex sep -----
set.seed(100)
list_ran_dir_sna_measure_sex_sep_uw <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_sex_sep_uw[[i]] <- dir_sna_measure_df_uw %>%
    group_by(year, network_sex, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 11.25.20 resaved after prox index *100
# 7.3.21 resaved ater gm codes changed, and group by sex
#save(list_ran_dir_sna_measure_sex_sep_uw, file = "data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata")



# 2. node randomization for effect of estrus (females in mixed-sex networks, doesn't permute sex) -------
# unweighted - directed ------
set.seed(100)
list_ran_f_estrus_dir_uw <- vector("list", length = 1000)

for(i in 1:1000) {
  
  # randomize nodes
  list_ran_f_estrus_dir_uw[[i]] <-   dir_sna_measure_df_uw %>%
    group_by(year, network_sex, behavior, sex) %>%
    mutate(prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    group_by(year, network_sex, behavior) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  


# 11.25.20 resaved after prox index *100
# 7.3.21 resaved ater gm codes changed, and group by sex
# 7.5.21 resaved - all attributes other than sex and prop cyc shuffled over male and females, just as in mixed sex network
# (actually controlling for just the additional effect of prop cyc... random F distribution of comparison for age and rank won't be different)

#save(list_ran_f_estrus_dir_uw, file = "data/ran9 - estrus dir uw - node (age rank prop_cyc chimp_id) randomized sna measures directed unweighted.Rdata")


# weighted - directed -------
set.seed(100)
list_ran_f_estrus_dir_w <- vector("list", length = 1000)

for(i in 1:1000) {
  
  # randomize nodes
  list_ran_f_estrus_dir_w[[i]] <-   dir_sna_measure_df_w %>%
  group_by(year, network_sex, behavior, sex) %>%
    mutate(prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    group_by(year, network_sex, behavior) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 11.25.20 resaved after prox index *100
# 7.3.21 resaved ater gm codes changed, and group by sex
# 7.5.21 resaved - all attributes other than sex and prop cyc shuffled over male and females, just as in mixed sex network
# (actually controlling for just the additional effect of prop cyc... random F distribution of comparison for age and rank won't be different)


#save(list_ran_f_estrus_dir_w, file = "data/ran10 - estrus dir w - node (age rank prop_cyc chimp_id) randomized sna measures directed weighted.Rdata")

# weighted - undirected ------

# for female integration by estrus
set.seed(100)
list_ran_f_estrus_undir_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, rank, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_f_estrus_undir_w[[i]] <- all_sna_measure_df_w %>%
    group_by(year, network_sex, behavior, sex) %>%
    mutate(prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    group_by(year, network_sex, behavior) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# 11.25.20 resaved after prox index *100
# 7.3.21 resaved ater gm codes changed, and group by sex
# 7.5.21 resaved - all attributes other than sex and prop cyc shuffled over male and females, just as in mixed sex network
# (actually controlling for just the additional effect of prop cyc... random F distribution of comparison for age and rank won't be different)

#save(list_ran_f_estrus_undir_w, file = "data/ran11 - estrus undir w - node (age rank prop_cyc chimp_id) randomized sna measures undirected weighted.Rdata")



## any combo, use sex as grouping factor for shuffling within any combo

# gyard -----------
#check that sampling happens within years
ID12009 <- ran_total_gm_gmd_index %>%
  filter(year == 2009) %>%
  pull(ID1) %>%
  unique()
RID12009 <- ran_total_gm_gmd_index %>%
  filter(year == 2009) %>%
  pull(RID1) %>% sort() %>%
  unique()
ID12009 %in% RID12009

#same w map?
#load("data/list column dyadic data prox & gm by year & dyad-sex year.Rdata", verbose = T)


RID1 <- sample(total_gm_gmd_index$ID1)


#  -------
load("data/attribute data alone.Rdata")
load("data/indices - annual dyadic grooming.Rdata", verbose = T)
load("data/indices - annual dyadic 5m proximity.Rdata", verbose = T)
source("functions/functions - data preparation.R")
source("functions/functions - sna measures and plot.R")
# XXX Edge permutations ----------------
#steps
# 1 - permute/randomize edges(!) in index data:
# --- adjust what sex combos of dyads are groupings
# --- within years, randomly sample each ID1 and ID2

list_e_ran_sna_measure_df <- vector("list", length = 1000)

# do this 1000 f'ing times
set.seed(100)

t <- Sys.time()
for(k in seq(list_ran_sna_measure_df)){
  
  
  ran_gdf_gmgmd_sex_comb <- total_gm_gmd_index %>%
    mutate(dyad_sex = "any_combo") %>% #change from sex specific to any combination of sexes (orig in 3.3)
    #sample/randomize individuals within years
    group_by(year) %>% #no grouping by sex, dyad designation is any combination of partner sexes
    mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # eep, do this a few times to remove instance of RID1 = RID2
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>%
    ungroup() %>%
    # nest dyadic data in list column (o.g. data script 3.3)
    select(year, dyad_sex, RID1, RID2, gmgmdi) %>%
    nest(data = c(RID1, RID2, gmgmdi)) %>%
    arrange(year) %>%
    # turn list col dyadic data into graphs (o.g. data in script 4.1)
    mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
    #add sna attributes to vertices
    mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))
  
  
  ran_gdf_gmgmd_sex_sep <- total_gm_gmd_index %>%
    #sample/randomize individuals within years and sex dyad type
    filter(dyad_sex != "mixed") %>% #keep only sex matching dyads (o.g. filter in script 3.3)
    group_by(year, dyad_sex) %>%
    mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # eep, do this a few times to remove instance of RID1 = RID2
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>%
    ungroup() %>%
    # nest dyadic data in list column (o.g. data script 3.3)
    select(year, dyad_sex, RID1, RID2, gmgmdi) %>%
    nest(data = c(RID1, RID2, gmgmdi)) %>%
    arrange(year) %>%
    # turn list col dyadic data into graphs (o.g. data in script 4.1)
    mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
    #add sna attributes to vertices
    mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))
  
  ran_gdf_prox_sex_comb <- index_5m %>%
    mutate(dyad_sex = "any_combo") %>% #change from sex specific to any combination of sexes (orig in 3.3)
    #sample/randomize individuals within years
    group_by(year) %>% #no grouping by sex, dyad designation is any combination of partner sexes
    mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # eep, do this a few times to remove instance of RID1 = RID2
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>%
    ungroup() %>%
    # nest dyadic data in list column (o.g. data script 3.3)
    select(year, dyad_sex, RID1, RID2, prox5i) %>%
    nest(data = c(RID1, RID2, prox5i)) %>%
    arrange(year) %>%
    # turn list col dyadic data into graphs (o.g. data in script 4.1)
    mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
    #add sna attributes to vertices
    mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))
  
  ran_gdf_prox_sex_sep <- index_5m %>%
    #sample/randomize individuals within years and dyad sexes
    filter(dyad_sex != "mixed") %>% #keep only sex matching dyads (o.g. filter in script 3.3)
    group_by(year, dyad_sex) %>%
    mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # eep, do this a few times to remove instance of RID1 = RID2
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # opportunity for recursion (if any ID1 & 2 still equal, then sample again)
    ungroup() %>%
    # nest dyadic data in list column (o.g. data script 3.3)
    select(year, dyad_sex, RID1, RID2, prox5i) %>%
    nest(data = c(RID1, RID2, prox5i)) %>%
    arrange(year) %>%
    # turn list col dyadic data into graphs (o.g. data in script 4.1)
    mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
    # add sna attributes to vertices
    mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))
  
  #create master data frame with all individual sna measures by year
  
  ran_graph_list <- list(ran_gdf_gmgmd_sex_sep, ran_gdf_gmgmd_sex_comb, ran_gdf_prox_sex_sep, ran_gdf_prox_sex_comb) 
  
  ran_sna_measure_df_list <- vector("list", length(ran_graph_list))
  
  for(j in 1:length(ran_graph_list)){
    gdf <- ran_graph_list[[j]] 
    n <- nrow(gdf)
    measures_list <- vector("list", length = n)
    
    for(i in seq(n)) {
      g <- gdf$graph[[i]]
      y <- gdf$year[[i]]
      s <- gdf$dyad_sex[[i]]
      measures_list[[i]] <- sna_measures_undir(g, year = y, network_sex = s, output = "data.frame")
    }
    df <- do.call("rbind", measures_list)
    ran_sna_measure_df_list[[j]] <- df
  }
  
  ran_sna_measure_df <- do.call("rbind", ran_sna_measure_df_list) %>%
    left_join(attr %>% select(chimp_id, sex, ends_with("id"), dobc, dfs, dls), by = "chimp_id") %>%
    add_age(dyad = FALSE) %>%
    arrange(year, network_sex, behavior, chimp_id)
  
  
  list_ran_sna_measure_df[[k]] <- ran_sna_measure_df
  
}
Sys.time() - t # 17.25 now?? bc all sex attribute is in and 09-10 merged? ;takes about 6.9 minutes to create

list_e_ran_sna_measure_df <- list_ran_sna_measure_df


#save(list_e_ran_sna_measure_df, file = "data/sna dataframes - edge randomized graphs.Rdata")


# ### Testing perm of attributes within sex and then sex separately. -----

# ### weighted - undir - for model sex sep ----
# undir ran for each sex in mixed sex net, and for same sex, either way sexes are analyzed separately
# and randomizations of relevant variables are done within sex
set.seed(100)

# wspp - within sex permutation plus .... a separate sex permutation among all individuals in a given year
list_ran_undir_sna_measure_both_sex_w_wspp <- vector("list", length = 1000)

# Undirected prox gm networks, node attributes of id, age, rank, and prop_cyc randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_both_sex_w_wspp[[i]] <- all_sna_measure_df_w %>%
    group_by(year, network_sex, behavior, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    group_by(year, network_sex, behavior) %>%
    mutate(chimp_id = sample(chimp_id), sex = sample(sex)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

#save(list_ran_undir_sna_measure_both_sex_w_wspp, file = "data/ran12 - both sex w - WSP+ node (age rank prop_cyc chimp_id) + sex randomized sna measures undirected prox and gmgmd weighted.Rdata")


# ### unweighted -  dir - for model sex sep -----
set.seed(100)
list_ran_dir_sna_measure_both_sex_uw_wspp <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_both_sex_uw_wspp[[i]] <- dir_sna_measure_df_uw %>%
    group_by(year, network_sex, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    group_by(year, network_sex, behavior) %>%
    mutate(chimp_id = sample(chimp_id), sex = sample(sex)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

#save(list_ran_dir_sna_measure_both_sex_uw_wspp, file = "data/ran13 - both sex w - WSP+ node (age rank prop_cyc chimp_id) + sex randomized sna measures directed gm unweighted.Rdata")


# ### weighted - dir - for model sex sep -----
set.seed(100)
list_ran_dir_sna_measure_both_sex_w_wspp <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_both_sex_w_wspp[[i]] <- dir_sna_measure_df_w %>%
    group_by(year, network_sex, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    group_by(year, network_sex, behavior) %>%
    mutate(chimp_id = sample(chimp_id), sex = sample(sex)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  


#save(list_ran_dir_sna_measure_both_sex_w_wspp, file = "data/ran14 - both sex w - WSP+ node (age rank prop_cyc chimp_id) + sex randomized sna measures directed gm weighted.Rdata")


# 3. not using - node randomization for effect of offspring (females in mixed-sex networks, doesn't permute sex) -------

# not redone after gm recode 7.2.21
source("data/data for mother offspring exploration.R")

# mom offspring mixed sex undir weighted 
# (for local trans gm mixed sex
# centrality prox in mixed sex,
# centrality groom same sex)
set.seed(100)
list_ran_undir_mo_w <- vector("list", length = 1000)

for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_mo_w[[i]] <-   mo_sna_w %>%
    group_by(year, network_sex, behavior, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), num_off = sample(num_off), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, num_off, sex, avg_rank, prop_cyc, everything())
}  

# mom offspring directed weighted
# strength out same sex
set.seed(100)
list_ran_dir_mo_w <- vector("list", length = 1000)

for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_mo_w[[i]] <- mo_dir_sna_w %>%
    group_by(year, network_sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), num_off  = sample(num_off), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, num_off, prop_cyc, everything())
}  


#save(list_ran_undir_mo_w, list_ran_dir_mo_w, file = "data/ran12 - offspring - node (age rank num off chimp_id) randomized sna measures for all companion offspring models.Rdata")





# within sex permutations for estrus - works exactly the same as same sex just use any combo for females sna in mixed sex networks
# unweighted - directed -----
set.seed(100)
list_ran_dir_sna_measure_both_sex_uw_wsp <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_both_sex_uw_wsp[[i]] <- dir_sna_measure_df_uw %>%
    group_by(year, network_sex, sex) %>% 
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc =sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  


#save(list_ran_dir_sna_measure_both_sex_uw_wsp, file = "data/ran12 - both sexes uw - WITHIN SEX PERMUTATION for estr - node (sex age rank chimp_id) randomized sna measures directed gm unweighted.Rdata")

# weighted - directed -----
set.seed(100)
list_ran_dir_sna_measure_both_sex_w_wsp <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_both_sex_w_wsp[[i]] <- dir_sna_measure_df_w %>%
    group_by(year, network_sex, sex) %>%
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  


#save(list_ran_dir_sna_measure_both_sex_w_wsp, file = "data/ran13 - both sexes w - WITHIN SEX PERMUTATION for estr - node (sex age rank chimp_id) randomized sna measures directed gm weighted.Rdata")

# weighted - undirected ----
set.seed(100)
list_ran_undir_sna_measure_both_sex_w_wsp <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, rank, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_both_sex_w_wsp[[i]] <- all_sna_measure_df_w %>%
    group_by(year, network_sex, behavior, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

# save(list_ran_undir_sna_measure_both_sex_w_wsp, file = "data/ran14 - both sexes w - WITHIN SEX PERMUTATION for estr - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata")
