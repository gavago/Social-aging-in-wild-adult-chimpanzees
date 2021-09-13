library(tidyverse)
library(mgcv)
library(progress)
source("functions/functions - dev expl re and test sig dev expl re.R")

# missing ms_digr dogr sigr sogr, fs_ same
# & fem mixed

# A. Load node-randomized datasets ----
# for all mixed sex nets
# use sex sep randomized datasets for models
# bc even in any_combo (i.e. mixed networks) attributes are randomized within sexes
# why? bc need females to truly be females and males to truly be males to run within sex repeatabilities, even 
# on integration in mixed sex networks
# so specify data used from rando df's as
# network sex == "any_combo" sex == ... M|F depending
# for same sex network can just use same sex networks from sex sep randomizations

# for mixed sex, create models w re on randomized data within the loop
# for same sex, use previously saved models w re on randomized data and call/compare within the loop

# undir w
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

# dir gm w
load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)

# dir gm uw
load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)


# B. Upfront load randomized models WITH RE (only same sex, set up mixed sex within loops) -----
#same
# - groom
load("data/models gam - ran - H1 same sex grooming random deg and strength in.Rdata", verbose = T)
load("data/models gam - ran - H1 same sex grooming random deg and strength out.Rdata", verbose = T)
load("data/models gam - ran - H2 same sex grooming bt and trans.Rdata", verbose = T)
load("data/models gam - ran - H3 same sex grooming ec.Rdata", verbose = T)
# - prox
load("data/models gam - ran - H1 same sex prox random strength.Rdata", verbose = T) 
load("data/models gam - ran - H2 same sex prox bt and trans.Rdata", verbose = T)
load("data/models gam - ran - H3 same sex prox ec.Rdata", verbose = T)


# C. Create randomized models WITHOUT RE and compare to with RE ----
# - 1. Mixed sex networks -----
# - MALES --------
# -- gm deg strength in -----
# load ran dfs
#load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)
#load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)

ran_mdigr_rde_list <- vector(length = 1000)
ran_msigr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  # use any_combo sex == M in f estrus dir rand data bc attributes randomized within sexes (neglected group by sex in sex sep dir rand data, unlike sex sep undir)
  # need males to truly be males, just ID and attributes permuted, not testing sig of sex effect
  mdata_w <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  mdata_uw <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)

  ran_mdigr_w_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = mdata_uw, family = gaussian(link = "log"), method = "REML")
  ran_mdigr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = mdata_uw, family = gaussian(link = "log"), method = "REML")
  ran_mdigr_rde_list[[i]] <- dev_expl_re(ran_mdigr_w_re, ran_mdigr_no_re)
  
  ran_msigr_w_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = mdata_w, family = gaussian(link = "log"), method = "REML")
  ran_msigr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = mdata_w, family = gaussian(link = "log"), method = "REML")
  ran_msigr_rde_list[[i]] <- dev_expl_re(ran_msigr_w_re, ran_msigr_no_re)
  
}
Sys.time() - t # 6.7 min

#save(ran_mdigr_rde_list, ran_msigr_rde_list, file = "data/models gam - ran - RE deviance explained - H1 MALE mixed sex grooming random deg and strength in.Rdata") 


# -- gm deg strength out -----
# load ran dfs
#load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)
#load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)

ran_mdogr_rde_list <- vector(length = 1000)
ran_msogr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  # use any combo sex == M in f estrus dir rand data bc attributes randomized within sexes (neglected group by sex in dir rand data, unlike undir)
  # need males to truly be males, just ID and attributes permuted, not testing sig of sex effect
  mdata_w <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  mdata_uw <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_mdogr_w_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = mdata_uw, family = gaussian(link = "log"), method = "REML")
  ran_mdogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = mdata_uw, family = gaussian(link = "log"), method = "REML")
  ran_mdogr_rde_list[[i]] <- dev_expl_re(ran_mdogr_w_re, ran_mdogr_no_re)
  
  ran_msogr_w_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = mdata_w, family = gaussian(link = "log"), method = "REML")
  ran_msogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = mdata_w, family = gaussian(link = "log"), method = "REML")
  ran_msogr_rde_list[[i]] <- dev_expl_re(ran_msogr_w_re, ran_msogr_no_re)
  
}
Sys.time() - t #4.8 min

#save(ran_mdogr_rde_list, ran_msogr_rde_list, file = "data/models gam - ran - RE deviance explained - H1 MALE mixed sex grooming random deg and strength out.Rdata") 

# -- gm bt trans ----
# load ran dfs
# load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

ran_mbtgr_rde_list <- vector(length = 1000)
ran_mtrgr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  # use any combo in sex sep undir rand data bc need males to truly be males, just ID and attributes permuted, not testing sig of sex effect
  mdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)
  
  ran_mbtgr_w_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mbtgr_no_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mbtgr_rde_list[[i]] <- dev_expl_re(ran_mbtgr_w_re, ran_mbtgr_no_re)
  
  ran_mtrgr_w_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)+ s(chimp_id, bs = "re"),  data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mtrgr_no_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mtrgr_rde_list[[i]] <- dev_expl_re(ran_mtrgr_w_re, ran_mtrgr_no_re)
}

Sys.time() - t # 5.7 min

#save(ran_mbtgr_rde_list, ran_mtrgr_rde_list, file = "data/models gam - ran - RE deviance explained - H2 MALE mixed sex grooming bt and trans.Rdata")

# -- gm ec ----
# load ran dfs
#load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

ran_mecgr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  # use any combo in sex sep undir rand data bc need males to truly be males, just ID and attributes permuted, not testing sig of sex effect
  mdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_mecgr_w_re <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)+ s(chimp_id, bs = "re"),  data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mecgr_no_re <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mecgr_rde_list[[i]] <- dev_expl_re(ran_mecgr_w_re, ran_mecgr_no_re)
  
}
Sys.time() - t #2.6 min


#save(ran_mecgr_rde_list, file = "data/models gam - ran - RE deviance explained - H3 MALE mixed sex grooming ec.Rdata")

# -- prox strength ----
# load ran dfs
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

ran_mspr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  # use any combo in sex sep undir rand data bc need males to truly be males, just ID and attributes permuted, not testing sig of sex effect
  mdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(behavior == "prox", network_sex == "any_combo" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)
  
  ran_mspr_w_re <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"), data = mdata, family = Gamma(link = "log"), method = "REML")
  ran_mspr_no_re <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mdata, family = Gamma(link = "log"), method = "REML")
  ran_mspr_rde_list[[i]] <- dev_expl_re(ran_mspr_w_re, ran_mspr_no_re)
  
}
Sys.time() - t # 2.4

ran_mspr_rde_list <- x1

save(ran_mspr_rde_list, file = "data/models gam - ran - RE deviance explained - H1 MALE mixed sex prox random strength.Rdata") 

# -- prox bt trans ----
# load ran dfs
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

ran_mtrpr_rde_list <- vector(length = 1000)
ran_mbtpr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  # use any combo in sex sep undir rand data bc need males to truly be males, just ID and attributes permuted, not testing sig of sex effect
  mdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>% 
    filter(behavior == "prox" & network_sex == "any_combo" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)
  
  ran_mtrpr_w_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mtrpr_no_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mtrpr_rde_list[[i]] <- dev_expl_re(ran_mtrpr_w_re, ran_mtrpr_no_re)
  
  ran_mbtpr_w_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mbtpr_no_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mbtpr_rde_list[[i]] <- dev_expl_re(ran_mbtpr_w_re, ran_mbtpr_no_re)
  
}
Sys.time() - t #11.5

save(ran_mbtpr_rde_list, ran_mtrpr_rde_list, file = "data/models gam - ran - RE deviance explained - H2 MALE mixed sex prox random bt and trans.Rdata") 


# -- prox ec ----
# load ran dfs
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

ran_mecpr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  # use any combo in sex sep undir rand data bc need males to truly be males, just ID and attributes permuted, not testing sig of sex effect
  mdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "M") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_mecpr_w_re <- gam(ec ~ s(age_mid_year,k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"), data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mecpr_no_re <- gam(ec ~ s(age_mid_year,k = 5) + s(avg_rank, k = 5), data = mdata, family = gaussian(link = "log"), method = "REML")
  ran_mecpr_rde_list[[i]] <- dev_expl_re(ran_mecpr_w_re, ran_mecpr_no_re)
  
}
Sys.time() - t #2.5 min

save(ran_mecpr_rde_list, file = "data/models gam - ran - RE deviance explained - H3 MALE mixed sex prox ec.Rdata")

# - FEMALES --------
# -- gm deg strength in -----

ran_fdigr_rde_list <- vector(length = 1000)
ran_fsigr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  fdata_uw <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  fdata_w <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_fdigr_w_re <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata_uw, family = gaussian(link = "log"), method = "REML")
  ran_fdigr_no_re <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata_uw, family = gaussian(link = "log"), method = "REML")
  ran_fdigr_rde_list[[i]] <- dev_expl_re(ran_fdigr_w_re, ran_fdigr_no_re)
  
  ran_fsigr_w_re <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata_w, family = Gamma(link = "log"), method = "REML")
  ran_fsigr_no_re <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata_w, family = Gamma(link = "log"), method = "REML")
  ran_fsigr_rde_list[[i]] <- dev_expl_re(ran_fsigr_w_re, ran_fsigr_no_re)
  
}
Sys.time() - t # 6.3 min

#save(ran_fdigr_rde_list, ran_fsigr_rde_list, file = "data/models gam - ran - RE deviance explained - H1 FEMALE mixed sex grooming random deg and strength in.Rdata") 

#
# -- gm deg strength out -----

ran_fdogr_rde_list <- vector(length = 1000)
ran_fsogr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  fdata_uw <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  fdata_w <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_fdogr_w_re <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata_uw, family = gaussian(link = "log"), method = "REML")
  ran_fdogr_no_re <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 5), data = fdata_uw, family = gaussian(link = "log"), method = "REML")
  ran_fdogr_rde_list[[i]] <- dev_expl_re(ran_fdogr_w_re, ran_fdogr_no_re)
  
  ran_fsogr_w_re <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata_w, family = Gamma(link = "log"), method = "REML")
  ran_fsogr_no_re <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata_w, family = Gamma(link = "log"), method = "REML")
  ran_fsogr_rde_list[[i]] <- dev_expl_re(ran_fsogr_w_re, ran_fsogr_no_re)
  
}
Sys.time() - t # 8 min

#save(ran_fdogr_rde_list, ran_fsogr_rde_list, file = "data/models gam - ran - RE deviance explained - H1 FEMALE mixed sex grooming random deg and strength out.Rdata")

# -- gm bt trans ----

ran_fbtgr_rde_list <- vector(length = 1000)
ran_ftrgr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  fdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_fbtgr_w_re <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fbtgr_no_re <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fbtgr_rde_list[[i]] <- dev_expl_re(ran_fbtgr_w_re, ran_fbtgr_no_re)
  
  ran_ftrgr_w_re <- gam(trans ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_ftrgr_no_re <- gam(trans ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_ftrgr_rde_list[[i]] <- dev_expl_re(ran_ftrgr_w_re, ran_ftrgr_no_re)
}

Sys.time() - t # 24.7 min

#save(ran_fbtgr_rde_list, ran_ftrgr_rde_list, file = "data/models gam - ran - RE deviance explained - H2 FEMALE mixed sex grooming bt and trans.Rdata")


# -- gm ec ----
ran_fecgr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  fdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_fecgr_w_re <-  gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fecgr_no_re <-  gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fecgr_rde_list[[i]] <- dev_expl_re(ran_fecgr_w_re, ran_fecgr_no_re)
  
}
Sys.time() - t # 7.4 min

#save(ran_fecgr_rde_list, file = "data/models gam - ran - RE deviance explained - H3 FEMALE mixed sex grooming ec.Rdata")

# -- prox strength ----
ran_fspr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  fdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo", behavior == "prox", sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)
  
  ran_fspr_w_re <- gam(deg ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fspr_no_re <- gam(deg ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fspr_rde_list[[i]] <- dev_expl_re(ran_fspr_w_re, ran_fspr_no_re)
  
}
Sys.time() - t # 3.2 min


save(ran_fspr_rde_list, file = "data/models gam - ran - RE deviance explained - H1 FEMALE mixed sex prox strength.Rdata") 

# -- prox bt trans ----
ran_fbtpr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  fdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001)
  
  ran_fbtpr_w_re <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fbtpr_no_re <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fbtpr_rde_list[[i]] <- dev_expl_re(ran_fbtpr_w_re, ran_fbtpr_no_re)
  
}
Sys.time() - t # 10.4 min

ran_ftrpr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  fdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_ftrpr_w_re <- gam(trans ~ s(age_mid_year, k = 3) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 3) + s(chimp_id, bs = "re"), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_ftrpr_no_re <- gam(trans ~ s(age_mid_year, k = 3) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 3), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_ftrpr_rde_list[[i]] <- dev_expl_re(ran_ftrpr_w_re, ran_ftrpr_no_re)
}
Sys.time() - t # 1.6 min

save(ran_fbtpr_rde_list, ran_ftrpr_rde_list, file = "data/models gam - ran - RE deviance explained - H2 FEMALE mixed sex prox bt and trans.Rdata")

# -- prox ec ----

ran_fecpr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  fdata <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_fecpr_w_re <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fecpr_no_re <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = fdata, family = gaussian(link = "log"), method = "REML")
  ran_fecpr_rde_list[[i]] <- dev_expl_re(ran_fecpr_no_re, ran_fecpr_no_re)
  
  
} # 2.6 min
Sys.time() - t 

#save(ran_fecpr_rde_list, file = "data/models gam - ran - RE deviance explained - H3 FEMALE mixed sex prox ec.Rdata")

## don't add female prop cyc from here down ----
# - 2. Same sex networks ------
# -- gm deg strength in -----
# load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)
# load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)

# load ran mods w RE
# load("data/models gam - ran - H1 same sex grooming random deg and strength in.Rdata", verbose = T)

ran_fs_digr_rde_list <- vector(length = 1000)
ran_fs_sigr_rde_list <- vector(length = 1000)

ran_ms_digr_rde_list <- vector(length = 1000)
ran_ms_sigr_rde_list <- vector(length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  data_w <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex != "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  #no re mod paired w with RE mod, saved to RDE list
  ran_fsdigr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_digr_rde_list[[i]] <- dev_expl_re(ran_fsdigr_list[[i]], ran_fsdigr_no_re)
  
  ran_fssigr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data_w %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_sigr_rde_list[[i]] <- dev_expl_re(ran_fssigr_list[[i]], ran_fssigr_no_re)
  
  ran_msdigr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_digr_rde_list[[i]] <- dev_expl_re(ran_msdigr_list[[i]], ran_msdigr_no_re)
  
  ran_mssigr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_sigr_rde_list[[i]] <- dev_expl_re(ran_mssigr_list[[i]], ran_mssigr_no_re)
}
Sys.time() - t # 5.3 min

# save(ran_fs_digr_rde_list, ran_fs_sigr_rde_list,
#      ran_ms_digr_rde_list, ran_ms_sigr_rde_list,
#      file = "data/models gam - ran - RE deviance explained - H1 same sex grooming random deg and strength in.Rdata")

# -- gm deg strength out -----
# load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)
# load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)

# load ran mods w RE
# load("data/models gam - ran - H1 same sex grooming random deg and strength out.Rdata", verbose = T)

ran_fs_dogr_rde_list <- vector(length = 1000)
ran_fs_sogr_rde_list <- vector(length = 1000)

ran_ms_dogr_rde_list <- vector(length = 1000)
ran_ms_sogr_rde_list <- vector(length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  data_w <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex != "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  
  ran_fsdogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_dogr_rde_list[[i]] <- dev_expl_re(ran_fsdogr_list[[i]], ran_fsdogr_no_re)
  
  ran_fssogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data_w %>% filter(sex == "F"), family = Gamma(link = "log"), method = "REML")
  ran_fs_sogr_rde_list[[i]] <- dev_expl_re(ran_fssogr_list[[i]], ran_fssogr_no_re)
  
  
  ran_msdogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) ,  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_dogr_rde_list[[i]] <- dev_expl_re(ran_msdogr_list[[i]], ran_msdogr_no_re)
  
  ran_mssogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) ,  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_sogr_rde_list[[i]] <- dev_expl_re(ran_mssogr_list[[i]], ran_mssogr_no_re)
}
Sys.time() - t #  min

 # save(ran_fs_dogr_rde_list, ran_fs_sogr_rde_list,
 #      ran_ms_dogr_rde_list, ran_ms_sogr_rde_list,
 #     file = "data/models gam - ran - RE deviance explained - H1 same sex grooming random deg and strength out.Rdata")

# -- gm bt trans -----
# load ran mods w RE
# load("data/models gam - ran - H2 same sex grooming bt and trans.Rdata", verbose = T)

ran_fs_btgr_rde_list <- vector(length = 1000)
ran_fs_trgr_rde_list <- vector(length = 1000)

ran_ms_btgr_rde_list <- vector(length = 1000)
ran_ms_trgr_rde_list <- vector(length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "total_grooming") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)
  
  
  ran_fsbtgr_no_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) ,  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_btgr_rde_list[[i]] <- dev_expl_re(ran_fsbtgr_list[[i]], ran_fsbtgr_no_re)
  
  ran_fs_trgr_no_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) ,  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_trgr_rde_list[[i]] <- dev_expl_re(ran_fstrgr_list[[i]], ran_fstrgr_no_re)
  
  ran_ms_btgr_no_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_btgr_rde_list[[i]] <- dev_expl_re(ran_msbtgr_list[[i]],ran_msbtgr_no_re)
  
  ran_ms_trgr_no_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_trgr_rde_list[[i]] <- dev_expl_re(ran_mstrgr_list[[i]], ran_mstrgr_no_re)
  
}
Sys.time() - t # 7.9 min

# save(ran_fs_btgr_rde_list, ran_fs_trgr_rde_list,
#     ran_ms_btgr_rde_list, ran_ms_trgr_rde_list,
#      file = "data/models gam - ran - RE deviance explained - H2 same sex grooming random bt and trans.Rdata")

# -- gm ec ------
# load ran mods w RE
# load("data/models gam - ran - H3 same sex grooming ec.Rdata", verbose = T)

ran_fs_ecgr_rde_list <- vector(length = 1000)
ran_ms_ecgr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "total_grooming") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_fsecgr_no_re <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_ecgr_rde_list[[i]] <- dev_expl_re(ran_fsecgr_list[[i]], ran_fsecgr_no_re )
  
  ran_msecgr_no_re <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_ecgr_rde_list[[i]] <- dev_expl_re(ran_msecgr_list[[i]], ran_msecgr_no_re)
  
}
Sys.time() - t # 3.6 min

#save(ran_fs_ecgr_rde_list, ran_ms_ecgr_rde_list, file = "data/models gam - ran - RE deviance explained - H3 same sex grooming random ec.Rdata")

# -- prox strength ----
# load ran mods w RE
load("data/models gam - ran - H1 same sex prox random strength.Rdata", verbose = T)
ran_fs_spr_rde_list <- vector(length = 1000)
ran_ms_spr_rde_list <- vector(length = 1000)

ran_fs_spr_rde_list <- unlist(ran_fs_spr_rde_list)
ran_ms_spr_rde_list <- unlist(ran_ms_spr_rde_list)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)
  
  ran_fsspr_no_re <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_spr_rde_list[[i]] <- dev_expl_re(ran_fsspr_list[[i]], ran_fsspr_no_re)
  
  ran_mssp_no_re <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_spr_rde_list[[i]] <- dev_expl_re(ran_msspr_list[[i]], ran_mssp_no_re)
  
}
Sys.time() - t #  2.25 min

# save(ran_fs_spr_rde_list, ran_ms_spr_rde_list,
#      file = "data/models gam - ran - RE deviance explained - H1 same sex prox random strength.Rdata")

# -- prox bt trans ----
# load ran mods w RE
# load("data/models gam - ran - H2 same sex prox bt and trans.Rdata", verbose = T)

ran_fs_btpr_rde_list <- vector(length = 1000)
ran_fs_trpr_rde_list <- vector(length = 1000)

ran_ms_btpr_rde_list <- vector(length = 1000)
ran_ms_trpr_rde_list <- vector(length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)
  
  ran_fsbtpr_no_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_btpr_rde_list[[i]] <- dev_expl_re(ran_fsbtpr_list[[i]], ran_fsbtpr_no_re)
  ran_fstrpr_no_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_trpr_rde_list[[i]] <- dev_expl_re(ran_fstrpr_list[[i]], ran_fstrpr_no_re )
  
  ran_msbtpr_no_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_btpr_rde_list[[i]] <- dev_expl_re(ran_msbtpr_list[[i]], ran_msbtpr_no_re)
  ran_mstrpr_no_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) ,  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_trpr_rde_list[[i]] <- dev_expl_re(ran_mstrpr_list[[i]], ran_mstrpr_no_re)
}
Sys.time() - t #5.7min

# save(ran_fs_btpr_rde_list, ran_fs_trpr_rde_list,
#     ran_ms_btpr_rde_list, ran_ms_trpr_rde_list,
#     file = "data/models gam - ran - RE deviance explained - H2 same sex prox bt and trans.Rdata")

# -- prox ec ----
# load ran mods w RE
load("data/models gam - ran - H3 same sex prox ec.Rdata", verbose = T)

ran_fs_ecpr_rde_list <- vector(length = 1000)
ran_ms_ecpr_rde_list <- vector(length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_fsecpr_no_re <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fs_ecpr_rde_list[[i]] <- dev_expl_re(ran_fsecpr_list[[i]], ran_fsecpr_no_re)
  
  ran_msecpr_no_re <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_ms_ecpr_rde_list[[i]] <- dev_expl_re(ran_msecpr_list[[i]], ran_msecpr_no_re)
}
Sys.time() - t #  2.26 min


#save(ran_fs_ecpr_rde_list, ran_ms_ecpr_rde_list, file = "data/models gam - ran - RE deviance explained - H3 same sex prox ec.Rdata")

# Test sig observed dev explained -----
rm(list = ls())
library(tidyverse)
source("functions/functions - dev expl re and test sig dev expl re.R")

# observed RE DE -
load("data/observed deviance explained random effects.Rdata", verbose = T)  
obs_re_devs

# random RE DE -
# Male mixed - all 
#groom
load("data/models gam - ran - RE deviance explained - H1 MALE mixed sex grooming random deg and strength in.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H1 MALE mixed sex grooming random deg and strength out.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H2 MALE mixed sex grooming bt and trans.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H3 MALE mixed sex grooming ec.Rdata", verbose = T)
#prox
load("data/models gam - ran - RE deviance explained - H1 MALE mixed sex prox random strength.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H2 MALE mixed sex prox random bt and trans.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H3 MALE mixed sex prox ec.Rdata", verbose = T)


# Female mixed -
# groom
load("data/models gam - ran - RE deviance explained - H1 FEMALE mixed sex grooming random deg and strength in.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H1 FEMALE mixed sex grooming random deg and strength out.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H2 FEMALE mixed sex grooming bt and trans.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H3 FEMALE mixed sex grooming ec.Rdata", verbose = T)
#prox
load("data/models gam - ran - RE deviance explained - H1 FEMALE mixed sex prox strength.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H2 FEMALE mixed sex prox bt and trans.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H3 FEMALE mixed sex prox ec.Rdata", verbose = T)

# same sex 
#groom
load("data/models gam - ran - RE deviance explained - H1 same sex grooming random deg and strength in.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H1 same sex grooming random deg and strength out.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H2 same sex grooming random bt and trans.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H3 same sex grooming random ec.Rdata", verbose = T)
#prox
load("data/models gam - ran - RE deviance explained - H1 same sex prox random strength.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H2 same sex prox bt and trans.Rdata", verbose = T)
load("data/models gam - ran - RE deviance explained - H3 same sex prox ec.Rdata", verbose = T)


# match vector name to mod name, add vector as list column, 
# compare obs_re_dev to vector in list col

fem_mixed <- Filter(function(x) "numeric" %in% class(get(x)) & grepl("ran_f(?!s_)", x, perl = T), ls())

fem_same <- Filter(function(x) "numeric" %in% class(get(x)) & grepl("ran_fs_", x, perl = T), ls())

male_mixed <- Filter(function(x) "numeric" %in% class(get(x)) & grepl("ran_m(?!s_)", x, perl = T), ls())

male_same <- Filter(function(x) "numeric" %in% class(get(x)) & grepl("ran_ms_", x, perl = T), ls())


ran_vectors <- c(male_mixed, fem_mixed, fem_same, male_same)

# names of vectors of random deviance explained by re
obs_re_devs$rde_re <- ran_vectors

sig_re_devs <- vector(length = nrow(obs_re_devs))

for(i in seq(nrow(obs_re_devs))){
  # observed deviance explained RE (numeric)
  ode_re <- obs_re_devs[i,"obs_re_dev"]
  # random deviance explained RE (name of vector to get)
  rde_re <- get(obs_re_devs[i,"rde_re"])
  
  sig_re_devs[[i]] <- sum(ode_re > rde_re)/length(rde_re)

  }

# proportion random less than obs
obs_re_devs$prlo <- round(sig_re_devs * 100, 0)

net_order <- c("Mixed sex", "Same sex")
sex_order <- c("Male", "Female")
beh_order <- c("Grooming", "Proximity")
sna_order <- c("In-Degree", "Out-Degree", "In-Strength", "Out-Strength", 
  "Strength", "Local Transitivity", "Betweenness", "Eigenvector Centrality")

rde_re_tab_base <- obs_re_devs %>%
  mutate(sig = ifelse(prlo >= 95, "*", "")) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate(
    sex = case_when(
      grepl("_m", rde_re) ~ "Male",
      grepl("_f", rde_re) ~ "Female"),
    network = case_when(
      grepl("s_", rde_re) ~ "Same sex",
      TRUE ~ "Mixed sex"
    )) %>%
  mutate(
    behavior = case_when(
      grepl("gr", mod) ~ "Grooming",
      TRUE ~ "Proximity")
    ) %>%
  mutate(
    sna = case_when(
      grepl("di", mod) ~ "In-Degree",
      grepl("do", mod) ~ "Out-Degree",
      grepl("si", mod) ~ "In-Strength",
      grepl("so", mod) ~ "Out-Strength",
      grepl("sp", mod) ~ "Strength",
      grepl("(?<!es)tr", mod, perl = T) ~ "Local Transitivity",
      grepl("bt", mod) ~ "Betweenness",
      grepl("ec", mod) ~ "Eigenvector Centrality"
    )) %>%
  # add levels
  mutate(behavior = factor(behavior, levels = beh_order), 
         sna = factor(sna, levels = sna_order),
         sex = factor(sex, levels = sex_order),
         network = factor(network, levels = net_order))

rde_re_tab_summ <- rde_re_tab_base %>%
  arrange(sex, network, sna, behavior) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(re_de_sig =  paste0(obs_re_dev, " [", prlo, "]")) %>%
  select(mod,sex, network, sna, behavior, re_de_sig, sig) 
           
rde_re_tab_summ[duplicated(rde_re_tab_summ[,c("sex","network")]), c("sex","network")] <- c("","")

rde_re_tab_supp <- rde_re_tab_base %>%
  arrange(behavior, network, sex, sna) %>%
  select(mod,behavior, network, sex, sna, obs_re_dev, prlo, sig) %>%
  mutate_if(is.factor, as.character)

rde_re_tab_supp[duplicated(rde_re_tab_supp[,c("behavior","network", "sex")]), c("behavior","network", "sex")] <-c("","", "")


# write.table(rde_re_tab_summ, file = "results/tables/GAMs/Repeatability Table4 summary.txt", quote = FALSE, sep = "/", row.names = FALSE)
# write.table(rde_re_tab_supp, file = "results/tables/GAMs/Repeatability supplemental.txt", quote = FALSE, sep = "/", row.names = FALSE)



# -----
# ----- 




