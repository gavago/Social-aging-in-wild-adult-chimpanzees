library(tidyverse)
library(mgcv)
library(progress)

source("data/data sets for gams.R")
# load data sets
load("data/ran9 - estrus dir uw - node (age rank prop_cyc chimp_id) randomized sna measures directed unweighted.Rdata", verbose = T)
load("data/ran10 - estrus dir w - node (age rank prop_cyc chimp_id) randomized sna measures directed weighted.Rdata", verbose = T)
load("data/ran11 - estrus undir w - node (age rank prop_cyc chimp_id) randomized sna measures undirected weighted.Rdata", verbose = T)

#####  WITH RANK ############
# 1. Run ran mods ----

## GROOMING -----

# In Degree -----
#ii_digr_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

ran_digr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_digr_estr_list[[i]] <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #17.2 min # 


# Out Degree --------
#ii_dogr_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

ran_dogr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_dogr_estr_list[[i]] <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 23.1 min

save(ran_digr_estr_list, ran_dogr_estr_list, file = "data/temp random f estrus digr dogr.Rdata")

# In Strength -----
#ii_sigr_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sigr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_sigr_estr_list[[i]] <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = Gamma(link = "log"), method = "REML")
  
}
Sys.time() - t # 6.7

# Out Strength -----
#ii_sogr_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sogr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_sogr_estr_list[[i]] <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = Gamma(link = "log"), method = "REML")
  
}
Sys.time() - t # 7.3

#save(ran_digr_estr_list, ran_dogr_estr_list, ran_sigr_estr_list, ran_sogr_estr_list, file = "data/models gam - ran - H1 f estrus mixed sex grooming in out degree n strength w rank.Rdata")

# ------ extract Fs -----
load("data/models gam - ran - H1 f estrus mixed sex grooming in out degree n strength w rank.Rdata", verbose = T)

sF_ran_digr <- vector("list", length = 1000)
sF_ran_dogr <- vector("list", length = 1000)
sF_ran_sigr <- vector("list", length = 1000)
sF_ran_sogr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_digr[[i]] <- summary(ran_digr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_digr_estr_list[[i]])$r.sq, 2))
  
  sF_ran_dogr[[i]] <- summary(ran_dogr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_dogr_estr_list[[i]])$r.sq,2))
  
  sF_ran_sigr[[i]] <- summary(ran_sigr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_sigr_estr_list[[i]])$r.sq, 2))
  
  sF_ran_sogr[[i]] <- summary(ran_sogr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_sogr_estr_list[[i]])$r.sq, 2))
  
}
Sys.time() - t # 1.2 min

F_ran_digr_estr <- do.call("rbind", sF_ran_digr)
F_ran_dogr_estr <- do.call("rbind", sF_ran_dogr)
F_ran_sigr_estr <- do.call("rbind", sF_ran_sigr)
F_ran_sogr_estr <- do.call("rbind", sF_ran_sogr)

# Trans -----
#ii_trgr_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ran_trgr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_trgr_estr_list[[i]] <- gam(trans ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 8.4 min



# Betweenness -----
#ii_btgr_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

ran_btgr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001)
  
  ran_btgr_estr_list[[i]] <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 2.1 hours

any(is.null(ran_btgr_estr_list))


#save(ran_trgr_estr_list, ran_btgr_estr_list, file = "data/models gam - ran - H2 f estrus mixed sex grooming trans bt w rank.Rdata")


# ------ extract Fs -----
load("data/models gam - ran - H2 f estrus mixed sex grooming trans bt w rank.Rdata", verbose = T)

sF_ran_trgr <- vector("list", length = 1000)
sF_ran_btgr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_trgr[[i]] <- summary(ran_trgr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_trgr_estr_list[[i]])$r.sq, 2))
  
  sF_ran_btgr[[i]] <- summary(ran_btgr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_btgr_estr_list[[i]])$r.sq,2))
  
}
Sys.time() - t # 32 sec

F_ran_trgr_estr <- do.call("rbind", sF_ran_trgr)
F_ran_btgr_estr <- do.call("rbind", sF_ran_btgr)



# EC -----
# ii_ecgr_f_estr <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ran_ecgr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width = 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecgr_estr_list[[i]] <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 40 min

#save(ran_ecgr_estr_list, file = "data/models gam - ran - H3 f estrus mixed sex grooming ec w rank.Rdata")

# ---- extract Fs -----
load("data/models gam - ran - H3 f estrus mixed sex grooming ec w rank.Rdata", verbose = T)

sF_ran_ecgr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecgr[[i]] <- summary(ran_ecgr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_ecgr_estr_list[[i]])$r.sq, 2))
  
}
Sys.time() - t # 18.5 sec

F_ran_ecgr_estr <- do.call("rbind", sF_ran_ecgr)


## PROX ----
# Strength ----
#ii_spr_f_estr <- gam(deg ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ran_spr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo", behavior == "prox", sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)
  
  ran_spr_estr_list[[i]] <- gam(deg ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 10 min

#save(ran_spr_estr_list, file = "data/models gam - ran - H1 f estrus mixed sex prox strength w rank.Rdata")
# ----- extract Fs ------
load("data/models gam - ran - H1 f estrus mixed sex prox strength w rank.Rdata", verbose = T)
sF_ran_spr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_spr[[i]] <- summary(ran_spr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_spr_estr_list[[i]])$r.sq, 2))
  
}
Sys.time() - t # 20 secs

F_ran_spr_estr <- do.call("rbind", sF_ran_spr)


# Trans -----
#ii_trpr_f_estr <- gam(trans ~ s(age_mid_year, k = 3) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 3) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ran_trpr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_trpr_estr_list[[i]] <- gam(trans ~ s(age_mid_year, k = 3) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 3) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 



# Betweenness -----
#ii_btpr_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ran_btpr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001)
  
  ran_btpr_estr_list[[i]] <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 1.24 hours


#save(ran_trpr_estr_list, ran_btpr_estr_list, file = "data/models gam - ran - H2 f estrus mixed sex prox trans bt w rank.Rdata")

#----- extract Fs ------
load("data/models gam - ran - H2 f estrus mixed sex prox trans bt w rank.Rdata", verbose = T)

sF_ran_trpr <- vector("list", length = 1000)
sF_ran_btpr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_trpr[[i]] <- summary(ran_trpr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_trpr_estr_list[[i]])$r.sq, 2))
  
  sF_ran_btpr[[i]] <- summary(ran_btpr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_btpr_estr_list[[i]])$r.sq,2))
  
}
Sys.time() - t # 36 sec

F_ran_trpr_estr <- do.call("rbind", sF_ran_trpr)
F_ran_btpr_estr <- do.call("rbind", sF_ran_btpr)


# EC -----
#ii_ecpr_f_estr <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

ran_ecpr_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecpr_estr_list[[i]] <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
} # 13 min
Sys.time() - t 


#save(ran_ecpr_estr_list, file = "data/models gam - ran - H3 f estrus mixed sex prox ec w rank.Rdata")

# ----- extract Fs -----
load("data/models gam - ran - H3 f estrus mixed sex prox ec w rank.Rdata", verbose = T)

sF_ran_ecpr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecpr[[i]] <- summary(ran_ecpr_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_ecpr_estr_list[[i]])$r.sq, 2))
  
}
Sys.time() - t # 22 sec

F_ran_ecpr_estr <- do.call("rbind", sF_ran_ecpr)


#####  save ran Fs & R ----
# save(F_ran_digr_estr,  F_ran_dogr_estr,
#      F_ran_sigr_estr, F_ran_sogr_estr,
#      F_ran_btgr_estr,  F_ran_trgr_estr, F_ran_ecgr_estr, file = "data/ran Fs gams - f estrus directed and total grooming mixed sex w rank.Rdata")
# 
# save(F_ran_spr_estr,
#      F_ran_btpr_estr,  F_ran_trpr_estr, F_ran_ecpr_estr, file = "data/ran Fs gams - f estrus prox mixed sex w rank.Rdata")

# 2. Evaluate sig ----------
library(tidyverse)
library(magrittr)
source("functions/functions - test sig gamm.R")
load("data/ran Fs gams - f estrus directed and total grooming mixed sex w rank.Rdata", verbose = T)
load( "data/ran Fs gams - f estrus prox mixed sex w rank.Rdata", verbose = T)

# load obs models
load("data/models gam - estrus female w rank in mixed sex net.Rdata", verbose = T)

a <- test_sig_gamm_f_estrus_rank(response = "In-Degree", behavior = "Grooming", 
                                 modr = ii_digr_f_estr, F_ran_r = F_ran_digr_estr) 

b <- test_sig_gamm_f_estrus_rank(response = "Out-Degree", behavior = "Grooming",
                                 modr = ii_dogr_f_estr, F_ran_r = F_ran_dogr_estr) 

c <- test_sig_gamm_f_estrus_rank(response = "In-Strength", behavior = "Grooming",
                                 modr = ii_sigr_f_estr, F_ran_r = F_ran_sigr_estr) 

d <- test_sig_gamm_f_estrus_rank(response = "Out-Strength", behavior = "Grooming",
                                 modr = ii_sogr_f_estr, F_ran_r = F_ran_sogr_estr) 

e <- test_sig_gamm_f_estrus_rank(response = "Local Transitivty", behavior = "Grooming",
                                 modr = ii_trgr_f_estr, F_ran_r = F_ran_trgr_estr)

f <- test_sig_gamm_f_estrus_rank(response = "Betweenness", behavior = "Grooming", 
                                 modr = ii_btgr_f_estr, F_ran_r = F_ran_btgr_estr)

g <- test_sig_gamm_f_estrus_rank(response = "Eigenvector Centrality", behavior = "Grooming",
                                 modr = ii_ecgr_f_estr, F_ran_r = F_ran_ecgr_estr)


f_mixed_g_estr_tab_rank <- rbind(a,b,c,d,e,f,g)


h <- test_sig_gamm_f_estrus_rank(response = "Out-Strength", behavior = "Proximity",
                                 modr = ii_spr_f_estr, F_ran_r = F_ran_spr_estr) 

i <- test_sig_gamm_f_estrus_rank(response = "Betweenness", behavior = "Proximity", 
                                 modr = ii_btpr_f_estr, F_ran_r = F_ran_btpr_estr)

j <- test_sig_gamm_f_estrus_rank(response = "Local Transitivty", behavior = "Proximity",
                                 modr = ii_trpr_f_estr, F_ran_r = F_ran_trpr_estr)

k <- test_sig_gamm_f_estrus_rank(response = "Eigenvector Centrality", behavior = "Proximity",
                                 modr = ii_ecpr_f_estr, F_ran_r = F_ran_ecpr_estr)

f_mixed_p_estr_tab_rank <- rbind(h,i,j,k)


# write.table(f_mixed_g_estr_tab_rank, file = "results/tables/GAMs/female estrus grooming w rank.txt", sep = "/", row.names = F, quote = F)
# write.table(f_mixed_p_estr_tab_rank, file = "results/tables/GAMs/female estrus proximity w rank.txt", sep = "/", row.names = F, quote = F)




#####
#####
###############  WITH RANK - permutation within females alone ############
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)
load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)
load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)


# 1. Run ran mods ----

## GROOMING -----

# In Degree -----
#ii_digr_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

ran_digr_estr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_digr_estr_list_wsp[[i]] <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 9.9 m


# Out Degree --------
ran_dogr_estr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_dogr_estr_list_wsp[[i]] <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 9.6 min

# In Strength -----
#ii_sigr_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sigr_estr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_sigr_estr_list_wsp[[i]] <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = Gamma(link = "log"), method = "REML")
  
}
Sys.time() - t # 4.9

# Out Strength -----
ran_sogr_estr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_sogr_estr_list_wsp[[i]] <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = Gamma(link = "log"), method = "REML")
  
}
Sys.time() - t #  6.1 min

#save(ran_digr_estr_list_wsp, ran_dogr_estr_list_wsp, ran_sigr_estr_list_wsp, ran_sogr_estr_list_wsp, file = "data/models gam - ran - H1 f estrus mixed sex grooming in out degree n strength w rank WITHIN SEX PERM.Rdata")

# ------ extract Fs -----
load("data/models gam - ran - H1 f estrus mixed sex grooming in out degree n strength w rank WITHIN SEX PERM.Rdata", verbose = T)

sF_ran_digr_wsp <- vector("list", length = 1000)
sF_ran_dogr_wsp <- vector("list", length = 1000)
sF_ran_sigr_wsp <- vector("list", length = 1000)
sF_ran_sogr_wsp <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_digr_wsp[[i]] <- summary(ran_digr_estr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_digr_estr_list_wsp[[i]])$r.sq, 2))
  
  sF_ran_dogr_wsp[[i]] <- summary(ran_dogr_estr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_dogr_estr_list_wsp[[i]])$r.sq,2))
  
  sF_ran_sigr_wsp[[i]] <- summary(ran_sigr_estr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_sigr_estr_list_wsp[[i]])$r.sq, 2))
  
  sF_ran_sogr_wsp[[i]] <- summary(ran_sogr_estr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_sogr_estr_list_wsp[[i]])$r.sq, 2))
  
}
Sys.time() - t # 1.2 min

F_ran_digr_estr_wsp <- do.call("rbind", sF_ran_digr_wsp)
F_ran_dogr_estr_wsp <- do.call("rbind", sF_ran_dogr_wsp)
F_ran_sigr_estr_wsp <- do.call("rbind", sF_ran_sigr_wsp)
F_ran_sogr_estr_wsp <- do.call("rbind", sF_ran_sogr_wsp)

# Trans -----
#ii_trgr_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ran_trgr_estr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_trgr_estr_list_wsp[[i]] <- gam(trans ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 8.4 min



# Betweenness -----
#ii_btgr_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

ran_btgr_estr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F")  %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001)
  
  ran_btgr_estr_list_wsp[[i]] <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 2.1 hours

any(is.null(ran_btgr_estr_list_wsp))

save(ran_trgr_estr_list_wsp, ran_btgr_estr_list_wsp, file = "data/models gam - ran - H2 f estrus mixed sex grooming trans bt w rank WITHIN SEX PERM.Rdata")


# ------ extract Fs -----
load("data/models gam - ran - H2 f estrus mixed sex grooming trans bt w rank WITHIN SEX PERM.Rdata", verbose = T)

sF_ran_trgr_wsp <- vector("list", length = 1000)
sF_ran_btgr_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_trgr_wsp[[i]] <- summary(ran_trgr_estr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_trgr_estr_list_wsp[[i]])$r.sq, 2))
  
  sF_ran_btgr_wsp[[i]] <- summary(ran_btgr_estr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_btgr_estr_list_wsp[[i]])$r.sq,2))
  
}
Sys.time() - t # 32 sec

F_ran_trgr_estr_wsp <- do.call("rbind", sF_ran_trgr_wsp)
F_ran_btgr_estr_wsp <- do.call("rbind", sF_ran_btgr_wsp)



# EC -----
# ii_ecgr_f_estr <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ran_ecgr_estr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width = 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecgr_estr_list_wsp[[i]] <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 24.4 min

#save(ran_ecgr_estr_list_wsp, file = "data/models gam - ran - H3 f estrus mixed sex grooming ec w rank WITHIN SEX PERM.Rdata")

# ---- extract Fs -----
load("data/models gam - ran - H3 f estrus mixed sex grooming ec w rank WITHIN SEX PERM.Rdata", verbose = T)

sF_ran_ecgr_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecgr_wsp[[i]] <- summary(ran_ecgr_estr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_ecgr_estr_list_wsp[[i]])$r.sq, 2))
  
}
Sys.time() - t # 18.5 sec

F_ran_ecgr_estr_wsp <- do.call("rbind", sF_ran_ecgr_wsp)


###  save ran Fs ----
# save(F_ran_digr_estr_wsp,  F_ran_dogr_estr_wsp,
#      F_ran_sigr_estr_wsp, F_ran_sogr_estr_wsp,
#      F_ran_btgr_estr_wsp,  F_ran_trgr_estr_wsp, F_ran_ecgr_estr_wsp, file = "data/ran Fs gams - f estrus directed and total grooming mixed sex w rank WITHIN SEX PERM.Rdata")



# 2. Evaluate sig ----------
library(tidyverse)
library(magrittr)
source("functions/functions - test sig gamm.R")
load("data/ran Fs gams - f estrus directed and total grooming mixed sex w rank WITHIN SEX PERM.Rdata", verbose = T)

# load obs models
load("data/models gam - estrus female w rank in mixed sex net.Rdata", verbose = T)

a <- test_sig_gamm_f_estrus_rank(response = "In-Degree", behavior = "Grooming", 
                                 modr = ii_digr_f_estr, F_ran_r = F_ran_digr_estr_wsp) 

b <- test_sig_gamm_f_estrus_rank(response = "Out-Degree", behavior = "Grooming",
                                 modr = ii_dogr_f_estr, F_ran_r = F_ran_dogr_estr_wsp) 

c <- test_sig_gamm_f_estrus_rank(response = "In-Strength", behavior = "Grooming",
                                 modr = ii_sigr_f_estr, F_ran_r = F_ran_sigr_estr_wsp) 

d <- test_sig_gamm_f_estrus_rank(response = "Out-Strength", behavior = "Grooming",
                                 modr = ii_sogr_f_estr, F_ran_r = F_ran_sogr_estr_wsp) 

e <- test_sig_gamm_f_estrus_rank(response = "Local Transitivity", behavior = "Grooming",
                                 modr = ii_trgr_f_estr, F_ran_r = F_ran_trgr_estr_wsp)

f <- test_sig_gamm_f_estrus_rank(response = "Betweenness", behavior = "Grooming", 
                                 modr = ii_btgr_f_estr, F_ran_r = F_ran_btgr_estr_wsp)

g <- test_sig_gamm_f_estrus_rank(response = "Eigenvector Centrality", behavior = "Grooming",
                                 modr = ii_ecgr_f_estr, F_ran_r = F_ran_ecgr_estr_wsp)


f_mixed_g_estr_tab_rank_wsp <- rbind(a,b,c,d,e,f,g)

#write.table(f_mixed_g_estr_tab_rank_wsp, file = "results/tables/GAMs/female estrus grooming w rank WITHIN SEX PERMUTATION.txt", sep = "/", row.names = F, quote = F)



#################################### ########
######## WITHOUT RANK ########
# 1. Run ran mods ----

## Grooming -----

# In Degree -----
#ii_dig_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

ran_dig_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_dig_estr_list[[i]] <- gam(deg_in ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 9.5


# Out Degree --------
#ii_dog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

ran_dog_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_dog_estr_list[[i]] <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #24 min


# In Strength -----
#ii_sig_f_estr <- gam(deg_in ~ s(age_mid_year) + s(prop_cyc) + ti(age_mid_year, prop_cyc) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sig_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_sig_estr_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = Gamma(link = "log"), method = "REML")
  
}
Sys.time() - t #

# Out Strength -----
#ii_sog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(avg_rank) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sog_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_sog_estr_list[[i]] <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = Gamma(link = "log"), method = "REML")
  
}
Sys.time() - t #

#save(ran_dig_estr_list, ran_dog_estr_list, ran_sig_estr_list, ran_sog_estr_list, file = "data/models gam - ran - H1 f estrus mixed sex grooming in out degree n strength.Rdata")

# ------ extract Fs -----
load("data/models gam - ran - H1 f estrus mixed sex grooming in out degree n strength.Rdata", verbose = T)

sF_ran_dig <- vector("list", length = 1000)
sF_ran_dog <- vector("list", length = 1000)
sF_ran_sig <- vector("list", length = 1000)
sF_ran_sog <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_dig[[i]] <- summary(ran_dig_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_dig_list[[i]])$r.sq, 2))
  
  sF_ran_dog[[i]] <- summary(ran_dog_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_dog_list[[i]])$r.sq,2))
  
  sF_ran_sig[[i]] <- summary(ran_sig_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_sig_list[[i]])$r.sq, 2))
  
  sF_ran_sog[[i]] <- summary(ran_sog_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_sog_list[[i]])$r.sq, 2))
  
}
Sys.time() - t # 1.5 min

F_ran_dig_estr <- do.call("rbind", sF_ran_dig)
F_ran_dog_estr <- do.call("rbind", sF_ran_dog)
F_ran_sig_estr <- do.call("rbind", sF_ran_sig)
F_ran_sog_estr <- do.call("rbind", sF_ran_sog)

# Trans -----
#ii_trg_f_estr <- gam(trans ~ s(age_mid_year, k = 3) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")

ran_trg_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_trg_estr_list[[i]] <- gam(trans ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #



# Betweenness -----
#ii_btg_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")

ran_btg_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001)
  
  ran_btg_estr_list[[i]] <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #

any(is.null(ran_btg_list))

#save(ran_trg_estr_list, ran_btg_estr_list, file = "data/models gam - ran - H2 f estrus mixed sex grooming trans bt.Rdata")

# ------ extract Fs -----
load("data/models gam - ran - H2 f estrus mixed sex grooming trans bt.Rdata", verbose = T)

sF_ran_trg <- vector("list", length = 1000)
sF_ran_btg <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_trg[[i]] <- summary(ran_trg_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_trg_list[[i]])$r.sq, 2))
  
  sF_ran_btg[[i]] <- summary(ran_btg_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_btg_list[[i]])$r.sq,2))
  
}
Sys.time() - t # 1.5 min

F_ran_trg_estr <- do.call("rbind", sF_ran_trg)
F_ran_btg_estr <- do.call("rbind", sF_ran_btg)


# EC -----
ran_ecg_estr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecg_estr_list[[i]] <- gam(ec ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

}
Sys.time() - t # 7 min

#save(ran_ecg_estr_list, file = "data/models gam - ran - H3 f estrus mixed sex grooming ec.Rdata")
# ---- extract Fs -----
load("data/models gam - ran - H3 f estrus mixed sex grooming ec.Rdata", verbose = T)

sF_ran_ecg <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecg[[i]] <- summary(ran_ecg_estr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_ecg_list[[i]])$r.sq, 2))

}
Sys.time() - t # 1.5 min

F_ran_ecg_estr <- do.call("rbind", sF_ran_ecg)


## Proximity ----
# Strength ----
#ii_sog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sp_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo", behavior == "prox", sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)
  
  ran_sp_list[[i]] <- gam(deg ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #5.4


#save(ran_sp_list, file = "data/models gam - ran - H1 f estrus mixed sex prox strength.Rdata")
# ----- extract Fs ------
load("data/models gam - ran - H1 f estrus mixed sex prox strength.Rdata", verbose = T)
sF_ran_sp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_sp[[i]] <- summary(ran_sp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_sp_list[[i]])$r.sq, 2))
  
}
Sys.time() - t 

F_ran_sp_estr <- do.call("rbind", sF_ran_sp)



# Trans -----
#ii_trg_f_estr <- gam(trans ~ s(age_mid_year, k = 3) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")

ran_trp_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_trp_list[[i]] <- gam(trans ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #4.4



# Betweenness -----
ran_btp_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001)
  
  ran_btp_list[[i]] <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #24 min

#save(ran_trp_list, ran_btp_list, file = "data/models gam - ran - H2 f estrus mixed sex prox trans bt.Rdata")
#----- extract Fs ------
load("data/models gam - ran - H2 f estrus mixed sex prox trans bt.Rdata", verbose = T)

sF_ran_trp <- vector("list", length = 1000)
sF_ran_btp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_trp[[i]] <- summary(ran_trp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_trp_list[[i]])$r.sq, 2))
  
  sF_ran_btp[[i]] <- summary(ran_btp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_btp_list[[i]])$r.sq,2))
  
}
Sys.time() - t # 1.5 min

F_ran_trp_estr <- do.call("rbind", sF_ran_trp)
F_ran_btp_estr <- do.call("rbind", sF_ran_btp)


# EC -----
ran_ecp_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecp_list[[i]] <- gam(ec ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
  
} #6.7
Sys.time() - t 

#save(ran_ecp_list, file = "data/models gam - ran - H3 f estrus mixed sex prox ec.Rdata")

# ----- extract Fs -----
load("data/models gam - ran - H3 f estrus mixed sex prox ec.Rdata", verbose = T)

sF_ran_ecp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecp[[i]] <- summary(ran_ecp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_ecp_list[[i]])$r.sq, 2))
  
}
Sys.time() - t # 18

F_ran_ecp_estr <- do.call("rbind", sF_ran_ecp)



#####  save ran Fs & R ----
# save(F_ran_dig_estr,  F_ran_dog_estr,
#      F_ran_sig_estr, F_ran_sog_estr,
#      F_ran_btg_estr,  F_ran_trg_estr, F_ran_ecg_estr, file = "data/ran Fs gams - f estrus directed and total grooming mixed sex.Rdata")

# save(F_ran_sp_estr,
#      F_ran_btp_estr,  F_ran_trp_estr, F_ran_ecp_estr, file = "data/ran Fs gams - f estrus prox mixed sex.Rdata")


# 2. Evaluate sig ----------
source("functions/functions - test sig gamm.R")
load("data/ran Fs gams - f estrus directed and total grooming mixed sex.Rdata", verbose = T)
load( "data/ran Fs gams - f estrus prox mixed sex.Rdata", verbose = T)

# load obs models
load("data/models - estrus female in mixed sex net.R", verbose = T)


a <- test_sig_gamm_f_estrus(response = "In-Degree", behavior = "Grooming", 
                         mod = ii_dig_f_estr, F_ran = F_ran_dig_estr) 

b <- test_sig_gamm_f_estrus(response = "Out-Degree", behavior = "Grooming",
                         mod = ii_dog_f_estr, F_ran = F_ran_dog_estr) 

c <- test_sig_gamm_f_estrus(response = "In-Strength", behavior = "Grooming",
                         mod = ii_sig_f_estr, F_ran = F_ran_sig_estr) 

d <- test_sig_gamm_f_estrus(response = "Out-Strength", behavior = "Grooming",
                         mod = ii_sog_f_estr, F_ran = F_ran_sog_estr) 

e <- test_sig_gamm_f_estrus(response = "Betweenness", behavior = "Grooming", 
                         mod = ii_btg_f_estr, F_ran = F_ran_btg_estr)

f <- test_sig_gamm_f_estrus(response = "Local Transitivty", behavior = "Grooming",
                         mod = ii_trg_f_estr, F_ran = F_ran_trg_estr)

g <- test_sig_gamm_f_estrus(response = "Eigenvector Centrality", behavior = "Grooming",
                         mod = ii_ecg_f_estr, F_ran = F_ran_ecg_estr)


f_mixed_g_estr_tab <- rbind(a,b,c,d,e,f,g)


h <- test_sig_gamm_f_estrus(response = "Out-Strength", behavior = "Proximity",
                            mod = ii_sp_f_estr, F_ran = F_ran_sp_estr) 

i <- test_sig_gamm_f_estrus(response = "Betweenness", behavior = "Proximity", 
                            mod = ii_btp_f_estr, F_ran = F_ran_btp_estr)

j <- test_sig_gamm_f_estrus(response = "Local Transitivty", behavior = "Proximity",
                            mod = ii_trp_f_estr, F_ran = F_ran_trp_estr)

k <- test_sig_gamm_f_estrus(response = "Eigenvector Centrality", behavior = "Proximity",
                            mod = ii_ecp_f_estr, F_ran = F_ran_ecp_estr)

f_mixed_p_estr_tab <- rbind(h,i,j,k)


#write.table(f_mixed_g_estr_tab, file = "results/tables/GAMs/female estrus grooming.txt", sep = "/", row.names = F, quote = F)
#write.table(f_mixed_p_estr_tab, file = "results/tables/GAMs/female estrus proximity.txt", sep = "/", row.names = F, quote = F)

read_table("results/tables/GAMs/female estrus grooming.txt", sep = "/")

# f_mixed_g_estr_tab 
# f_mixed_p_estr_tab



