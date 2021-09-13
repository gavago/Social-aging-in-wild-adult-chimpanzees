library(tidyverse)
library(mgcv)
library(progress)

# A. -- load node-randomized datasets ----
# undir w
load("data/ran1 - both sexes w - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)
load("data/ran1 - both sexes w - rank within sex - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

# dir gm w
load("data/ran5 - both sexes w - node (sex age rank chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)
load("data/ran5 - both sexes w - rank within sex - node (sex age rank chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)

load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)

# dir gm uw
load("data/ran7 - both sexes uw - node (sex age rank chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)
load("data/ran7 - both sexes uw - rank within sex - node (sex age rank chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)

load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)


# B. Grooming mixed sex networks --------
#   1. Random F stat extraction -----
#     H1 - mixed sex Strength and In-Degree/out directed grooming --- mixed sex ----
#     - Degree & In-Strength - storage of Fs from randomized data models ----
#       - run ran mods -----

ran_dig_list <- vector("list", length = 1000)
ran_digr_list <- vector("list", length = 1000)
ran_sig_list <- vector("list", length = 1000)
ran_sigr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data_w <- list_ran_dir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_both_sex_uw[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_dig_list[[i]] <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_digr_list[[i]] <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_sig_list[[i]] <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  ran_sigr_list[[i]] <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #34 min

# save(ran_dig_list, ran_digr_list, ran_sig_list, ran_sigr_list, file = "data/models gam - ran - H1 mixed sex grooming random deg and In-Strength.Rdata") 
# 7.2.21 resaved after gm code change
# 7.3.21 resaved after gdf_gm_sex_comb appropriately saved w gmi

#       - extract Fs, beta and R -----
load("data/models gam - ran - H1 mixed sex grooming random deg and In-Strength.Rdata", verbose = T)

sF_ran_dig <- vector("list", length = 1000)
sF_ran_digr <- vector("list", length = 1000)
sF_ran_sig <- vector("list", length = 1000)
sF_ran_sigr <- vector("list", length = 1000)

sbetaR_ran_dig <- vector("list", length = 1000)
sbetaR_ran_digr <- vector("list", length = 1000)
sbetaR_ran_sig <- vector("list", length = 1000)
sbetaR_ran_sigr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat
  sF_ran_dig[[i]] <- summary(ran_dig_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  sF_ran_digr[[i]] <- summary(ran_digr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  sF_ran_sig[[i]] <- summary(ran_sig_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sigr[[i]] <- summary(ran_sigr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta 
  sbetaR_ran_dig[[i]] <- summary(ran_dig_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_dig_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_digr[[i]] <- summary(ran_digr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_digr_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_sig[[i]] <- summary(ran_sig_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sig_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_sigr[[i]] <- summary(ran_sigr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sigr_list[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t #

F_ran_dig <- do.call("rbind", sF_ran_dig)
F_ran_digr <- do.call("rbind", sF_ran_digr)
F_ran_sig <- do.call("rbind", sF_ran_sig)
F_ran_sigr <- do.call("rbind", sF_ran_sigr)

betaR_ran_dig <- do.call("rbind", sbetaR_ran_dig)
betaR_ran_digr <- do.call("rbind", sbetaR_ran_digr)
betaR_ran_sig <- do.call("rbind", sbetaR_ran_sig)
betaR_ran_sigr <- do.call("rbind", sbetaR_ran_sigr)

#     - Strength and Out-Degree ------
#       - run ran mods -----

ran_dog_list <- vector("list", length = 1000)
ran_dogr_list <- vector("list", length = 1000)
ran_sog_list <- vector("list", length = 1000)
ran_sogr_list <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
  total = 1000, clear = FALSE, width= 60)

t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data_w <- list_ran_dir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_both_sex_uw[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_dog_list[[i]] <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_dogr_list[[i]] <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_sog_list[[i]] <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  ran_sogr_list[[i]] <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 1.9 hrs

#save(ran_dog_list, ran_dogr_list, ran_sog_list, ran_sogr_list, file = "data/models gam - ran - H1 mixed sex grooming random deg and Out-Strength.Rdata")
# 7.2.21 resaved after gm code change
# 7.3.21 resaved after gdf_gm_sex_comb appropriately saved w gmi


#       - extract Fs, beta and R -----
load("data/models gam - ran - H1 mixed sex grooming random deg and Out-Strength.Rdata", verbose = T)

sF_ran_dog <- vector("list", length = 1000)
sF_ran_dogr <- vector("list", length = 1000)
sF_ran_sog <- vector("list", length = 1000)
sF_ran_sogr <- vector("list", length = 1000)

sbetaR_ran_dog <- vector("list", length = 1000)
sbetaR_ran_dogr <- vector("list", length = 1000)
sbetaR_ran_sog <- vector("list", length = 1000)
sbetaR_ran_sogr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_dog[[i]] <- summary(ran_dog_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_dogr[[i]] <- summary(ran_dogr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sog[[i]] <- summary(ran_sog_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sogr[[i]] <- summary(ran_sogr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta and R
  sbetaR_ran_dog[[i]] <- summary(ran_dog_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_dog_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_dogr[[i]] <- summary(ran_dogr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_dogr_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_sog[[i]] <- summary(ran_sog_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sog_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_sogr[[i]] <- summary(ran_sogr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sogr_list[[i]])$r.sq) %>% round(., 2)
  
  
}
Sys.time() - t # 1.5 min

F_ran_dog <- do.call("rbind", sF_ran_dog)
F_ran_dogr <- do.call("rbind", sF_ran_dogr)
F_ran_sog <- do.call("rbind", sF_ran_sog)
F_ran_sogr <- do.call("rbind", sF_ran_sogr)

betaR_ran_dog <- do.call("rbind", sbetaR_ran_dog)
betaR_ran_dogr <- do.call("rbind", sbetaR_ran_dogr)
betaR_ran_sog <- do.call("rbind", sbetaR_ran_sog)
betaR_ran_sogr <- do.call("rbind", sbetaR_ran_sogr)


#     H2 - mixed sex Betweenness and transitivity undirected grooming --- mixed sex ----
#       - run ran mods -----
ran_btg_list <- vector("list", length = 1000)
ran_btgr_list <- vector("list", length = 1000)
ran_trg_list <- vector("list", length = 1000)
ran_trgr_list <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)

  ran_btg_list[[i]] <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_btgr_list[[i]] <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_trg_list[[i]] <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_trgr_list[[i]] <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")

  }
Sys.time() - t # 47 min

#save(ran_btg_list, ran_btgr_list, ran_trg_list, ran_trgr_list, file = "data/models gam - ran - H2 mixed sex grooming bt and trans.Rdata")

#       - extract Fs, beta and R ------
load("data/models gam - ran - H2 mixed sex grooming bt and trans.Rdata", verbose = T)

sF_ran_btg <- vector("list", length = 1000)
sF_ran_btgr <- vector("list", length = 1000)
sF_ran_trg <- vector("list", length = 1000)
sF_ran_trgr <- vector("list", length = 1000)

sbetaR_ran_btg <- vector("list", length = 1000)
sbetaR_ran_btgr <- vector("list", length = 1000)
sbetaR_ran_trg <- vector("list", length = 1000)
sbetaR_ran_trgr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_btg[[i]] <- summary(ran_btg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  sF_ran_btgr[[i]] <- summary(ran_btgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  sF_ran_trg[[i]] <- summary(ran_trg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  sF_ran_trgr[[i]] <- summary(ran_trgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  # extract beta and R
  sbetaR_ran_btg[[i]] <- summary(ran_btg_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_btg_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_btgr[[i]] <- summary(ran_btgr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_btgr_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_trg[[i]] <- summary(ran_trg_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_trg_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_trgr[[i]] <- summary(ran_trgr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_trgr_list[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t # 1.5 min

F_ran_btg <- do.call("rbind", sF_ran_btg)
F_ran_btgr <- do.call("rbind", sF_ran_btgr)
F_ran_trg <- do.call("rbind", sF_ran_trg)
F_ran_trgr <- do.call("rbind", sF_ran_trgr)

betaR_ran_btg <- do.call("rbind", sbetaR_ran_btg)
betaR_ran_btgr <- do.call("rbind", sbetaR_ran_btgr)
betaR_ran_trg <- do.call("rbind", sbetaR_ran_trg)
betaR_ran_trgr <- do.call("rbind", sbetaR_ran_trgr)


#     H3 - mixed sex EC -----
#       - run ran mods ----
ran_ecg_list <- vector("list", length = 1000)
ran_ecgr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecg_list[[i]] <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_ecgr_list[[i]] <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")

}
Sys.time() - t #17.6 min

#save(ran_ecg_list, ran_ecgr_list, file = "data/models gam - ran - H3 mixed sex grooming ec.Rdata")

#       - extract Fs, beta and R -----

load("data/models gam - ran - H3 mixed sex grooming ec.Rdata", verbose = T)

sF_ran_ecg <- vector("list", length = 1000)
sF_ran_ecgr <- vector("list", length = 1000)

sbetaR_ran_ecg <- vector("list", length = 1000)
sbetaR_ran_ecgr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecg[[i]] <- summary(ran_ecg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  sF_ran_ecgr[[i]] <- summary(ran_ecgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta and R
  sbetaR_ran_ecg[[i]] <- summary(ran_ecg_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_ecg_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_ecgr[[i]] <- summary(ran_ecgr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_ecgr_list[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t # 1 min

F_ran_ecg <- do.call("rbind", sF_ran_ecg)
F_ran_ecgr <- do.call("rbind", sF_ran_ecgr)

betaR_ran_ecg <- do.call("rbind", sbetaR_ran_ecg)
betaR_ran_ecgr <- do.call("rbind", sbetaR_ran_ecgr)


#     Saving ran Fs and coefs from each hypo -----

# Fs
# save(F_ran_dig, F_ran_digr, F_ran_dog, F_ran_dogr,
#      F_ran_sig, F_ran_sigr, F_ran_sog, F_ran_sogr,
#      F_ran_btg, F_ran_btgr, F_ran_trg, F_ran_trgr, F_ran_ecg, F_ran_ecgr, file = "data/ran Fs gams - directed and total grooming mixed sex.Rdata")
# # sex betas and Rs
# save(betaR_ran_dig, betaR_ran_digr, betaR_ran_dog, betaR_ran_dogr,
#      betaR_ran_sig, betaR_ran_sigr, betaR_ran_sog, betaR_ran_sogr,
#      betaR_ran_btg, betaR_ran_btgr, betaR_ran_trg, betaR_ran_trgr, betaR_ran_ecg, betaR_ran_ecgr, file = "data/ran beta n R gams - directed and total grooming mixed sex.Rdata")

# load("data/ran Fs gams - directed and total grooming mixed sex.Rdata", verbose = T)
# load("data/ran beta n R gams - directed and total grooming mixed sex.Rdata", verbose = T)

#   2. Evaluating sig grooming mixed - comparing observed to random --------

#random values
load("data/ran beta n R gams - directed and total grooming mixed sex.Rdata", verbose = T)
load("data/ran Fs gams - directed and total grooming mixed sex.Rdata", verbose = T)


#load observed
load("data/models gam - mixed sex grooming and total grooming with and without rank.Rdata", verbose = T)
source("functions/functions - test sig gamm.R")


a <- test_sig_gamm_mixed(response = "In-Degree", behavior = "Grooming", 
                         mod = dig, modr = digr, F_ran = F_ran_dig, F_ran_r = F_ran_digr,
                         beta_ran = betaR_ran_dig, beta_ran_r = betaR_ran_digr) #%>% rank_sep()

b <- test_sig_gamm_mixed(response = "Out-Degree", behavior = "Grooming",
                         mod = dog, modr = dogr, F_ran = F_ran_dog, F_ran_r = F_ran_dogr,
                         beta_ran = betaR_ran_dog, beta_ran_r = betaR_ran_dogr) 

c <- test_sig_gamm_mixed(response = "In-Strength", behavior = "Grooming",
                         mod = sig, modr = sigr, F_ran = F_ran_sig, F_ran_r = F_ran_sigr,
                         beta_ran = betaR_ran_sig, beta_ran_r = betaR_ran_sigr) 
d <- test_sig_gamm_mixed(response = "Out-Strength", behavior = "Grooming",
                         mod = sog, modr = sogr, F_ran = F_ran_sog, F_ran_r = F_ran_sogr,
                         beta_ran = betaR_ran_sog, beta_ran_r = betaR_ran_sogr) 

e <- test_sig_gamm_mixed(response = "Local Transitivity", behavior = "Grooming",
                         mod = trg, modr = trgr, F_ran = F_ran_trg, F_ran_r = F_ran_trgr,
                         beta_ran = betaR_ran_trg, beta_ran_r = betaR_ran_trgr)

f <- test_sig_gamm_mixed(response = "Betweenness", behavior = "Grooming", 
                         mod = btg, modr = btgr, F_ran = F_ran_btg, F_ran_r = F_ran_btgr,
                         beta_ran = betaR_ran_btg, beta_ran_r = betaR_ran_btgr)

g <- test_sig_gamm_mixed(response = "Eigenvector Centrality", behavior = "Grooming",
                         mod = ecg, modr = ecgr, F_ran = F_ran_ecg, F_ran_r = F_ran_ecgr,
                         beta_ran = betaR_ran_ecg, beta_ran_r = betaR_ran_ecgr)

#mixed sex tables
ms_gamH1g <- do.call("rbind", list(a,b,c,d))
ms_gamH2g <- do.call("rbind", list(e,f))
ms_gamH3g <- g

# write.table(ms_gamH1g, file = "results/tables/GAMs/H1. GAM mixed sex grooming in-deg out-deg in-strength out-strength.txt", quote = FALSE, row.names = FALSE, sep = "/")
# write.table(ms_gamH2g, file = "results/tables/GAMs/H2. GAM mixed sex grooming bt trans.txt", quote = FALSE, row.names = FALSE, sep = "/")
# write.table(ms_gamH3g, file = "results/tables/GAMs/H3. GAM mixed sex grooming ec.txt", quote = FALSE, row.names = FALSE, sep = "/")

# Tables of only models with rank:

ms_gamH1g_ro <- ms_gamH1g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))

ms_gamH2g_ro <- ms_gamH2g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))

ms_gamH3g_ro <- ms_gamH3g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))


ms_gamg_ro <- do.call("rbind", list(ms_gamH1g_ro, ms_gamH2g_ro, ms_gamH3g_ro))

# write.table(ms_gamg_ro, file = "results/tables/GAMs/H1-3. rank mods only - GAM mixed sex grooming all measures.txt", quote = FALSE, row.names = FALSE, sep = "/")

# Tables of only models without rank:

ms_gamH1g_ao <- ms_gamH1g %>%
  filter(!grepl("rank", pred))

ms_gamH2g_ao <- ms_gamH2g %>%
  filter(!grepl("rank", pred))

ms_gamH3g_ao <- ms_gamH3g %>%
  filter(!grepl("rank", pred))

ms_gamg_ao <- do.call("rbind", list(ms_gamH1g_ao, ms_gamH2g_ao, ms_gamH3g_ao))

# write.table(ms_gamg_ao, file = "results/tables/GAMs/H1-3. age only models - GAM mixed sex grooming all measures.txt", quote = FALSE, row.names = FALSE, sep = "/")

# ----
# ----
# C. Grooming same sex networks ------
#   3. Random F stat extraction -----
#     H1 - same sex Strength and In-Degree/out directed grooming --- mixed sex ----
#     - Degree & In-Strength  ----
#       - run ran mods ------
ran_fsdig_list <- vector("list", length = 1000)
ran_fsdigr_list <- vector("list", length = 1000)
ran_fssig_list <- vector("list", length = 1000)
ran_fssigr_list <- vector("list", length = 1000)

ran_msdig_list <- vector("list", length = 1000)
ran_msdigr_list <- vector("list", length = 1000)
ran_mssig_list <- vector("list", length = 1000)
ran_mssigr_list <- vector("list", length = 1000)


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
  
  ran_fsdig_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsdigr_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fssig_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fssigr_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")

  ran_msdig_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 15) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msdigr_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 15) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mssig_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mssigr_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")

}
Sys.time() - t # 43 min

# save(ran_fsdig_list, ran_fsdigr_list, ran_fssig_list, ran_fssigr_list,
#      ran_msdig_list, ran_msdigr_list, ran_mssig_list, ran_mssigr_list,
#      file = "data/models gam - ran - H1 same sex grooming random deg and strength in.Rdata")

# 7.2.21 resaved after gm code change

#       - extract Fs and R -------
load("data/models gam - ran - H1 same sex grooming random deg and strength in.Rdata", verbose = T)

sF_ran_fsdig <- vector("list", length = 1000)
sF_ran_fsdigr <- vector("list", length = 1000)
sF_ran_fssig <- vector("list", length = 1000)
sF_ran_fssigr <- vector("list", length = 1000)

sF_ran_msdig <- vector("list", length = 1000)
sF_ran_msdigr <- vector("list", length = 1000)
sF_ran_mssig <- vector("list", length = 1000)
sF_ran_mssigr <- vector("list", length = 1000)

sbetaR_ran_fsdig <- vector("list", length = 1000)
sbetaR_ran_fsdigr <- vector("list", length = 1000)
sbetaR_ran_fssig <- vector("list", length = 1000)
sbetaR_ran_fssigr <- vector("list", length = 1000)

sbetaR_ran_msdig <- vector("list", length = 1000)
sbetaR_ran_msdigr <- vector("list", length = 1000)
sbetaR_ran_mssig <- vector("list", length = 1000)
sbetaR_ran_mssigr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
#extract F stat like this
sF_ran_fsdig[[i]] <- summary(ran_fsdig_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_fsdigr[[i]] <- summary(ran_fsdigr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_fssig[[i]] <- summary(ran_fssig_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_fssigr[[i]] <- summary(ran_fssigr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_msdig[[i]] <- summary(ran_msdig_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_msdigr[[i]] <- summary(ran_msdigr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_mssig[[i]] <- summary(ran_mssig_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_mssigr[[i]] <- summary(ran_mssigr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

# extract betas and Rs

sbetaR_ran_fsdig[[i]] <- summary(ran_fsdig_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_fsdigr[[i]] <- summary(ran_fsdigr_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_fssig[[i]] <- summary(ran_fssig_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_fssigr[[i]] <- summary(ran_fssigr_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_msdig[[i]] <-  summary(ran_msdig_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_msdigr[[i]] <- summary(ran_msdigr_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_mssig[[i]] <- summary(ran_mssig_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_mssigr[[i]] <- summary(ran_mssigr_list[[i]])$r.sq %>% round(., 2)

}
Sys.time() - t # 1.4 min

F_ran_fsdig <- do.call("rbind", sF_ran_fsdig)
F_ran_fsdigr <- do.call("rbind", sF_ran_fsdigr)
F_ran_fssig <- do.call("rbind", sF_ran_fssig)
F_ran_fssigr <- do.call("rbind", sF_ran_fssigr)

F_ran_msdig <- do.call("rbind", sF_ran_msdig)
F_ran_msdigr <- do.call("rbind", sF_ran_msdigr)
F_ran_mssig <- do.call("rbind", sF_ran_mssig)
F_ran_mssigr <- do.call("rbind", sF_ran_mssigr)

betaR_ran_fsdig <- do.call("rbind", sbetaR_ran_fsdig)
betaR_ran_fsdigr <- do.call("rbind", sbetaR_ran_fsdigr)
betaR_ran_fssig <- do.call("rbind", sbetaR_ran_fssig)
betaR_ran_fssigr <- do.call("rbind", sbetaR_ran_fssigr)

betaR_ran_msdig <- do.call("rbind", sbetaR_ran_msdig)
betaR_ran_msdigr <- do.call("rbind", sbetaR_ran_msdigr)
betaR_ran_mssig <- do.call("rbind", sbetaR_ran_mssig)
betaR_ran_mssigr <- do.call("rbind", sbetaR_ran_mssigr)


#     - Degree & Out-Strength ---- 
#       - run ran mods ------
ran_fsdog_list <- vector("list", length = 1000)
ran_fsdogr_list <- vector("list", length = 1000)
ran_fssog_list <- vector("list", length = 1000)
ran_fssogr_list <- vector("list", length = 1000)

ran_msdog_list <- vector("list", length = 1000)
ran_msdogr_list <- vector("list", length = 1000)
ran_mssog_list <- vector("list", length = 1000)
ran_mssogr_list <- vector("list", length = 1000)


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
  
  ran_fsdog_list[[i]] <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsdogr_list[[i]] <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fssog_list[[i]] <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "F"), family = Gamma(link = "log"), method = "REML")
  ran_fssogr_list[[i]] <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "F"), family = Gamma(link = "log"), method = "REML")

  ran_msdog_list[[i]] <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msdogr_list[[i]] <- gam(deg_out ~ s(age_mid_year, k = 15) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mssog_list[[i]] <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mssogr_list[[i]] <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #21 min

# save(ran_fsdog_list, ran_fsdogr_list, ran_fssog_list, ran_fssogr_list,
#      ran_msdog_list, ran_msdogr_list, ran_mssog_list, ran_mssogr_list,
#      file = "data/models gam - ran - H1 same sex grooming random deg and strength out.Rdata")

#changed save name 5.31.21
# 7.2.21 resaved after gm code change

#       - extract Fs and R -------
load("data/models gam - ran - H1 same sex grooming random deg and Out-Strength.Rdata", verbose = T)

sF_ran_fsdog <- vector("list", length = 1000)
sF_ran_fsdogr <- vector("list", length = 1000)
sF_ran_fssog <- vector("list", length = 1000)
sF_ran_fssogr <- vector("list", length = 1000)

sF_ran_msdog <- vector("list", length = 1000)
sF_ran_msdogr <- vector("list", length = 1000)
sF_ran_mssog <- vector("list", length = 1000)
sF_ran_mssogr <- vector("list", length = 1000)

sbetaR_ran_fsdog <- vector("list", length = 1000)
sbetaR_ran_fsdogr <- vector("list", length = 1000)
sbetaR_ran_fssog <- vector("list", length = 1000)
sbetaR_ran_fssogr <- vector("list", length = 1000)

sbetaR_ran_msdog <- vector("list", length = 1000)
sbetaR_ran_msdogr <- vector("list", length = 1000)
sbetaR_ran_mssog <- vector("list", length = 1000)
sbetaR_ran_mssogr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_fsdog[[i]] <- summary(ran_fsdog_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsdogr[[i]] <- summary(ran_fsdogr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fssog[[i]] <- summary(ran_fssog_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fssogr[[i]] <- summary(ran_fssogr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msdog[[i]] <- summary(ran_msdog_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msdogr[[i]] <- summary(ran_msdogr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mssog[[i]] <- summary(ran_mssog_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mssogr[[i]] <- summary(ran_mssogr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  # extract Rs
  
  sbetaR_ran_fsdog[[i]] <- summary(ran_fsdog_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_fsdogr[[i]] <- summary(ran_fsdogr_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_fssog[[i]] <- summary(ran_fssog_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_fssogr[[i]] <- summary(ran_fssogr_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_msdog[[i]] <- summary(ran_msdog_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_msdogr[[i]] <- summary(ran_msdogr_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_mssog[[i]] <- summary(ran_mssog_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_mssogr[[i]] <- summary(ran_mssogr_list[[i]])$r.sq %>% round(., 2)
  
  }
Sys.time() - t # 1.5 min

F_ran_fsdog <- do.call("rbind", sF_ran_fsdog)
F_ran_fsdogr <- do.call("rbind", sF_ran_fsdogr)
F_ran_fssog <- do.call("rbind", sF_ran_fssog)
F_ran_fssogr <- do.call("rbind", sF_ran_fssogr)

F_ran_msdog <- do.call("rbind", sF_ran_msdog)
F_ran_msdogr <- do.call("rbind", sF_ran_msdogr)
F_ran_mssog <- do.call("rbind", sF_ran_mssog)
F_ran_mssogr <- do.call("rbind", sF_ran_mssogr)

betaR_ran_fsdog <- do.call("rbind", sbetaR_ran_fsdog)
betaR_ran_fsdogr <- do.call("rbind", sbetaR_ran_fsdogr)
betaR_ran_fssog <- do.call("rbind", sbetaR_ran_fssog)
betaR_ran_fssogr <- do.call("rbind", sbetaR_ran_fssogr)

betaR_ran_msdog <- do.call("rbind", sbetaR_ran_msdog)
betaR_ran_msdogr <- do.call("rbind", sbetaR_ran_msdogr)
betaR_ran_mssog <- do.call("rbind", sbetaR_ran_mssog)
betaR_ran_mssogr <- do.call("rbind", sbetaR_ran_mssogr)

#     H2 - same sex Betweenness and transitivity undirected grooming --- mixed sex ----
#       - run ran mods -----
ran_fsbtg_list <- vector("list", length = 1000)
ran_fsbtgr_list <- vector("list", length = 1000)
ran_fstrg_list <- vector("list", length = 1000)
ran_fstrgr_list <- vector("list", length = 1000)

ran_msbtg_list <- vector("list", length = 1000)
ran_msbtgr_list <- vector("list", length = 1000)
ran_mstrg_list <- vector("list", length = 1000)
ran_mstrgr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "total_grooming") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)
  
  ran_fsbtg_list[[i]] <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsbtgr_list[[i]] <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fstrg_list[[i]] <- gam(trans ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fstrgr_list[[i]] <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  
  ran_msbtg_list[[i]] <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msbtgr_list[[i]] <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mstrg_list[[i]] <- gam(trans ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mstrgr_list[[i]] <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")

}
Sys.time() - t # 55.8 min

# save(ran_fsbtg_list, ran_fsbtgr_list, ran_fstrg_list, ran_fstrgr_list, 
#      ran_msbtg_list, ran_msbtgr_list, ran_mstrg_list, ran_mstrgr_list, 
#      file = "data/models gam - ran - H2 same sex grooming bt and trans.Rdata")

#       - extract Fs and R -----
load("data/models gam - ran - H2 same sex grooming bt and trans.Rdata", verbose = T)

sF_ran_fsbtg <- vector("list", length = 1000)
sF_ran_fsbtgr <- vector("list", length = 1000)
sF_ran_fstrg <- vector("list", length = 1000)
sF_ran_fstrgr <- vector("list", length = 1000)

sF_ran_msbtg <- vector("list", length = 1000)
sF_ran_msbtgr <- vector("list", length = 1000)
sF_ran_mstrg <- vector("list", length = 1000)
sF_ran_mstrgr <- vector("list", length = 1000)

sbetaR_ran_fsbtg <- vector("list", length = 1000)
sbetaR_ran_fsbtgr <- vector("list", length = 1000)
sbetaR_ran_fstrg <- vector("list", length = 1000)
sbetaR_ran_fstrgr <- vector("list", length = 1000)

sbetaR_ran_msbtg <- vector("list", length = 1000)
sbetaR_ran_msbtgr <- vector("list", length = 1000)
sbetaR_ran_mstrg <- vector("list", length = 1000)
sbetaR_ran_mstrgr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
    
  
#extract F stat like this
sF_ran_fsbtg[[i]] <- summary(ran_fsbtg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_fsbtgr[[i]] <- summary(ran_fsbtgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_fstrg[[i]] <- summary(ran_fstrg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_fstrgr[[i]] <- summary(ran_fstrgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_msbtg[[i]] <- summary(ran_msbtg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_msbtgr[[i]] <- summary(ran_msbtgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_mstrg[[i]] <- summary(ran_mstrg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

sF_ran_mstrgr[[i]] <- summary(ran_mstrgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
# extract betas and Rs

sbetaR_ran_fsbtg[[i]] <- summary(ran_fsbtg_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_fsbtgr[[i]] <- summary(ran_fsbtgr_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_fstrg[[i]] <- summary(ran_fstrg_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_fstrgr[[i]] <- summary(ran_fstrgr_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_msbtg[[i]] <- summary(ran_msbtg_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_msbtgr[[i]] <- summary(ran_msbtgr_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_mstrg[[i]] <- summary(ran_mstrg_list[[i]])$r.sq %>% round(., 2)

sbetaR_ran_mstrgr[[i]] <- summary(ran_mstrgr_list[[i]])$r.sq %>% round(., 2)

}
t - Sys.time() #
  
F_ran_fsbtg <- do.call("rbind", sF_ran_fsbtg)
F_ran_fsbtgr <- do.call("rbind", sF_ran_fsbtgr)
F_ran_fstrg <- do.call("rbind", sF_ran_fstrg)
F_ran_fstrgr <- do.call("rbind", sF_ran_fstrgr)

F_ran_msbtg <- do.call("rbind", sF_ran_msbtg)
F_ran_msbtgr <- do.call("rbind", sF_ran_msbtgr)
F_ran_mstrg <- do.call("rbind", sF_ran_mstrg)
F_ran_mstrgr <- do.call("rbind", sF_ran_mstrgr)

betaR_ran_fsbtg <- do.call("rbind", sbetaR_ran_fsbtg)
betaR_ran_fsbtgr <- do.call("rbind", sbetaR_ran_fsbtgr)
betaR_ran_fstrg <- do.call("rbind", sbetaR_ran_fstrg)
betaR_ran_fstrgr <- do.call("rbind", sbetaR_ran_fstrgr)

betaR_ran_msbtg <- do.call("rbind", sbetaR_ran_msbtg)
betaR_ran_msbtgr <- do.call("rbind", sbetaR_ran_msbtgr)
betaR_ran_mstrg <- do.call("rbind", sbetaR_ran_mstrg)
betaR_ran_mstrgr <- do.call("rbind", sbetaR_ran_mstrgr)


#     H3 - same sex EC ----- 
#       - run ran mods -----
ran_fsecg_list <- vector("list", length = 1000)
ran_fsecgr_list <- vector("list", length = 1000)
ran_msecg_list <- vector("list", length = 1000)
ran_msecgr_list <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "total_grooming") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_fsecg_list[[i]] <- gam(ec ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsecgr_list[[i]] <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_msecg_list[[i]] <- gam(ec ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msecgr_list[[i]] <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #  8.6 min


#save(ran_fsecg_list, ran_fsecgr_list, ran_msecg_list, ran_msecgr_list, file = "data/models gam - ran - H3 same sex grooming ec.Rdata")

#       - extract Fs and R -----
load("data/models gam - ran - H3 same sex grooming ec.Rdata", verbose = T)

sF_ran_fsecg <- vector("list", length = 1000)
sF_ran_fsecgr <- vector("list", length = 1000)
sF_ran_msecg <- vector("list", length = 1000)
sF_ran_msecgr <- vector("list", length = 1000)

sbetaR_ran_fsecg <- vector("list", length = 1000)
sbetaR_ran_fsecgr <- vector("list", length = 1000)
sbetaR_ran_msecg <- vector("list", length = 1000)
sbetaR_ran_msecgr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_fsecg[[i]] <- summary(ran_fsecg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsecgr[[i]] <- summary(ran_fsecgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msecg[[i]] <- summary(ran_msecg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msecgr[[i]] <- summary(ran_msecgr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract R
  sbetaR_ran_fsecg[[i]] <- summary(ran_fsecg_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_fsecgr[[i]] <- summary(ran_fsecgr_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_msecg[[i]] <- summary(ran_msecg_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_msecgr[[i]] <- summary(ran_msecgr_list[[i]])$r.sq %>% round(., 2)
  
}
Sys.time() - t # 40 sec

F_ran_fsecg <- do.call("rbind", sF_ran_fsecg)
F_ran_fsecgr <- do.call("rbind", sF_ran_fsecgr)
F_ran_msecg <- do.call("rbind", sF_ran_msecg)
F_ran_msecgr <- do.call("rbind", sF_ran_msecgr)

betaR_ran_fsecg <- do.call("rbind", sbetaR_ran_fsecg)
betaR_ran_fsecgr <- do.call("rbind", sbetaR_ran_fsecgr)
betaR_ran_msecg <- do.call("rbind", sbetaR_ran_msecg)
betaR_ran_msecgr <- do.call("rbind", sbetaR_ran_msecgr)

#     Saving same sex ran Fs from each hypo -----


# save(F_ran_fsdig, F_ran_fsdigr, F_ran_fsdog, F_ran_fsdogr,
#      F_ran_msdig, F_ran_msdigr, F_ran_msdog, F_ran_msdogr,
#      F_ran_fssig, F_ran_fssigr, F_ran_fssog, F_ran_fssogr,
#      F_ran_mssig, F_ran_mssigr, F_ran_mssog, F_ran_mssogr,
#      F_ran_fsbtg, F_ran_fsbtgr, F_ran_fstrg, F_ran_fstrgr,
#      F_ran_msbtg, F_ran_msbtgr, F_ran_mstrg, F_ran_mstrgr,
#      F_ran_fsecg, F_ran_fsecgr, F_ran_msecg, F_ran_msecgr, file = "data/ran Fs gams - directed and total grooming same sex.Rdata")
# save(betaR_ran_fsdig, betaR_ran_fsdigr, betaR_ran_fsdog, betaR_ran_fsdogr,
#      betaR_ran_msdig, betaR_ran_msdigr, betaR_ran_msdog, betaR_ran_msdogr,
#      betaR_ran_fssig, betaR_ran_fssigr, betaR_ran_fssog, betaR_ran_fssogr,
#      betaR_ran_mssig, betaR_ran_mssigr, betaR_ran_mssog, betaR_ran_mssogr,
#      betaR_ran_fsbtg, betaR_ran_fsbtgr, betaR_ran_fstrg, betaR_ran_fstrgr,
#      betaR_ran_msbtg, betaR_ran_msbtgr, betaR_ran_mstrg, betaR_ran_mstrgr,
#      betaR_ran_fsecg, betaR_ran_fsecgr, betaR_ran_msecg, betaR_ran_msecgr, file = "data/ran beta n R gams - directed and total grooming same sex.Rdata")
# 
# resaved 5/31/21 after resaving "data/models gam - ran - H3 same sex grooming ec.Rdata", bc written over
# previously w prox data
# resave 7/2/21 after recode gm


#   4. Evaluating sig grooming same - comparing observed to random --------

#random values
load("data/ran beta n R gams - directed and total grooming same sex.Rdata", verbose = T)
load("data/ran Fs gams - directed and total grooming same sex.Rdata", verbose = T)

#observed values
load("data/models gam - same sex grooming and total grooming with and without rank.Rdata", verbose = T)
source("functions/functions - test sig gamm.R") # add sex of network and behavior to sig table function, also create new funcction for same sex nets

a <- test_sig_gamm_same(response = "In-Degree", behavior = "Grooming", net_sex = "same",
                   fmod = f_dig, fmodr = f_digr, mmod = m_dig, mmodr = m_digr,
                   F_ranf = F_ran_fsdig, F_ranf_r = F_ran_fsdigr, 
                   F_ranm = F_ran_msdig, F_ranm_r = F_ran_msdigr)

b <- test_sig_gamm_same(response = "Out-Degree", behavior = "Grooming", net_sex = "same",
                   fmod = f_dog, fmodr = f_digr, mmod = m_dog, mmodr = m_dogr,
                   F_ranf = F_ran_fsdog, F_ranf_r = F_ran_fsdogr, 
                   F_ranm = F_ran_msdog, F_ranm_r = F_ran_msdogr)

c <- test_sig_gamm_same(response = "In-Strength", behavior = "Grooming", net_sex = "same",
                        fmod = f_sig, fmodr = f_sigr, mmod = m_sig, mmodr = m_sigr,
                        F_ranf = F_ran_fssig, F_ranf_r = F_ran_fssigr, 
                        F_ranm = F_ran_mssig, F_ranm_r = F_ran_mssigr)

d <- test_sig_gamm_same(response = "Out-Strength", behavior = "Grooming", net_sex = "same",
                        fmod = f_sog, fmodr = f_sigr, mmod = m_sog, mmodr = m_sogr,
                        F_ranf = F_ran_fssog, F_ranf_r = F_ran_fssogr, 
                        F_ranm = F_ran_mssog, F_ranm_r = F_ran_mssogr)

e <- test_sig_gamm_same(response = "Local Transitivity", behavior = "Grooming", net_sex = "same",
                        fmod = f_trg, fmodr = f_trgr, mmod = m_trg, mmodr = m_trgr,
                        F_ranf = F_ran_fstrg, F_ranf_r = F_ran_fstrgr, 
                        F_ranm = F_ran_mstrg, F_ranm_r = F_ran_mstrgr)

f <- test_sig_gamm_same(response = "Betweenness", behavior = "Grooming", net_sex = "same",
                   fmod = f_btg, fmodr = f_btgr, mmod = m_dog, mmodr = m_btgr,
                   F_ranf = F_ran_fsbtg, F_ranf_r = F_ran_fsbtgr, 
                   F_ranm = F_ran_msbtg, F_ranm_r = F_ran_msbtgr)

g <- test_sig_gamm_same(response = "Eigenvector Centrality", behavior = "Grooming", net_sex = "same",
                   fmod = f_ecg, fmodr = f_ecgr, mmod = m_ecg, mmodr = m_ecgr,
                   F_ranf = F_ran_fsecg, F_ranf_r = F_ran_fsecgr, 
                   F_ranm = F_ran_msecg, F_ranm_r = F_ran_msecgr)


#same sex tables
ss_gamH1g <- do.call("rbind", list(a,b,c,d))
ss_gamH2g <- do.call("rbind", list(e, f))
ss_gamH3g <- g                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    


# write.table(ss_gamH1g, file = "results/tables/GAMs/H1. GAM same sex grooming in-deg out-deg in-strength out-strength.txt", quote = F, row.names = FALSE, sep = "/")
# write.table(ss_gamH2g, file = "results/tables/GAMs/H2. GAM same sex grooming bt trans.txt", quote = F, row.names = FALSE, sep = "/")
# write.table(ss_gamH3g, file = "results/tables/GAMs/H3. GAM same sex grooming ec.txt", quote = F, row.names = FALSE, sep = "/")

#tables only of models with rank

ss_gamH1g_ro <- ss_gamH1g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))

ss_gamH2g_ro <- ss_gamH2g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))

ss_gamH3g_ro <- ss_gamH3g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))

ss_gamg_ro <- do.call("rbind", list(ss_gamH1g_ro, ss_gamH2g_ro, ss_gamH3g_ro))

#write.table(ss_gamg_ro, file = "results/tables/GAMs/H1-3. rank mods only - GAM same sex grooming all measures.txt", quote = F, row.names = FALSE, sep = "/")


#tables only of models with rank

ss_gamH1g_ao <- ss_gamH1g %>%
  filter(!grepl("rank", pred)) 

ss_gamH2g_ao <- ss_gamH2g %>%
  filter(!grepl("rank", pred)) 

ss_gamH3g_ao <- ss_gamH3g %>%
  filter(!grepl("rank", pred))

ss_gamg_ao <- do.call("rbind", list(ss_gamH1g_ao, ss_gamH2g_ao, ss_gamH3g_ao))

# write.table(ss_gamg_ao, file = "results/tables/GAMs/H1-3. age only models - GAM same sex grooming all measures.txt", quote = F, row.names = FALSE, sep = "/")


###################################################################################### ####
# within sex permutations of age and rank for mixed sex networks ----
#################################################################################### ####
# A. -- load node-randomized datasets ----

# undir w
load("data/ran12 - both sex w - WSP+ node (age rank prop_cyc chimp_id) + sex randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)
# dir uw
load("data/ran13 - both sex w - WSP+ node (age rank prop_cyc chimp_id) + sex randomized sna measures directed gm unweighted.Rdata", verbose = T)
# dir gm w
load("data/ran14 - both sex w - WSP+ node (age rank prop_cyc chimp_id) + sex randomized sna measures directed gm weighted.Rdata", verbose = T)

# B. Grooming mixed sex networks --------
#   1. Random F stat extraction -----
#     H1 - mixed sex Strength and In-Degree/out directed grooming --- mixed sex ----
#     - Degree & In-Strength - storage of Fs from randomized data models ----
#       - run ran mods -----

ran_dig_list_wsp <- vector("list", length = 1000)
ran_digr_list_wsp <- vector("list", length = 1000)
ran_sig_list_wsp <- vector("list", length = 1000)
ran_sigr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data_w <- list_ran_dir_sna_measure_both_sex_w_wspp[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_both_sex_uw_wspp[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_dig_list_wsp[[i]] <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_digr_list_wsp[[i]] <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_sig_list_wsp[[i]] <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  ran_sigr_list_wsp[[i]] <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #34 min

# save(ran_dig_list_wsp, ran_digr_list_wsp, ran_sig_list_wsp ran_sigr_list_wsp, file = "data/models gam - ran - H1 mixed sex grooming random deg and In-Strength WITHIN SEX PERM.Rdata") 


#       - extract Fs, beta and R -----
load("data/models gam - ran - H1 mixed sex grooming random deg and In-Strength WITHIN SEX PERM.Rdata", verbose = T)

sF_ran_dig_wsp <- vector("list", length = 1000)
sF_ran_digr_wsp <- vector("list", length = 1000)
sF_ran_sig_wsp <- vector("list", length = 1000)
sF_ran_sigr_wsp <- vector("list", length = 1000)

sbetaR_ran_dig_wsp <- vector("list", length = 1000)
sbetaR_ran_digr_wsp <- vector("list", length = 1000)
sbetaR_ran_sig_wsp<- vector("list", length = 1000)
sbetaR_ran_sigr_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat
  sF_ran_dig_wsp[[i]] <- summary(ran_dig_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_digr_wsp[[i]] <- summary(ran_digr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sig_wsp[[i]] <- summary(ran_sig_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sigr_wsp[[i]] <- summary(ran_sigr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta 
  sbetaR_ran_dig_wsp[[i]] <- summary(ran_dig_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_dig_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_digr_wsp[[i]] <- summary(ran_digr_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_digr_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_sig_wsp[[i]] <- summary(ran_sig_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sig_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_sigr_wsp[[i]] <- summary(ran_sigr_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sigr_list_wsp[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t #

F_ran_dig_wsp <- do.call("rbind", sF_ran_dig_wsp)
F_ran_digr_wsp <- do.call("rbind", sF_ran_digr_wsp)
F_ran_sig_wsp <- do.call("rbind", sF_ran_sig_wsp)
F_ran_sigr_wsp <- do.call("rbind", sF_ran_sigr_wsp)

betaR_ran_dig_wsp <- do.call("rbind", sbetaR_ran_dig_wsp)
betaR_ran_digr_wsp <- do.call("rbind", sbetaR_ran_digr_wsp)
betaR_ran_sig_wsp <- do.call("rbind", sbetaR_ran_sig_wsp)
betaR_ran_sigr_wsp <- do.call("rbind", sbetaR_ran_sigr_wsp)

#     - Strength and Out-Degree ------
#       - run ran mods -----

ran_dog_list_wsp <- vector("list", length = 1000)
ran_dogr_list_wsp <- vector("list", length = 1000)
ran_sog_list_wsp <- vector("list", length = 1000)
ran_sogr_list_wsp <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data_w <- list_ran_dir_sna_measure_both_sex_w_wspp[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_both_sex_uw_wspp[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_dog_list_wsp[[i]] <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_dogr_list_wsp[[i]] <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_sog_list_wsp[[i]] <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  ran_sogr_list_wsp[[i]] <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 33 min

#save(ran_dog_list_wsp, ran_dogr_list_wsp, ran_sog_list_wsp, ran_sogr_list_wsp, file = "data/models gam - ran - H1 mixed sex grooming random deg and Out-Strength WITHIN SEX PERM.Rdata")


#       - extract Fs, beta and R -----
load("data/models gam - ran - H1 mixed sex grooming random deg and Out-Strength WITHIN SEX PERM.Rdata", verbose = T)

sF_ran_dog_wsp <- vector("list", length = 1000)
sF_ran_dogr_wsp <- vector("list", length = 1000)
sF_ran_sog_wsp <- vector("list", length = 1000)
sF_ran_sogr_wsp <- vector("list", length = 1000)

sbetaR_ran_dog_wsp <- vector("list", length = 1000)
sbetaR_ran_dogr_wsp <- vector("list", length = 1000)
sbetaR_ran_sog_wsp <- vector("list", length = 1000)
sbetaR_ran_sogr_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_dog_wsp[[i]] <- summary(ran_dog_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_dogr_wsp[[i]] <- summary(ran_dogr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sog_wsp[[i]] <- summary(ran_sog_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sogr_wsp[[i]] <- summary(ran_sogr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta and R
  sbetaR_ran_dog_wsp[[i]] <- summary(ran_dog_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_dog_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_dogr_wsp[[i]] <- summary(ran_dogr_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_dogr_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_sog_wsp[[i]] <- summary(ran_sog_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sog_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_sogr_wsp[[i]] <- summary(ran_sogr_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sogr_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  
}
Sys.time() - t # 1.5 min

F_ran_dog_wsp <- do.call("rbind", sF_ran_dog_wsp)
F_ran_dogr_wsp <- do.call("rbind", sF_ran_dogr_wsp)
F_ran_sog_wsp <- do.call("rbind", sF_ran_sog_wsp)
F_ran_sogr_wsp <- do.call("rbind", sF_ran_sogr_wsp)

betaR_ran_dog_wsp <- do.call("rbind", sbetaR_ran_dog_wsp)
betaR_ran_dogr_wsp <- do.call("rbind", sbetaR_ran_dogr_wsp)
betaR_ran_sog_wsp <- do.call("rbind", sbetaR_ran_sog_wsp)
betaR_ran_sogr_wsp <- do.call("rbind", sbetaR_ran_sogr_wsp)


#     H2 - mixed sex Betweenness and transitivity undirected grooming --- mixed sex ----
#       - run ran mods -----
ran_btg_list_wsp <- vector("list", length = 1000)
ran_btgr_list_wsp <- vector("list", length = 1000)
ran_trg_list_wsp <- vector("list", length = 1000)
ran_trgr_list_wsp <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_both_sex_w_wspp[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)
  
  ran_btg_list_wsp[[i]] <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_btgr_list_wsp[[i]] <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_trg_list_wsp[[i]] <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_trgr_list_wsp[[i]] <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 35 min

#save(ran_btg_list_wsp, ran_btgr_list_wsp, ran_trg_list_wsp, ran_trgr_list_wsp, file = "data/models gam - ran - H2 mixed sex grooming bt and trans WITHIN SEX PERM.Rdata")

#       - extract Fs, beta and R ------
load("data/models gam - ran - H2 mixed sex grooming bt and trans WITHIN SEX PERM.Rdata", verbose = T)

sF_ran_btg_wsp <- vector("list", length = 1000)
sF_ran_btgr_wsp <- vector("list", length = 1000)
sF_ran_trg_wsp <- vector("list", length = 1000)
sF_ran_trgr_wsp <- vector("list", length = 1000)

sbetaR_ran_btg_wsp <- vector("list", length = 1000)
sbetaR_ran_btgr_wsp <- vector("list", length = 1000)
sbetaR_ran_trg_wsp <- vector("list", length = 1000)
sbetaR_ran_trgr_wsp <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_btg_wsp[[i]] <- summary(ran_btg_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_btgr_wsp[[i]] <- summary(ran_btgr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_trg_wsp[[i]] <- summary(ran_trg_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_trgr_wsp[[i]] <- summary(ran_trgr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta and R
  sbetaR_ran_btg_wsp[[i]] <- summary(ran_btg_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_btg_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_btgr_wsp[[i]] <- summary(ran_btgr_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_btgr_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_trg_wsp[[i]] <- summary(ran_trg_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_trg_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_trgr_wsp[[i]] <- summary(ran_trgr_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_trgr_list_wsp[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t # 1.5 min

F_ran_btg_wsp <- do.call("rbind", sF_ran_btg_wsp)
F_ran_btgr_wsp <- do.call("rbind", sF_ran_btgr_wsp)
F_ran_trg_wsp <- do.call("rbind", sF_ran_trg_wsp)
F_ran_trgr_wsp <- do.call("rbind", sF_ran_trgr_wsp)

betaR_ran_btg_wsp <- do.call("rbind", sbetaR_ran_btg_wsp)
betaR_ran_btgr_wsp <- do.call("rbind", sbetaR_ran_btgr_wsp)
betaR_ran_trg_wsp <- do.call("rbind", sbetaR_ran_trg_wsp)
betaR_ran_trgr_wsp <- do.call("rbind", sbetaR_ran_trgr_wsp)


#     H3 - mixed sex EC -----
#       - run ran mods ----
ran_ecg_list_wsp <- vector("list", length = 1000)
ran_ecgr_list_wsp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_both_sex_w_wspp[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecg_list_wsp[[i]] <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_ecgr_list_wsp[[i]] <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 19.5 min

#save(ran_ecg_list_wsp, ran_ecgr_list_wsp, file = "data/models gam - ran - H3 mixed sex grooming ec WITHIN SEX PERM.Rdata")

#       - extract Fs, beta and R -----

load("data/models gam - ran - H3 mixed sex grooming ec WITHIN SEX PERM.Rdata", verbose = T)

sF_ran_ecg_wsp <- vector("list", length = 1000)
sF_ran_ecgr_wsp <- vector("list", length = 1000)

sbetaR_ran_ecg_wsp <- vector("list", length = 1000)
sbetaR_ran_ecgr_wsp <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecg_wsp[[i]] <- summary(ran_ecg_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_ecgr_wsp[[i]] <- summary(ran_ecgr_list_wsp[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta and R
  sbetaR_ran_ecg_wsp[[i]] <- summary(ran_ecg_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_ecg_list_wsp[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_ecgr_wsp[[i]] <- summary(ran_ecgr_list_wsp[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_ecgr_list_wsp[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t # 1 min

F_ran_ecg_wsp <- do.call("rbind", sF_ran_ecg_wsp)
F_ran_ecgr_wsp <- do.call("rbind", sF_ran_ecgr_wsp)

betaR_ran_ecg_wsp <- do.call("rbind", sbetaR_ran_ecg_wsp)
betaR_ran_ecgr_wsp <- do.call("rbind", sbetaR_ran_ecgr_wsp)


#     Saving ran Fs and coefs from each hypo -----

# #Fs
# save(F_ran_dig_wsp, F_ran_digr_wsp, F_ran_dog_wsp, F_ran_dogr_wsp,
#      F_ran_sig_wsp, F_ran_sigr_wsp, F_ran_sog_wsp, F_ran_sogr_wsp,
#      F_ran_btg_wsp, F_ran_btgr_wsp, F_ran_trg_wsp, F_ran_trgr_wsp, F_ran_ecg_wsp, F_ran_ecgr_wsp, file = "data/ran Fs gams - directed and total grooming mixed sex WITHIN SEX PERM.Rdata")
# 
# # sex betas and Rs
# save(betaR_ran_dig_wsp, betaR_ran_digr_wsp, betaR_ran_dog_wsp, betaR_ran_dogr_wsp,
#      betaR_ran_sig_wsp, betaR_ran_sigr_wsp, betaR_ran_sog_wsp, betaR_ran_sogr_wsp,
#      betaR_ran_btg_wsp, betaR_ran_btgr_wsp, betaR_ran_trg_wsp, betaR_ran_trgr_wsp, betaR_ran_ecg_wsp, betaR_ran_ecgr_wsp, file = "data/ran beta n R gams - directed and total grooming mixed sex WITHIN SEX PERM.Rdata")

# load("data/ran Fs gams - directed and total grooming mixed sex WITHIN SEX PERM.Rdata", verbose = T)
# load("data/ran beta n R gams - directed and total grooming mixed sex WITHIN SEX PERM.Rdata", verbose = T)

#   2. Evaluating sig grooming mixed - comparing observed to random --------

#random values
load("data/ran beta n R gams - directed and total grooming mixed sex WITHIN SEX PERM.Rdata", verbose = T)
load("data/ran Fs gams - directed and total grooming mixed sex WITHIN SEX PERM.Rdata", verbose = T)


#load observed
load("data/models gam - mixed sex grooming and total grooming with and without rank.Rdata", verbose = T)
source("functions/functions - test sig gamm.R")


a <- test_sig_gamm_mixed(response = "In-Degree", behavior = "Grooming", 
                         mod = dig, modr = digr, F_ran = F_ran_dig_wsp, F_ran_r = F_ran_digr_wsp,
                         beta_ran = betaR_ran_dig_wsp, beta_ran_r = betaR_ran_digr_wsp) #%>% rank_sep()

b <- test_sig_gamm_mixed(response = "Out-Degree", behavior = "Grooming",
                         mod = dog, modr = dogr, F_ran = F_ran_dog_wsp, F_ran_r = F_ran_dogr_wsp,
                         beta_ran = betaR_ran_dog_wsp, beta_ran_r = betaR_ran_dogr_wsp) 

c <- test_sig_gamm_mixed(response = "In-Strength", behavior = "Grooming",
                         mod = sig, modr = sigr, F_ran = F_ran_sig_wsp, F_ran_r = F_ran_sigr_wsp,
                         beta_ran = betaR_ran_sig_wsp, beta_ran_r = betaR_ran_sigr_wsp)

d <- test_sig_gamm_mixed(response = "Out-Strength", behavior = "Grooming",
                         mod = sog, modr = sogr, F_ran = F_ran_sog_wsp, F_ran_r = F_ran_sogr_wsp,
                         beta_ran = betaR_ran_sog_wsp, beta_ran_r = betaR_ran_sogr_wsp) 

e <- test_sig_gamm_mixed(response = "Local Transitivity", behavior = "Grooming",
                         mod = trg, modr = trgr, F_ran = F_ran_trg_wsp, F_ran_r = F_ran_trgr_wsp,
                         beta_ran = betaR_ran_trg_wsp, beta_ran_r = betaR_ran_trgr_wsp)

f <- test_sig_gamm_mixed(response = "Betweenness", behavior = "Grooming", 
                         mod = btg, modr = btgr, F_ran = F_ran_btg_wsp, F_ran_r = F_ran_btgr_wsp,
                         beta_ran = betaR_ran_btg_wsp, beta_ran_r = betaR_ran_btgr_wsp)

g <- test_sig_gamm_mixed(response = "Eigenvector Centrality", behavior = "Grooming",
                         mod = ecg, modr = ecgr, F_ran = F_ran_ecg_wsp, F_ran_r = F_ran_ecgr_wsp,
                         beta_ran = betaR_ran_ecg_wsp, beta_ran_r = betaR_ran_ecgr_wsp)

#mixed sex tables
ms_gamH1g <- do.call("rbind", list(a,b,c,d))
ms_gamH2g <- do.call("rbind", list(e,f))
ms_gamH3g <- g

# write.table(ms_gamH1g, file = "results/tables/GAMs/H1. GAM mixed sex grooming in-deg out-deg in-strength out-strength WITHIN SEX PERMUTATION.txt", quote = FALSE, row.names = FALSE, sep = "/")
# write.table(ms_gamH2g, file = "results/tables/GAMs/H2. GAM mixed sex grooming bt trans WITHIN SEX PERMUTATION.txt", quote = FALSE, row.names = FALSE, sep = "/")
# write.table(ms_gamH3g, file = "results/tables/GAMs/H3. GAM mixed sex grooming ec WITHIN SEX PERMUTATION.txt", quote = FALSE, row.names = FALSE, sep = "/")

# Tables of only models with rank:

ms_gamH1g_ro <- ms_gamH1g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))

ms_gamH2g_ro <- ms_gamH2g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))

ms_gamH3g_ro <- ms_gamH3g %>%
  filter(grepl("rank", pred)) %>%
  mutate(pred = case_when(
    str_detect(pred, "(with rank)") ~ str_remove(pred, " \\(with rank\\)"),
    TRUE ~ pred
  ))


ms_gamg_ro <- do.call("rbind", list(ms_gamH1g_ro, ms_gamH2g_ro, ms_gamH3g_ro))

#write.table(ms_gamg_ro, file = "results/tables/GAMs/H1-3. rank mods only - GAM mixed sex grooming all measures WITHIN SEX PERMUTATION.txt", quote = FALSE, row.names = FALSE, sep = "/")

# Tables of only models without rank:

ms_gamH1g_ao <- ms_gamH1g %>%
  filter(!grepl("rank", pred))

ms_gamH2g_ao <- ms_gamH2g %>%
  filter(!grepl("rank", pred))

ms_gamH3g_ao <- ms_gamH3g %>%
  filter(!grepl("rank", pred))

ms_gamg_ao <- do.call("rbind", list(ms_gamH1g_ao, ms_gamH2g_ao, ms_gamH3g_ao))

# write.table(ms_gamg_ao, file = "results/tables/GAMs/H1-3. age only models - GAM mixed sex grooming all measures WITHIN SEX PERMUTATION.txt", quote = FALSE, row.names = FALSE, sep = "/")

# gyard ---------------

# see if gam does differently than glmms when assessing sig for isolated male age and In-Degree ----

xo <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML") %>%
  summary() %>% 
  .$s.table %>% data.frame() %>% pull(F) 
xo

test <- vector("list", 1000)

for(i in 1:1000){

  data_uw <- list_ran_dir_sna_measure_both_sex_uw[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_digr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
  #extract F stat like this
  test[[i]] <- summary(ran_digr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

}

test <- do.call("rbind", test)

xr <- test %>% filter(rowname == "s(age_mid_year)") %>% pull(F)

sum(xo[1] > xr)/1000


F_ran_dig %>%
  count( `F`==0)
F_ran_digr %>%
  count( `F`==0)
F_ran_digr == 0
count( `F`==0)

sum(F_ran_dig$F == 0)
sum(F_ran_sig$F == 0)


F_ran_ecgr %>% filter(rowname == "s(age_mid_year):sexM") %>% pull(F) %>% hist()

