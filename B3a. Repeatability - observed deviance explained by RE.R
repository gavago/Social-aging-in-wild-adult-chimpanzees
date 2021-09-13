library(tidyverse)
library(mgcv)
# library(performance) #
source("data/data sets for gams.R")
source("functions/functions - dev expl re and test sig dev expl re.R")

# Deviance explained in gam: calculates (dev NULL - dev RESIDUAL ) / dev NULL 
# to essentially see how much of deviance in null model (no predictors or random effects) is explained by the 
# fitted model (if all dev null is explained by dev residual, then dev residual = 0 and dev explained  = 1) 
# if none of dev null is explained by model then dev residual = dev null, and dev explained = 0.

#deviance explained by each gam component: https://r.789695.n4.nabble.com/variance-explained-by-each-term-in-a-GAM-td836513.html


# vignette of function to calculate deviance explained by random effect alone ------
dev_expl_re <- function(mod_re, mod_no_re) summary(mod_re)$dev.expl - summary(mod_no_re)$dev.expl

#example of full method, calculating
# null dev
dig0 <- gam(deg_in ~ 1, data = mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
# fully specified model deviance
digr <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
# no d
digr_no_re <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5), data = mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

# proportion of total deviance explained by random effect alone =
(deviance(digr_no_re) - deviance(digr)) / deviance(dig0)
# which equals difference in dev expl between model w and without RE
dev_expl_re(digr, digr_no_re)



# 1. Gams without random effects (REs) -----
#  Grooming ----
# - mixed ----

# --- male -----
digr_no_re_mm <- gam(deg_in ~ s(age_mid_year, k = 15) + s(avg_rank, k = 5), data = mixed_dir_sna_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
dogr_no_re_mm <- gam(deg_out ~ s(age_mid_year, k = 15) + s(avg_rank, k = 5), data = mixed_dir_sna_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
sigr_no_re_mm <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mixed_dir_sna_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
sogr_no_re_mm <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mixed_dir_sna_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
btgr_no_re_mm <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mixed_sna_w %>% filter(sex == "M" & behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
trgr_no_re_mm <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mixed_sna_w %>% filter(sex == "M" & behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ecgr_no_re_mm <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  , data = mixed_sna_w %>% filter(sex == "M" & behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

# --- female ----

ii_digr_f_estr_no_re <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
ii_dogr_f_estr_no_re <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
ii_sigr_f_estr_no_re <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")
ii_sogr_f_estr_no_re <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")
ii_trgr_f_estr_no_re <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ii_btgr_f_estr_no_re <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ii_ecgr_f_estr_no_re <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

# - same ----
f_digr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = f_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
m_digr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = m_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
f_dogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = f_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
m_dogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = m_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

f_sigr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = f_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")
m_sigr_no_re <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = m_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")
f_sogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = f_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")
m_sogr_no_re <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = m_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")

m_btgr_no_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  , data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
f_btgr_no_re <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  , data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
m_trgr_no_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  , data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
f_trgr_no_re <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  , data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
m_ecgr_no_re <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  , data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
f_ecgr_no_re <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  , data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")


# Prox  ----
# - mixed ----
# --- male ----
spr_no_re_mm <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mixed_sna_w %>% filter(sex == "M" & behavior == "prox"), family = Gamma(link = "log"), method = "REML")
btpr_no_re_mm  <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mixed_sna_w %>% filter(sex == "M" & behavior == "prox"), family = gaussian(link = "log"), method = "REML")
trpr_no_re_mm  <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mixed_sna_w %>% filter(sex == "M" & behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ecpr_no_re_mm <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = mixed_sna_w %>% filter(sex == "M" & behavior == "prox"), family = gaussian(link = "log"), method = "REML")

# --- female ----
ii_spr_f_estr_no_re <- gam(deg ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ii_trpr_f_estr_no_re <- gam(trans ~ s(age_mid_year, k = 3) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 3), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ii_btpr_f_estr_no_re <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_sna_w  %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ii_ecpr_f_estr_no_re <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

# - same -----
m_spr_no_re  <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) , data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
f_spr_no_re  <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) , data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
m_btpr_no_re  <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) , data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
f_btpr_no_re  <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) , data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
m_trpr_no_re  <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
f_trpr_no_re  <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
m_ecpr_no_re  <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
f_ecpr_no_re  <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

# Save all no re gams ------

# #male mixed sex
# save(digr_no_re_mm, dogr_no_re_mm, sigr_no_re_mm, sogr_no_re_mm, btgr_no_re_mm, trgr_no_re_mm, ecgr_no_re_mm,
#     spr_no_re_mm, btpr_no_re_mm, trpr_no_re_mm, ecpr_no_re_mm,
#     file = "data/models gam - repeatability - male mixed sex grooming and prox no RE.Rdata")
# #female mixed sex
# save(ii_btgr_f_estr_no_re, ii_btpr_f_estr_no_re, ii_digr_f_estr_no_re, ii_dogr_f_estr_no_re,
#      ii_ecgr_f_estr_no_re, ii_ecpr_f_estr_no_re, ii_sigr_f_estr_no_re, ii_sogr_f_estr_no_re,
#      ii_spr_f_estr_no_re,  ii_trgr_f_estr_no_re, ii_trpr_f_estr_no_re,
#      file = "data/models gam - repeatability - estrus female w rank in mixed sex net no RE.Rdata")
# #same sex
# save(f_digr_no_re, f_dogr_no_re, f_sigr_no_re, f_sogr_no_re, f_btgr_no_re, f_trgr_no_re, f_ecgr_no_re,
#    m_digr_no_re, m_dogr_no_re, m_sigr_no_re, m_sogr_no_re, m_btgr_no_re, m_trgr_no_re, m_ecgr_no_re,
#    f_spr_no_re, f_btpr_no_re, f_trpr_no_re, f_ecpr_no_re,
#    m_spr_no_re, m_btpr_no_re, m_trpr_no_re, m_ecpr_no_re,
#   file = "data/models gam - repeatability - same sex groom and prox no RE.Rdata")


# Calculate observed RE dev expl ------
#rm(list = ls())
source("functions/functions - dev expl re and test sig dev expl re.R")

# - load models and names -----
# -- males mixed w RE
load("data/models gam - mixed sex grooming and total grooming with and without rank - sex specific for viz.Rdata", verbose = T)
load("data/models gam - mixed sex prox with and without rank - sex specific for viz.Rdata", verbose = T)
# -- females mixed w RE
load("data/models gam - estrus female w rank in mixed sex net.Rdata", verbose = T)
# -- same sex w RE
load("data/models gam - same sex grooming and total grooming with and without rank.Rdata", verbose = T)
load("data/models gam - same sex prox with and without rank.Rdata", verbose = T)

# -- males no RE
load("data/models gam - repeatability - male mixed sex grooming and prox no RE.Rdata", verbose = T)
# -- females no RE
load("data/models gam - repeatability - estrus female w rank in mixed sex net no RE.Rdata", verbose = T)
# -- same sex no RE
load("data/models gam - repeatability - same sex groom and prox no RE.Rdata", verbose = T)

# get model names from environment to pair and compare...

# male mixed sex ---
rpt_male_mixed_sex_w_re <- Filter(function(x) 'gam' %in% class(get(x)) & !grepl("no_re", x) & grepl("r$", x) & grepl("^m(?!_)", x, perl = T) , ls())
rpt_male_mixed_sex_no_re <- Filter(function(x) 'gam' %in% class(get(x)) & grepl("mm$", x), ls())
rpt_male_mixed <- data.frame(w_re = rpt_male_mixed_sex_w_re, no_re = rpt_male_mixed_sex_no_re)

# female mixed sex ---
rpt_female_mixed_sex_w_re <- Filter(function(x) 'gam' %in% class(get(x)) & !grepl("no_re", x) & grepl("ii", x), ls())
rpt_female_mixed_sex_no_re <- Filter(function(x) 'gam' %in% class(get(x)) & grepl("no_re", x) & grepl("ii", x), ls())
rpt_female_mixed <- data.frame(w_re = rpt_female_mixed_sex_w_re, no_re = rpt_female_mixed_sex_no_re)

# same sex
rpt_same_sex_w_re <- Filter(function(x) 'gam' %in% class(get(x)) & grepl("^f_|^m_", x) & !grepl("no_re", x) & grepl("r$", x) , ls())
rpt_same_sex_no_re <- Filter(function(x) 'gam' %in% class(get(x)) & grepl("^f_|^m_", x) & grepl("no_re", x) , ls())
rpt_same_sex <- data.frame(w_re = rpt_same_sex_w_re, no_re = rpt_same_sex_no_re)

# pair names of models w and w/out REs  
mods <- do.call("rbind", list(rpt_male_mixed, rpt_female_mixed, rpt_same_sex))

# - compare models ----
obs_re_devs1<- vector(length = nrow(mods))

for(i in seq(nrow(mods))){
  w_re <- get(mods[i,1])
  no_re <- get(mods[i,2]) 
  
  obs_re_devs1[[i]] <- dev_expl_re(w_re, no_re)
  
}

obs_re_devs <- data.frame(mod = mods[,1], obs_re_dev = obs_re_devs1)

#save(obs_re_devs, file ="data/observed deviance explained random effects.Rdata")


# what is purpose of perms?
# run mod_no_re's for each randomized data set
# and then extract dev.expl from both mod_re's and mod_no_re to then run dev_expl_re
# this would give a random distribution of the amount of deviance explained by REs
# to evaluate significance of observed





