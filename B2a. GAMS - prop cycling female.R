library(mgcv)
library(mgcViz)
library(fitdistrplus)
library(gridExtra)
library(grid)
source("data/data sets for gams.R")

# Expect that the relationship between integration and prop_cyc per year more
# positive as age increases
# or in viz, integration higher as time swollen

# however, overall goal is to determine whether age effect on integration is independent of reproductive status
# could address 



# time swollen by age ----
prop_cyc_age <- gam(prop_cyc ~ s(age_mid_year, k =5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
gam.check(prop_cyc_age)
summary(prop_cyc_age)
plot(prop_cyc_age, pages = 1)


# i. viz explore bivariate: age, prop cycling, in-strength -----

# strange, has same appearance as deg in w age, looks promising, as if prop year cycling influences deg in.
a <- f_mixed_dir_sna_w %>%
  mutate(strength_grooming_rec = deg_in) %>%
  ggplot(aes(y = prop_cyc, x = age_mid_year)) +
  geom_point(aes(size = strength_grooming_rec), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5)) +
  labs(x = "Age", title = "Time swollen by age", subtitle = "size of dot = time receiving grooming", y = "Annual time estrlen") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", plot.subtitle = element_text(hjust = 0.5))

# same pattern w grooming, femaels hat spend most time cycling are youngest
b <- f_mixed_dir_sna_w %>%
  ggplot(aes(x = prop_cyc, y = deg_in)) +
  geom_point(aes(size = age_mid_year), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5)) +
  labs(title = "Time receiving grooming by \n time estrlen", subtitle = "size of dot = age", y = "In-Strength Grooming", x = "Annual time estrlen") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", plot.subtitle = element_text(hjust = 0.5))

# seems like females spend less time around others the more time in the year they are cycling
c <- f_mixed_dir_sna_uw %>%
  ggplot(aes(x = prop_cyc, y = deg_in)) +
  geom_point(aes(size = age_mid_year), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5)) +
  labs(title = "Number partners grooming subject \n by time estrlen", subtitle = "size of dot = age", y = "Number partners grooming subject", x = "Annual time estrlen") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", plot.subtitle = element_text(hjust = 0.5))


# seems like females spend less time around others the more time in the year they are cycling
d <- f_mixed_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(x = prop_cyc, y = deg)) +
  geom_point(aes(size = age_mid_year), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5)) +
  labs(title = "Time in prox by time estrlen", subtitle = "size of dot = age", y = "Prox Strength", x = "Annual time estrlen") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", plot.subtitle = element_text(hjust = 0.5))


dev.off()
g <- arrangeGrob(a,b,c,d,
                 nrow = 2, top = textGrob("Bivariate relationships: age, prop year estrlen, degree & strength", gp=gpar(fontsize = 10)))
grid.draw(g)
#ggsave(filename = "results/visualization/GAM figures/Explore bivariate relations among time estrlen, age, and association.png", plot = g, height = 7, width = 7)



# ii. explore gam structures ----

# 1. creates a smooth surface: includes main effects in interaction, one being sig would drown the other out and carry the signal
# special kind of interaction, see https://www.youtube.com/watch?v=sgw4cu8hrZM at 1:19
digr_f_cyc <- gam(deg_in ~ te(age_mid_year, prop_cyc, k = 5)+ s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
# 2. main effects and indpendent interaction effects (best to do w continuous vars)
#no sig effect of interaction alone
digr_f_cyc <- gam(deg_in ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
# 3. just main effects of each, prop no relationship w in degree
#age main effect sig
digr_f_cyc <- gam(deg_in ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")


summary(digr_f_cyc)
plot.gam(digr_f_cyc, pages = 1, scheme = 1)
vis.gam(digr_f_cyc, view = c("age_mid_year", "prop_cyc"), plot.type = 'persp', phi = 220) 

#theta horizontal rotation
#phi verical
#r zoomed in
# plot.type = 'contour"

# WITH RANK - prop cyc as main effect and independent interaction w age ------
## Grooming -----
# -- in out deg strength ----

ii_digr_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(ii_digr_f_estr)
gam.check(ii_digr_f_estr)

ii_dogr_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(ii_dogr_f_estr)
gam.check(ii_dogr_f_estr)

ii_sigr_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")
summary(ii_sigr_f_estr) # main effect increase in strength
plot(ii_sigr_f_estr, pages = 1)
gam.check(ii_sigr_f_estr)

ii_sogr_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")
summary(ii_sogr_f_estr) # main effect increase out strength
gam.check(ii_sogr_f_estr, pages = 1)


# -- social role ------
ii_trgr_f_estr <- gam(trans ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(ii_trgr_f_estr) 
gam.check(ii_trgr_f_estr)

ii_btgr_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(ii_btgr_f_estr)
gam.check(ii_btgr_f_estr)

# -- eigenvector centrality ----
ii_ecgr_f_estr <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(ii_ecgr_f_estr)
plot.gam(ii_ecgr_f_estr, pages = 1)
gam.check(ii_ecgr_f_estr)

## Proximity ----

# -- strength ----
ii_spr_f_estr <- gam(deg ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
summary(ii_spr_f_estr) # main effect increase out strength
gam.check(ii_spr_f_estr, pages = 1)

# -- social role ------
ii_trpr_f_estr <- gam(trans ~ s(age_mid_year, k = 3) + s(avg_rank, k = 5) + s(prop_cyc, k = 3) + ti(age_mid_year, prop_cyc, k = 3) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
summary(ii_trpr_f_estr) 
gam.check(ii_trpr_f_estr) # model is bad

ii_btpr_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
gam.check(ii_btpr_f_estr)

# -- eigenvector centrality ----
ii_ecpr_f_estr <- gam(ec ~ s(age_mid_year, k =5) + s(avg_rank, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
gam.check(ii_ecpr_f_estr)

# Save ----

# find names
rpt_female_mixed_sex_w_re <- Filter(function(x) 'gam' %in% class(get(x)) & grepl("^ii", x) & grepl("r_f", x) , ls())

# save(ii_btgr_f_estr, ii_btpr_f_estr, ii_digr_f_estr, ii_dogr_f_estr,
#      ii_ecgr_f_estr, ii_ecpr_f_estr, ii_sigr_f_estr, ii_sogr_f_estr,
#      ii_spr_f_estr,  ii_trgr_f_estr, ii_trpr_f_estr, file = "data/models gam - estrus female w rank in mixed sex net.Rdata")

load("data/models gam - estrus female w rank in mixed sex net.Rdata")
# Explore sig effects models ----- 
load("data/models gam - estrus female w rank in mixed sex net.Rdata", verbose = T)
vis.gam(ii_ecpr_f_estr, c("age_mid_year", "prop_cyc")) # same effect
vis.gam(ii_sigr_f_estr, c("age_mid_year", "prop_cyc"))  # more proximity and time receiving grooming with time swollen amnog older females
plot(ii_ecpr_f_estr, pages = 1) # time swollen increase time grooming.


# NO RANK ---prop cyc as main effect and independent interaction w age ------
##  Grooming -----
# -- in out deg strength ----

ii_dig_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(ii_dig_f_estr)
gam.check(ii_dig_f_estr)

ii_dog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(ii_dog_f_estr)
gam.check(ii_dog_f_estr)

ii_sig_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")
summary(ii_sig_f_estr) # main effect decrease in strength
plot.gam(ii_sig_f_estr, pages = 1)

ii_sog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")
summary(ii_sog_f_estr) # main effect increase out strength
plot.gam(ii_sog_f_estr, pages = 1)
vis.gam(ii_sog_f_estr)

# -- social role ------
ii_trg_f_estr <- gam(trans ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(ii_trg_f_estr) 
gam.check(ii_trg_f_estr)

ii_btg_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(ii_btg_f_estr) 
plot.gam(ii_btg_f_estr, pages = 1)
gam.check(ii_btg_f_estr)

# -- eigenvector centrality ----
ii_ecg_f_estr <- gam(ec ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(ii_ecg_f_estr)
plot.gam(ii_ecg_f_estr, pages = 1)
vis.gam(ii_ecg_f_estr)

##  Proximity ----

# -- strength ----
ii_sp_f_estr <- gam(deg ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
summary(ii_sp_f_estr) # main effect increase out strength
gam.check(ii_sp_f_estr, pages = 1)
plot.gam(ii_sp_f_estr, pages = 1)
vis.gam(ii_sp_f_estr)

# -- social role ------
ii_trp_f_estr <- gam(trans ~ s(age_mid_year, k = 3) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
summary(ii_trp_f_estr) 
gam.check(ii_trp_f_estr) # model is bad

ii_btp_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
summary(ii_btp_f_estr) # no apparent change w prop cyc but says sig
plot.gam(ii_btp_f_estr, pages = 1)
gam.check(ii_btp_f_estr)


# -- eigenvector centrality ----
ii_ecp_f_estr <- gam(ec ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
summary(ii_ecp_f_estr)
plot.gam(ii_ecp_f_estr, pages = 1)
vis.gam(ii_ecp_f_estr)

# save ------

filter_gam <- function(x) inherits(get(x),"gam")

estr_gam <- Filter(filter_gam, ls())

# save(ii_btg_f_estr, ii_btp_f_estr, ii_dig_f_estr, ii_dog_f_estr, ii_ecg_f_estr, ii_ecp_f_estr,
#  ii_sig_f_estr, ii_sog_f_estr,ii_sp_f_estr,ii_trg_f_estr, ii_trp_f_estr, file = "data/models - estrus female in mixed sex net.R")

# ----------------------- ALTERNATIVE models ------
# B. prop cyc as main effect ------
# -- in out deg strength ----

me_dig_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(me_dig_f_estr)
gam.check(dig_f_estr)

me_dog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(me_dog_f_estr)
gam.check(dog_f_estr)

me_sig_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
summary(me_sig_f_estr) # main effect decrease in strength
plot.gam(me_sig_f_estr, pages = 1)

me_sog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
summary(me_sog_f_estr) # main effect increase out strength
plot.gam(me_sog_f_estr, pages = 1)

# -- social role ------
me_trans_f_estr <- gam(trans ~ s(age_mid_year, k = 3) + s(prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(me_trans_f_estr) 
gam.check(trans_f_estr) # model is bad

me_bt_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(me_bt_f_estr) # no apparent change w prop cyc but says sig
plot.gam(me_bt_f_estr, pages = 1)
gam.check(me_bt_f_estr)



# -- eigenvector centrality ----
me_ec_f_estr <- gam(ec ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(me_ec_f_estr)
gam.check(me_ec_f_estr)
plot.gam(me_ec_f_estr, pages = 1)

# C. tensor smooths: time swollen + age (grooming) ------- 
# -- in out deg strength ----

dig_f_estr <- gam(deg_in ~ te(age_mid_year, prop_cyc, k =5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(dig_f_estr)
gam.check(dig_f_estr)

dog_f_estr <- gam(deg_out ~ te(age_mid_year, prop_cyc, k =5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(dog_f_estr)
gam.check(dog_f_estr)

sig_f_estr <- gam(deg_in ~ te(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
summary(sig_f_estr) #interaction sig, use te instead of s when predictors not on same scale
gam.check(sig_f_estr)

sog_f_estr <- gam(deg_out ~ te(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
summary(sog_f_estr)
gam.check(sog_f_estr)

vis.gam(dig_f_estr,  plot.type='contour', color='topo')
vis.gam(dog_f_estr,  plot.type='contour', color='topo')
vis.gam(sig_f_estr,  plot.type='contour', color='topo')
vis.gam(sog_f_estr,  plot.type='contour', color='topo')

vis.gam(dig_f_estr, plot.type='persp', phi = 5, theta = -30, ticktype = "detailed")# number of partners that groom subject highest when age and time estrlen highest
vis.gam(dog_f_estr,  plot.type='persp', phi = -5, theta = -40, ticktype = "detailed") # number of partner subject grooms highest when age and time estrlen highest
vis.gam(sig_f_estr,  plot.type='persp', phi = -5, theta = 60, ticktype = "detailed") # time receiving grooming lowest when age and time estrlen highest
vis.gam(sog_f_estr,  plot.type='persp', phi = -5, theta = -40, ticktype = "detailed") # time giving grooming highest when age and time estrlen highest



# -- social role ------
trans_f_estr <- gam(trans ~ te(age_mid_year, prop_cyc, k = 10) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(trans_f_estr)
gam.check(trans_f_estr)

bt_f_estr <- gam(bt ~ te(age_mid_year, prop_cyc, k = 10) + s(chimp_id, bs = "re"), data = f_mixed_sna_w  %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(bt_f_estr)
gam.check(bt_f_estr)


vis.gam(trans_f_estr,  plot.type='contour', color='topo')
vis.gam(bt_f_estr,  plot.type='contour', color='topo')

vis.gam(trans_f_estr,  plot.type='persp', phi = -5)
vis.gam(bt_f_estr,  plot.type='persp', phi = -5)



# -- eigenvector centrality ----
ec_f_estr <- gam(ec ~ te(age_mid_year, prop_cyc, k = 10) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
summary(ec_f_estr)
gam.check(trans_f_estr)

vis.gam(ec_f_estr, plot.type = "persp", phi = -5, theta = -40)



# gyard ---- 
# summary(digr_f_cyc)
# summary(dogr_f_cyc)
# summary(sigr_f_cyc)
# summary(sogr_f_cyc)

btgr_f_cyc <- gam(bt ~ s(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w, family = gaussian(link = "log"), method = "REML")
trgr_f_cyc <- gam(trans ~ s(age_mid_year, prop_cyc, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w, family = gaussian(link = "log"), method = "REML")
ecgr_f_cyc <- gam(ec ~ s(age_mid_year, prop_cyc, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w, family = gaussian(link = "log"), method = "REML")

summary(btgr_f_cyc)
summary(trgr_f_cyc)
summary(ecgr_f_cyc)

# horrible fit, maybe do hurdle?
#prop_cyc_age <- gam(prop_cyc + 0.001 ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")
#summary(prop_cyc_age)
prop_cyc_age_bin <- gam(yes_no_cyc ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w %>% mutate(yes_no_cyc = ifelse(prop_cyc == 0, 0, 1)), family = binomial(link = "logit"), method = "REML")
prop_cyc_age_cont <- gam(prop_cyc ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w %>% filter(prop_cyc > 0), family = Gamma(link = "log"), method = "REML")
gam.check(prop_cyc_age_cont)
summary(prop_cyc_age_cont)
plot(prop_cyc_age_cont, pages = 1)

btpr_f_cyc <- gam(bt ~ s(age_mid_year, prop_cyc, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
trpr_f_cyc <- gam(trans ~ s(age_mid_year, prop_cyc, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ecpr_f_cyc <- gam(ec ~ s(age_mid_year, prop_cyc, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

summary(btpr_f_cyc)
summary(trpr_f_cyc)
summary(ecpr_f_cyc)


# hypothesize that older females are more attractive to males -

# so would expect that older females have higher deg in, holding prop cycling constant
# expect that females would receive more attention when cycling
# and that the attractionis magnified the older they are 
# (should use marginal effects plot - slope of sociality by prop cycling relative to age)

# BUT proportion of year cycling is not good for evaluating the sexual nature of social attraction
# because it it may have negative relationships w attractiveness
# The longer a female spends cycling in a given year could indicate how undesirable she is
# may be that highly attractive females 
# are snapped up and conceive when they first hit maximal swelling...


# prop cycling alone not influence degree gm-in when not in interaction
deg_in_ni <- gam(deg_in ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5)  + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(deg_in_ni)

# prop cycling alone DECREASE strength gm-in (only > 0.3 prop cycling) when not in interaction
str_in_ni <- gam(deg_in ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5)  + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
summary(str_in_ni)
plot.gam(str_in_ni, pages = 1)

## prop cycling alone not influence degree gm-out when not in interaction
deg_out_ni <- gam(deg_out ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5)  + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
summary(deg_out_ni)

## prop cycling alone *INCREASE* strength gm-out when not in interaction
# could be grooming more becuase appeasing males more, or because are largely young
# nulliparous and socially needy
str_out_ni <- gam(deg_out ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5)  + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
summary(str_out_ni)
f_str_out_ni <- gam(deg_out ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5)  + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")
summary(f_str_out_ni)

plot.gam(str_out_ni, pages = 1)
plot.gam(f_str_out_ni, pages = 1)
gam.check(str_out_ni)

## prop cycling alone *DECREASE* strength prox when not in interaction
str_prox_ni <- gam(deg ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5)  + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
summary(str_prox_ni)
plot.gam(str_prox_ni, pages = 1)

# Takeaway --- time estrlen decreases the total time that a female receives grooming and spends in association,
# but increases the time she spend giving grooming
# does not influence the number of partners she interacts with.

# see str_prox_ni: any interaction between age and prop cyc in relation to time in prox
# is that time in prox decreases with both age and time cycling. bc females decrease time cycling with age, it might actually
# mean that the overall decrease in social interaction with age for females is actually attenuated by there
# reproductive aging (if they kept up cycling, they might be even more asocial with age?)


# pr