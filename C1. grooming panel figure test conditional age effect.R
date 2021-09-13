library(tidyverse)
library(mgcv)
library(mgcViz)
library(grid)
library(gridExtra)
library(ggrepel)
library(visreg)


source("data/data sets for gams.R") # same sex dfs alredy here
sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw
display <- c("red", "blue")

load("data/models gam - mixed sex grooming and total grooming with and without rank.Rdata", verbose = T)
load("data/models gam - mixed sex grooming and total grooming with and without rank - sex specific for viz.Rdata", verbose = T)
load("data/models gam - same sex grooming and total grooming with and without rank.Rdata", verbose = T)



# GROOM MIXED -----
#-msgH1. -------

#      In-Degree mixed sex gm ----
plot(digr, pages = 1)
#gam.check(digr)

fv_digr <- visreg(fdigr, "age_mid_year", scale = "response", plot = T)$fit
mv_digr <- visreg(mdigr, "age_mid_year", scale = "response", plot = T)$fit

v_in_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_in, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fv_digr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mv_digr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_digr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mv_digr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


#      Out-Degree mixed sex gm ----
# plot(dogr, pages = 1) # no match at all, what?

fv_dogr <- visreg(fdogr, "age_mid_year", scale = "response", plot = T)$fit
mv_dogr <- visreg(mdogr, "age_mid_year", scale = "response", plot = T)$fit

v_out_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_out, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fv_dogr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mv_dogr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_dogr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mv_dogr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))

# msgH1b. 
#      In-Strength mixed sex gm -----
#plot(sigr, pages = 1)
# gam.check(sigr)
fv_sigr <- visreg(fsigr, "age_mid_year", scale = "response", plot = T)$fit
mv_sigr <- visreg(msigr, "age_mid_year", scale = "response", plot = T)$fit

v_in_strength <- dir_sna_w %>%
  filter(deg_in < 50) %>%
  filter(network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_in, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fv_sigr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mv_sigr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_sigr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mv_sigr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs(x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0,42, by = 10), limits = c(0, 42))

#      Out-Strength mixed sex gm -----
#plot(sogr, pages = 1) # yet again totally different for out 

fv_sogr <- visreg(fsogr, "age_mid_year", scale = "response", plot = T)$fit
mv_sogr <- visreg(msogr, "age_mid_year", scale = "response", plot = T)$fit

v_out_strength <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_out, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fv_sogr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mv_sogr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_sogr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mv_sogr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia") )


#-msgH2 ------
#     BT mixed sex gm -----
#plot(btgr, pages = 1)

fv_btgr <- visreg(fbtgr, "age_mid_year", scale = "response", plot = T)$fit
mv_btgr <- visreg(mbtgr, "age_mid_year", scale = "response", plot = T)$fit

v_bt_gmgmd <- sna_w %>%
  #filter(bt < 125) %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, bt, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fv_btgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mv_btgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_btgr, aes(age_mid_year, visregFit), method = "gam", se = FALSE, formula = y ~ s(x, k =5), linetype = 1, color = "red") +
  geom_smooth(data =  mv_btgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,155, by = 50), limits = c(0, 155))


#     TRANS mixed sex gm -----
# ggplot matches gam plot well, M and F trans change w age NR and R
# plot(trgr, pages = 1)
# gam.check(trgr)

fv_trgr <- visreg(ftrgr, "age_mid_year", scale = "response", plot = T)$fit
mv_trgr <- visreg(mtrgr, "age_mid_year", scale = "response", plot = T)$fit


v_trans_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, trans, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fv_trgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mv_trgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_trgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mv_trgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs(x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme( legend.position = "none",
         axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
         plot.margin = margin(1,1,1,1, unit = "pt"))


#-msgH3. EC mixed sex gm ------
#plot(ecgr, pages = 1) # ugh, totes different, maybe not?

fv_ecgr <- visreg(fecgr, "age_mid_year", scale = "response", plot = T)$fit
mv_ecgr <- visreg(mecgr, "age_mid_year", scale = "response", plot = T)$fit

v_ec_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, ec, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fv_ecgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mv_ecgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_ecgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mv_ecgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "Age (years)", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1.2, by = .2), limits = c(0, 1.2))


# GROOM SAME -----
#-ssgH1. ------
#      In-Degree same sex gm ----
#plot(f_digr, pages = 1)
#plot(m_digr, pages = 1)

fv_ssdigr<- visreg(f_digr, "age_mid_year", scale = "response", plot = T)$fit
mv_ssdigr<- visreg(m_digr, "age_mid_year", scale = "response", plot = T)$fit


v_in_deg_fem <- f_same_dir_sna_uw %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_in), size = 1 , alpha = 0.5, shape = 1, color = "red") +
  geom_ribbon(data =  fv_ssdigr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_ssdigr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,10), limits = c(0,10))

v_in_deg_mal <- m_same_dir_sna_uw %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_in), size = 1 , alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mv_ssdigr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mv_ssdigr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10), limits = c(0,10))


#      Out-Degree same sex gm ------
#plot(f_dogr, pages = 1)
#plot(m_dogr, pages = 1)

fv_ssdogr <- visreg(f_dogr, "age_mid_year", scale = "response", plot = T)$fit
mv_ssdogr <- visreg(m_dogr, "age_mid_year", scale = "response", plot = T)$fit


v_out_deg_fem <- f_same_dir_sna_uw %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_out), size = 1, alpha = 0.5, shape = 1, color = "red") +
  geom_ribbon(data =  fv_ssdogr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_ssdogr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9))


v_out_deg_mal <- m_same_dir_sna_uw %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_out), size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mv_ssdogr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mv_ssdogr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9)) 


#      In-Strength same sex gm -----
#plot(f_sigr, pages = 1)
#plot(m_sigr, pages = 1)

fv_sssigr <- visreg(f_sigr, "age_mid_year", scale = "response", plot = T)$fit
mv_sssigr <- visreg(m_sigr, "age_mid_year", scale = "response", plot = T)$fit

v_in_strength_fem <- f_same_dir_sna_w %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_in), size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  fv_sssigr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_sssigr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs(x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,40, by = 10), limits = c(0,40))


v_in_strength_mal <- m_same_dir_sna_w %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_in), size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mv_sssigr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mv_sssigr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5),linetype = 2,  color = "blue") +
  labs(x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,40, by = 10), limits = c(0,40))


#      Out-Strength same sex gm -----
#plot(f_sogr, pages = 1)
#plot(m_sogr, pages = 1)

fv_sssogr <- visreg(f_sogr, "age_mid_year", scale = "response", plot = T)$fit
mv_sssogr <- visreg(m_sogr, "age_mid_year", scale = "response", plot = T)$fit

v_out_strength_fem <- f_same_dir_sna_w %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_out), size = 1, shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  fv_sssogr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_sssogr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,25, by =5), limits = c(0,25))

v_out_strength_mal <- m_same_dir_sna_w %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, deg_out), size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mv_sssogr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mv_sssogr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  theme_bw()  +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,25, by =5), limits = c(0,25))


#-ssgH2. -----------
#     BT same sex gm -------
#plot(f_btgr, pages = 1)
#plot(m_btgr, pages = 1)

fv_ssbtgr <- visreg(f_btgr, "age_mid_year", scale = "response", plot = T)$fit
mv_ssbtgr <- visreg(m_btgr, "age_mid_year", scale = "response", plot = T)$fit


v_bt_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, bt), size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  fv_ssbtgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_ssbtgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0,50))


v_bt_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, bt), size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_ribbon(data =  mv_ssbtgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mv_ssbtgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0,50))


#     TRANS same sex gm -----
#plot(f_trgr, pages = 1)
#plot(m_trgr, pages = 1)

fv_sstrgr <- visreg(f_trgr, "age_mid_year", scale = "response", plot = T)$fit
mv_sstrgr <- visreg(m_trgr, "age_mid_year", scale = "response", plot = T)$fit

v_trans_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, trans), size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  fv_sstrgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_sstrgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") + 
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


v_trans_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, trans), size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_ribbon(data =  mv_sstrgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mv_sstrgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2,  color = "blue") + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


#-ssgH3. EC Same sex gm -----
#plot(f_ecgr, pages = 1)
#plot(m_ecgr, pages = 1)

fv_ssecgr <- visreg(f_ecgr, "age_mid_year", scale = "response", plot = T)$fit
mv_ssecgr <- visreg(m_ecgr, "age_mid_year", scale = "response", plot = T)$fit

v_ec_gmgmd_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, ec), size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  fv_ssecgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  fv_ssecgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") + 
  labs( x = "Age (years)", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0,1.2, by = .2), limits = c(0, 1.2))

v_ec_gmgmd_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(age_mid_year, ec), size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mv_ssecgr, aes(age_mid_year, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mv_ssecgr, aes(age_mid_year, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") + 
  labs( x = "Age (years)", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0,1.2, by = .2), limits = c(0, 1.2))

# save grooming panel -------
r_grobs <- list(v_in_deg,  v_in_deg_mal, v_in_deg_fem,
                v_out_deg, v_out_deg_mal, v_out_deg_fem,
                v_in_strength, v_in_strength_mal, v_in_strength_fem,
                v_out_strength, v_out_strength_mal,v_out_strength_fem,
                v_trans_gmgmd, v_trans_gmgmd_w_mal,  v_trans_gmgmd_w_fem,
                v_bt_gmgmd, v_bt_gmgmd_w_mal, v_bt_gmgmd_w_fem, 
                v_ec_gmgmd, v_ec_gmgmd_mal, v_ec_gmgmd_fem)

sna <- tableGrob(c( "In-Degree", "Out-Degree", "In-Strength",
                    "Out-Strength", "Local \nTransitivity", 
                    "Betweenness", "Eigenvector \nCentrality"), 
                 theme = ttheme_minimal(base_size = 14, base_family = "Georgia"))

grooming <- tableGrob(t(c("", "Grooming", "")), theme = ttheme_minimal(base_family = "Georgia", base_size = 17), rows = "")
title <- tableGrob(t(c("Mixed-sex", "All Male", "All Female")), theme = ttheme_minimal(base_family = "Georgia", base_size = 14), rows = "")

r_grob_bind <- rbind(title, 
                     cbind(sna, arrangeGrob(grobs = r_grobs,  nrow = 7, ncol = 3),
                           size = "last"), size = "last")

r_grob_bind2 <- rbind( grooming, rbind(title, 
                                       cbind(sna, arrangeGrob(grobs = r_grobs,  nrow = 7, ncol = 3),
                                             size = "last"), size = "last"), size = "last")

dev.off()
grid.draw(r_grob_bind2)

#ggsave(filename = "results/visualization/GAM figures/Fig 3. Grooming panel.png", plot = r_grob_bind2, height = 12, width = 8)




# GRAVEYARD -----

