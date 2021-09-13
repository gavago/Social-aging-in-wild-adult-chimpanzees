library(tidyverse)
library(mgcv)
library(mgcViz)
library(grid)
library(gridExtra)
library(ggrepel)
library(visreg)


source("data/data sets for gams.R") # same sex dfs already here
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
 # gam.check(digr)

frv_digr <- visreg(fdigr, "avg_rank", scale = "response", plot = T)$fit
mrv_digr <- visreg(mdigr, "avg_rank", scale = "response", plot = T)$fit

rv_in_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_in, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  frv_digr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mrv_digr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_digr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mrv_digr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,25, by = 5), limits = c(0, 25)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      Out-Degree mixed sex gm ----
# plot(dogr, pages = 1) # no match at all, what?

 frv_dogr <- visreg(fdogr, "avg_rank", scale = "response", plot = T)$fit
 mrv_dogr <- visreg(mdogr, "avg_rank", scale = "response", plot = T)$fit
 
rv_out_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_out, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  frv_dogr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mrv_dogr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_dogr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mrv_dogr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,30, by = 5), limits = c(0,30)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

# msgH1b. 
#      In-Strength mixed sex gm -----
#plot(sigr, pages = 1)
# gam.check(sigr)
frv_sigr <- visreg(fsigr, "avg_rank", scale = "response", plot = T)$fit
mrv_sigr <- visreg(msigr, "avg_rank", scale = "response", plot = T)$fit
plot.gam(msigr, pages = 1) # different from male rank effect in
plot.gam(sigr, pages = 1)

rv_in_strength <- dir_sna_w %>%
  filter(deg_in < 50) %>%
  filter(network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_in, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  frv_sigr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mrv_sigr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_sigr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mrv_sigr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs(x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,65, by =10), limits = c(0,65)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      Out-Strength mixed sex gm -----
#plot(sogr, pages = 1) # yet again totally different for out 

frv_sogr <- visreg(fsogr, "avg_rank", scale = "response", plot = T)$fit
mrv_sogr <- visreg(msogr, "avg_rank", scale = "response", plot = T)$fit

rv_out_strength <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_out, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  frv_sogr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mrv_sogr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_sogr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mrv_sogr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia") ) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-msgH2 ------
#     BT mixed sex gm -----
#plot(btgr, pages = 1)

frv_btgr <- visreg(fbtgr, "avg_rank", scale = "response", plot = T)$fit
mrv_btgr <- visreg(mbtgr, "avg_rank", scale = "response", plot = T)$fit

rv_bt_gmgmd <- sna_w %>%
  filter(bt < 125) %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, bt, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  frv_btgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mrv_btgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_btgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mrv_btgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#     TRANS mixed sex gm -----
# ggplot matches gam plot well, M and F trans change w age NR and R
# plot(trgr, pages = 1)
# gam.check(trgr)

frv_trgr <- visreg(ftrgr, "avg_rank", scale = "response", plot = T)$fit
mrv_trgr <- visreg(mtrgr, "avg_rank", scale = "response", plot = T)$fit


rv_trans_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, trans, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  frv_trgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mrv_trgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_trgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mrv_trgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs(x = "", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme( legend.position = "none",
         axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
         plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-msgH3. EC mixed sex gm ------
#plot(ecgr, pages = 1) # ugh, totes different, maybe not?

frv_ecgr <- visreg(fecgr, "avg_rank", scale = "response", plot = T)$fit
mrv_ecgr <- visreg(mecgr, "avg_rank", scale = "response", plot = T)$fit

rv_ec_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, ec, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  frv_ecgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_ribbon(data =  mrv_ecgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_ecgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  geom_smooth(data =  mrv_ecgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "Dominance rank", y = "") +
  theme_bw() +
  scale_color_manual(name = "sex", values = display) +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


# GROOM SAME -----
#-ssgH1. ------
#      In-Degree same sex gm ----
#plot(f_digr, pages = 1)
#plot(m_digr, pages = 1)

frv_ssdigr<- visreg(f_digr, "avg_rank", scale = "response", plot = T)$fit
mrv_ssdigr<- visreg(m_digr, "avg_rank", scale = "response", plot = T)$fit


rv_in_deg_fem <- f_same_dir_sna_uw %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_in), size = 1 , alpha = 0.5, shape = 1, color = "red") +
  geom_ribbon(data =  frv_ssdigr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_ssdigr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,10, by =2), limits = c(0,10)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

rv_in_deg_mal <- m_same_dir_sna_uw %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_in), size = 1 , alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mrv_ssdigr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mrv_ssdigr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,10, by =2), limits = c(0,10)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      Out-Degree same sex gm ------
#plot(f_dogr, pages = 1)
#plot(m_dogr, pages = 1)

frv_ssdogr <- visreg(f_dogr, "avg_rank", scale = "response", plot = T)$fit
mrv_ssdogr <- visreg(m_dogr, "avg_rank", scale = "response", plot = T)$fit


rv_out_deg_fem <- f_same_dir_sna_uw %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_out), size = 1, alpha = 0.5, shape = 1, color = "red") +
  geom_ribbon(data =  frv_ssdogr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_ssdogr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12), limits = c(0,12)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


rv_out_deg_mal <- m_same_dir_sna_uw %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_out), size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mrv_ssdogr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mrv_ssdogr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12), limits = c(0,12)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      In-Strength same sex gm -----
#plot(f_sigr, pages = 1)
#plot(m_sigr, pages = 1)

frv_sssigr <- visreg(f_sigr, "avg_rank", scale = "response", plot = T)$fit
mrv_sssigr <- visreg(m_sigr, "avg_rank", scale = "response", plot = T)$fit

rv_in_strength_fem <- f_same_dir_sna_w %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_in), size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  frv_sssigr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_sssigr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs(x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,8,2), limits = c(0,8))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


rv_in_strength_mal <- m_same_dir_sna_w %>% #looks weird
  ggplot() +
  geom_jitter(aes(avg_rank, deg_in), size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mrv_sssigr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mrv_sssigr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5),linetype = 2,  color = "blue") +
  labs(x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,60, by =20), limits = c(0,60)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      Out-Strength same sex gm -----
#plot(f_sogr, pages = 1)
#plot(m_sogr, pages = 1)

frv_sssogr <- visreg(f_sogr, "avg_rank", scale = "response", plot = T)$fit
mrv_sssogr <- visreg(m_sogr, "avg_rank", scale = "response", plot = T)$fit

rv_out_strength_fem <- f_same_dir_sna_w %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_out), size = 1, shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  frv_sssogr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_sssogr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  #scale_y_continuous(breaks = seq(0,25,by = 5), limits = c(0,25)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

rv_out_strength_mal <- m_same_dir_sna_w %>%
  ggplot() +
  geom_jitter(aes(avg_rank, deg_out), size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mrv_sssogr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mrv_sssogr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  theme_bw()  +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  #scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,15)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-ssgH2. -----------
#     BT same sex gm -------
#plot(f_btgr, pages = 1)
#plot(m_btgr, pages = 1)

frv_ssbtgr <- visreg(f_btgr, "avg_rank", scale = "response", plot = T)$fit
mrv_ssbtgr <- visreg(m_btgr, "avg_rank", scale = "response", plot = T)$fit


rv_bt_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, bt), size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  frv_ssbtgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_ssbtgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0,50)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))



rv_bt_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, bt), size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_ribbon(data =  mrv_ssbtgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mrv_ssbtgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0,50)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#     TRANS same sex gm -----
#plot(f_trgr, pages = 1)
#plot(m_trgr, pages = 1)

frv_sstrgr <- visreg(f_trgr, "avg_rank", scale = "response", plot = T)$fit
mrv_sstrgr <- visreg(m_trgr, "avg_rank", scale = "response", plot = T)$fit

rv_trans_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, trans), size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  frv_sstrgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_sstrgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") + 
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


rv_trans_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, trans), size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_ribbon(data =  mrv_sstrgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mrv_sstrgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2,  color = "blue") + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-ssgH3. EC Same sex gm -----
#plot(f_ecgr, pages = 1)
#plot(m_ecgr, pages = 1)

frv_ssecgr <- visreg(f_ecgr, "avg_rank", scale = "response", plot = T)$fit
mrv_ssecgr <- visreg(m_ecgr, "avg_rank", scale = "response", plot = T)$fit

rv_ec_gmgmd_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, ec), size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_ribbon(data =  frv_ssecgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  frv_ssecgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") + 
  labs( x = "Dominance rank", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1.2,.3), limits = c(0,1.2)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

rv_ec_gmgmd_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot() +
  geom_jitter(aes(avg_rank, ec), size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_ribbon(data =  mrv_ssecgr, aes(avg_rank, ymin = visregLwr, ymax = visregUpr),  alpha = 0.2) +
  geom_smooth(data =  mrv_ssecgr, aes(avg_rank, visregFit), method = "gam", formula = y ~ s(x, k =5), linetype = 2, color = "blue") + 
  labs( x = "Dominance rank", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1.2,.3), limits = c(0,1.2)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

# save grooming panel -------
r_grobs <- list(rv_in_deg,  rv_in_deg_mal, rv_in_deg_fem,
              rv_out_deg, rv_out_deg_mal, rv_out_deg_fem,
              rv_in_strength, rv_in_strength_mal, rv_in_strength_fem,
              rv_out_strength, rv_out_strength_mal,rv_out_strength_fem,
              rv_trans_gmgmd, rv_trans_gmgmd_w_mal,  rv_trans_gmgmd_w_fem,
              rv_bt_gmgmd, rv_bt_gmgmd_w_mal, rv_bt_gmgmd_w_fem, 
              rv_ec_gmgmd, rv_ec_gmgmd_mal, rv_ec_gmgmd_fem)

sna <- tableGrob(c( "In-Degree", "Out-Degree", "In-Strength",
                    "Out-Strength", "Local \nTransitivity", 
                    "Betweenness", "Eigenvector \nCentrality"), 
                 theme = ttheme_minimal(base_size = 14, base_family = "Georgia"))

grooming <- tableGrob(t(c("", "Grooming by rank", "")), theme = ttheme_minimal(base_family = "Georgia", base_size = 17), rows = "")
title <- tableGrob(t(c("Mixed-sex", "All Male", "All Female")), theme = ttheme_minimal(base_family = "Georgia", base_size = 14), rows = "")

r_grob_bind <- rbind(title, 
                   cbind(sna, arrangeGrob(grobs = r_grobs,  nrow = 7, ncol = 3),
                         size = "last"), size = "last")

r_grob_bind2 <- rbind( grooming, rbind(title, 
                                     cbind(sna, arrangeGrob(grobs = r_grobs,  nrow = 7, ncol = 3),
                                           size = "last"), size = "last"), size = "last")

dev.off()
grid.draw(r_grob_bind2)

#ggsave(filename = "results/visualization/GAM figures/Fig S1. Grooming by rank panel.png", plot = r_grob_bind2, height = 12, width = 8)




# GRAVEYARD -----

