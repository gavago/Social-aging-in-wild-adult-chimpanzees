library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)
library(mgcViz)
library(visreg)

source("data/data sets for gams.R") # same sex dfs alredy here
sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw


load("data/models gam - estrus female w rank in mixed sex net.Rdata", verbose = T)

#-msgH1. -------

#      In-Degree mixed sex gm ----
plot(ii_digr_f_estr, pages =1)

fev_digr <- visreg(ii_digr_f_estr, "prop_cyc", scale = "response", plot = T)$fit

fev_in_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo" & sex == "F") %>%
  ggplot() +
  geom_jitter(aes(prop_cyc, deg_in, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fev_digr, aes(prop_cyc, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_smooth(data =  fev_digr, aes(prop_cyc, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "", title = "In-Degree") +
  theme_bw() +
  theme(plot.title = element_text(family = "Georgia", size = 12, hjust = 0.5),
        axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


#      Out-Degree mixed sex gm ----
# plot(dogr, pages = 1) # no match at all, what?
plot(ii_dogr_f_estr, pages =1)

fev_dogr <- visreg(ii_dogr_f_estr, "prop_cyc", scale = "response", plot = T)$fit

fev_out_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo" & sex == "F") %>%
  ggplot() +
  geom_jitter(aes(prop_cyc, deg_out, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fev_dogr, aes(prop_cyc, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_smooth(data =  fev_dogr, aes(prop_cyc, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "", title = "Out-Degree") +
  theme_bw() +
  theme(plot.title = element_text(family = "Georgia", size = 12, hjust = 0.5),
        axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))

# msgH1b. 
#      In-Strength mixed sex gm -----

fev_sigr <- visreg(ii_sigr_f_estr, "prop_cyc", scale = "response", plot = T)$fit

fev_in_strength <- dir_sna_w %>%
  filter(network_sex == "any_combo" & sex == "F") %>%
  ggplot() +
  geom_jitter(aes(prop_cyc, deg_in, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fev_sigr, aes(prop_cyc, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_smooth(data =  fev_sigr, aes(prop_cyc, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "", title = "In-Strength") +
  theme_bw() +
  theme(plot.title = element_text(family = "Georgia", size = 12, hjust = 0.5),
        axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))



#      Out-Strength mixed sex gm -----

fev_sogr <- visreg(ii_sogr_f_estr, "prop_cyc", scale = "response", plot = T)$fit

fev_out_strength <- dir_sna_w %>%
  filter(network_sex == "any_combo" & sex == "F") %>%
  ggplot() +
  geom_jitter(aes(prop_cyc, deg_out, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fev_sogr, aes(prop_cyc, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_smooth(data =  fev_sogr, aes(prop_cyc, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "", title = "Out-Strength") +
  theme_bw() +
  theme(plot.title = element_text(family = "Georgia", size = 12, hjust = 0.5),
        axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


#-msgH2 ------
#     BT mixed sex gm -----

fev_btgr <- visreg(ii_btgr_f_estr, "prop_cyc", scale = "response", plot = T)$fit

fev_bt_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo" & sex == "F") %>%
  ggplot() +
  geom_jitter(aes(prop_cyc, bt, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fev_btgr, aes(prop_cyc, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_smooth(data =  fev_btgr, aes(prop_cyc, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "", title = "Betweenness") +
  theme_bw() +
  theme(plot.title = element_text(family = "Georgia", size = 12, hjust = 0.5),
        axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))

#     TRANS mixed sex gm -----

fev_trgr <- visreg(ii_trgr_f_estr, "prop_cyc", scale = "response", plot = T)$fit

fev_tr_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo" & sex == "F") %>%
  ggplot() +
  geom_jitter(aes(prop_cyc, trans, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fev_trgr, aes(prop_cyc, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_smooth(data =  fev_trgr, aes(prop_cyc, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "", y = "", title = "Local Transitivity") +
  theme_bw() +
  theme(plot.title = element_text(family = "Georgia", size = 12, hjust = 0.5),
        axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,1,.2), limits = c(0,1))


#-msgH3. EC mixed sex gm ------
#plot(ecgr, pages = 1) # ugh, totes different, maybe not?

fev_ecgr <- visreg(ii_ecgr_f_estr, "prop_cyc", scale = "response", plot = T)$fit

fev_ec_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo" & sex == "F") %>%
  ggplot() +
  geom_jitter(aes(prop_cyc, ec, color = sex, shape = sex), size = 1, alpha = 0.5) +
  geom_ribbon(data =  fev_ecgr, aes(prop_cyc, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_smooth(data =  fev_ecgr, aes(prop_cyc, visregFit), method = "gam", formula = y ~ s(x, k =5), color = "red") +
  labs( x = "Time swollen", y = "", title = "Eigenvector Centrality") +
  theme_bw() +
  theme(plot.title = element_text(family = "Georgia", size = 12, hjust = 0.5),
        axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) 

# save panel -----

#e_grobs1 <- list(fev_in_deg, fev_out_deg, fev_in_strength, fev_out_strength, fev_tr_gmgmd, fev_bt_gmgmd, fev_ec_gmgmd) %>% lapply(., ggplotGrob)
#e_grobs <- list(e_grobs1[[1]], e_grobs1[[2]], e_grobs1[[3]], e_grobs1[[4]], e_grobs1[[5]], e_grobs1[[6]], grid::nullGrob(), e_grobs1[[7]], grid::nullGrob())
#eg <- arrangeGrob(grobs = e_grobs, byrow = T, ncol = 4, layout_matrix = matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,8,8,9)))

e_grobs <- list(fev_in_deg, fev_out_deg, fev_in_strength, fev_out_strength, fev_tr_gmgmd, fev_bt_gmgmd, fev_ec_gmgmd)
eg <- arrangeGrob(grobs = e_grobs, nrow = 4, ncol = 2)

dev.off()
grid.draw(eg)

ggsave(filename = "results/visualization/GAM figures/Fig S2. female mixed sex sna by time swollen.png", plot = eg, height = 8, width = 5)

# interaction figure ------
png(file = "results/visualization/GAM figures/Fig S3. strength out prop cyc and age.png")

vis.gam(ii_sigr_f_estr, c("age_mid_year", "prop_cyc"), theta = -40) # strength out is high when females young and swollen

dev.off()

# export as Fig S2. strength out prop cyc and age.png


# GRAVEYARD -----

