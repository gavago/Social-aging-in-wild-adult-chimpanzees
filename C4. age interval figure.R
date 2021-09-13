library(tidyverse)
library(ggrepel)
library(cowplot)
source("data/data sets for gams.R")

# look at age intervals ---
age_int <- mixed_dir_sna_w %>%
  mutate(age_interval = cut(mixed_dir_sna_w $age_mid_year, breaks = c(10,20,30,40,50,60)))

age_int %>% filter(age_interval == "(30,40]") %>% distinct(chimp_id)
age_int %>% filter(age_interval == "(30,40]") %>% distinct(chimp_id)
age_int %>% filter(age_interval == "(40,50]") %>% distinct(chimp_id)
age_int %>% filter(age_interval == "(50,60]") %>% distinct(chimp_id)

#df of min and max ages
dfa <- mixed_dir_sna_w %>%
  mutate(chimp_id = as.character(chimp_id)) %>%
  group_by(chimp_id, sex) %>%
  summarise(min = min(age_mid_year), max = max(age_mid_year)) %>% 
  ungroup()
  #mutate(sex = recode_factor(sex, M = "Male", `F` = "Female"))


dfa %>%
  ggplot() +
  geom_segment(aes(x = fct_reorder(chimp_id, max), xend = fct_reorder(chimp_id, max), y = min, yend = max))

fem_ages <- dfa %>%
  filter(sex == "F") %>%
  ggplot() +
  geom_point(aes(x = max, y = fct_reorder(chimp_id, max)), color = "red") +
  geom_point(aes(x = min, y = fct_reorder(chimp_id, max)), color = "red") +
  geom_segment(aes(x = min, xend = max, y = fct_reorder(chimp_id, max), yend = fct_reorder(chimp_id, max)), color = "red") +
  theme_minimal() +
  labs(x ="Age (yrs)", y = "Subject", title = "Female") +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 11, family = "Georgia"), 
        axis.text.y = element_text(size = 10, family = "Georgia"),
        axis.title.x  = element_text(size = 13, family = "Georgia"),
        axis.title.y  = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, family = "Georgia", hjust = 0.5))


mal_ages <- dfa %>%
  filter(sex == "M") %>%
  ggplot() +
  geom_point(aes(x = max, y = fct_reorder(chimp_id, max)), color = "blue") +
  geom_point(aes(x = min, y = fct_reorder(chimp_id, max)), color = "blue") +
  geom_segment(aes(x = min, xend = max, y = fct_reorder(chimp_id, max), yend = fct_reorder(chimp_id, max)), color = "blue") +
  theme_minimal() +
  labs(x ="Age (yrs)", y = "", title = "Male") +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 11, family = "Georgia"), 
        axis.text.y = element_text(size = 10, family = "Georgia"),
        axis.title.x  = element_text(size = 13, family = "Georgia"),
        axis.title.y  = element_text(size = 13, family = "Georgia"),
        plot.title = element_text(size = 14, family = "Georgia", hjust = 0.5))


cowplot::plot_grid(fem_ages, mal_ages, nrows = 1)
# save 1000 to 500

