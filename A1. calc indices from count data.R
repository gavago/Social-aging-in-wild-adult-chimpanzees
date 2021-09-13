library(tidyverse)
library(magrittr)
library(readxl)
source("functions/functions - data preparation.R")

# Rdata files output from this file are
# 1. annual dyadic grooming indices.Rdata
# 2. save dyadic indices data in list columns for 

# 1. Grooming indices ----
# ---- load grooming minutes indices - created by ZPM ----
gm_min <- read_xlsx("data/Grooming Index by minutes_ZPM.xlsx") %>%
  rename(year = Year, ID1 = Chimp_A, ID2 = Chimp_B, gmgmdi = `All Grooming Index_%`, gmi = `Grooming Given Index_ %`,
         total_AB_gm = `Total A to B`, total_AB_gm_gmd = All_grooming_AB, AB_party_min = Abfocal_Abpresent_minutes)

names(gm_min)

# directed (doesn't need filter)
total_gm1 <- gm_min %>%
  select(year, ID1, ID2, total_AB_gm, AB_party_min, gmi)

# undirected (filter unique dyads)
total_gm_gmd1 <- gm_min %>%
  select(year, ID1, ID2, total_AB_gm_gmd, AB_party_min, gmgmdi)

# check calcs
# total_gm1 %>% mutate(new_index = ifelse(!is.na(100*(total_AB_gm/AB_party_min)), 100*(total_AB_gm/AB_party_min), 0), test = gmi == new_index) %>%
#   filter(gmi != new_index)
# total_gm_gmd1 %>% mutate(new_index = ifelse(!is.na(100*(total_AB_gm_gmd/AB_party_min)), 100*(total_AB_gm_gmd/AB_party_min), 0), test = gmgmdi == new_index) %>%
#   filter(gmgmdi != new_index)

# ---- filter undirected and add attributes -----
# readd IDs in alphabetical order to then remove duplicate dyads in undirected data
IDs <- total_gm_gmd1 %>%
  select(ID1, ID2) %>%
  apply(., 1, sort) %>% #alphabetize sort in each row
  t(.) %>%
  data.frame(., stringsAsFactors = F) %>%
  rename(ID1 = X1, ID2 = X2) # because undirected

total_gm_gmd2 <- total_gm_gmd1 %>%
  select(-ID1, -ID2) %>%
  cbind(IDs, .) %>%
  distinct(ID1, ID2, year, .keep_all = T) %>% #remove dup dyads, revealed after IDs were alphabetized 
  mutate(gmgmdi = if_else(is.na(gmgmdi), 0, gmgmdi)) %>%
  add_dyad_attr() %>%   # add their attributes, sex, dobc...
  add_age() %>% # and ages
  mutate(dyad_sex = ifelse(sex_ID1 == "M" & sex_ID2 == "M", "male", ifelse( sex_ID1 == "F" & sex_ID2 == "F", "female", "mixed" ))) 
  #filter_age() %>% # ZPM created data files with annual subjects from original groom scan data, already filtered along these lines.
  #clean_ghosts() %>% #sex specific age filter 
  #mark_short_time_pres(filter_n_clean = TRUE) 

total_gm2 <- total_gm1 %>%
  add_dyad_attr() %>%   # add their attributes, sex, dobc...
  add_age() %>% # and ages
  mutate(dyad_sex = ifelse(sex_ID1 == "M" & sex_ID2 == "M", "male", ifelse( sex_ID1 == "F" & sex_ID2 == "F", "female", "mixed" ))) 
#filter_age() %>% # ZPM created data files with annual subjects from original groom scan data, already filtered along these lines.
#clean_ghosts() %>% #sex specific age filter 
#mark_short_time_pres(filter_n_clean = TRUE) 

# final index dataframes 
total_gm_gmd_index <- total_gm_gmd2 %>%
  select(year, ID1, ID2, total_AB_gm_gmd, AB_party_min, gmgmdi, sex_ID1, sex_ID2, dyad_sex)
total_gm_index <- total_gm2 %>%
  select(year, ID1, ID2, total_AB_gm, AB_party_min, gmi, sex_ID1, sex_ID2, dyad_sex)

total_gm_gmd_index %>%
  filter(apply(., 1, function(x) any(is.na(x))))

total_gm_index %>%
  filter(apply(., 1, function(x) any(is.na(x))))

#check number of obs/rows match with previous groom scan data
# load("data/indices - annual dyadic grooming scan.Rdata", verbose = T)
# nrow(total_gm2) # 4824
# nrow(total_gm_index) # 4824
# anti_join(total_gm_index, total_gm2, by = c("ID1", "ID2")) %>% View() # empty set
# 
# nrow(total_gm_gmd2) #2412
# nrow(total_gm_gmd_index) #2412
# anti_join(total_gm_gmd_index, total_gm_gmd2, by = c("ID1", "ID2")) %>% View() # empty set


# ultra skewed chimp year bc low party membership
row_id1 <- total_gm_gmd_index$year == 2012  & total_gm_gmd_index$ID1 == "RD" & total_gm_gmd_index$ID2 == "YB"
total_gm_gmd_index[row_id1, "gmgmdi"] <- 0

row_id2 <- total_gm_index$year == 2012  & total_gm_index$ID1 == "RD" & total_gm_index$ID2 == "YB"
total_gm_index[row_id2, "gmi"] <- 0

#save(total_gm_gmd_index, total_gm_index, file = "data/indices - annual dyadic grooming min.Rdata")


# # exploring time in same party cut off, just settle on single RD YB year ----
# total_gm_gmd2 %>% 
#   #filter(gmgmdi < 30) %>%
#   ggplot(aes(x = AB_party_min, y = gmgmdi)) +
#   geom_jitter() +
#   geom_smooth()
# 
# total_gm2 %>% 
#   filter(gmi < 30) %>%
#   ggplot(aes(x = AB_party_min, y = gmi)) +
#   geom_jitter() +
#   geom_smooth()
# 
# # the highest indices, one bc of low party time, RD YB 2012
# total_gm_gmd2 %>% 
#   filter(gmgmdi > 10)
# 
# # only one hi gm given index inflated bc low shared party time, RD YB 2012
# total_gm2 %>% 
#   filter(gmi > 6)
# total_gm2 %>% 
#   filter(ID1 == "MS" & ID2 == "YB")
# total_gm2 %>% 
#   filter(ID2 == "MS" & ID1 == "YB")
# 
# # hist of minutes observed
# total_gm_gmd2 %>%
#   pull(AB_party_min) %>% hist(breaks = 100, axes = FALSE)
# axis(1, at = seq(0,20000, 500))
# 
# total_gm_gmd2 %>% 
#   filter(AB_party_min > 0 & AB_party_min < 1000) %>% 
#   pull(AB_party_min) %>% hist(breaks = 50)
# axis(1, at = seq(0,1000, 50))




# 2. Prox indices ----
# ----- Assemble time in 5m index #####
load("data/counts - dyadic focal party.Rdata", verbose = T)
load("data/counts - time in 5m.Rdata", verbose = T)

names(total_5m)
nrow(total_5m) #2412, 2597, 2936
names(total_AB_party)


#11/25/20 did not multiply prox index by 100 in original go
index_5m <- total_5m %>%
  left_join(., total_AB_party, by = c("ID1", "ID2", "year")) %>%
  mutate(total_AB_party = ifelse(is.na(total_AB_party), 0 , total_AB_party)) %>% #NAs of total AB party are dyads never seen in groups
  mutate(prox5i = ifelse(total_AB_party == 0, 0, (total_5m/total_AB_party)*100)) %>% # if total AB party is zero, avoid NaN of 0/0
  select(ID1, ID2, year, total_5m, total_AB_party, prox5i, everything()) %>%
  mutate(dyad_sex = ifelse(sex_ID1 == "M" & sex_ID2 == "M", "male", ifelse( sex_ID1 == "F" & sex_ID2 == "F", "female", "mixed" )))
  
range(index_5m$prox5i)

nrow(index_5m) #2412, 2597, 2679, 2936
names(index_5m)

index_5m %>%
  ggplot(aes(x = total_AB_party, y = prox5i)) +
  geom_jitter() +
  geom_smooth()


proxrow1 <- index_5m$ID1 == "AL" & index_5m$ID2 == "LN" & index_5m$year == 2010
proxrow2 <- index_5m$ID1 == "ES" & index_5m$ID2 == "LN" & index_5m$year == 2012
index_5m[proxrow1 | proxrow2, "prox5i"] <- c(0,0)

#save(index_5m, file = "data/indices - annual dyadic 5m proximity.Rdata")
# resaved 8/22 after removing two strongly biased obs above.

# 3. Save dyadic indices data as list columns for purrr -------
source("functions/functions - sna measures and plot.R")
load("data/indices - annual dyadic grooming min.Rdata", verbose = T)
load("data/indices - annual dyadic 5m proximity.Rdata", verbose = T)
names(index_5m)
names(total_gm_index)

# write.csv(total_gm_gmd_index, file = "data/annual total grooming indices min.csv", row.names = F)
# write.csv(total_gm_index, file = "data/annual directed grooming indices min.csv", row.names = F)
# write.csv(index_5m, file = "data/annual 5m prox indices.csv", row.names = F)


names(total_gm_gmd_index)
names(index_5m)


# where dyad ids and their indices are separate dataframes for each year year and dyad_sex
g_data_gmgmd_sex_sep <- total_gm_gmd_index %>%
  select(year, dyad_sex, ID1,ID2, gmgmdi) %>%
  filter(dyad_sex != "mixed") %>%
  # nest all dyad ids and indices within year and dyad sex
  nest(data = c(ID1,ID2, gmgmdi)) %>% #data = c(ID1,ID2, gmgmdi) # <- in windows
  arrange(dyad_sex, year) 

g_data_gm_sex_sep <- total_gm_index %>%
  select(year, dyad_sex, ID1,ID2, gmi) %>%
  filter(dyad_sex != "mixed") %>%
  # nest all dyad ids and indices within year and dyad sex
  nest(data = c(ID1,ID2, gmi)) %>% #data = c(ID1,ID2, gmgmdi) # <- in windows
  arrange(dyad_sex, year) 

g_data_prox_sex_sep <- index_5m %>%
  select(year, dyad_sex, ID1,ID2, prox5i) %>%
  filter(dyad_sex != "mixed") %>%
  # nest all dyad ids and indices within year and dyad sex
  nest(data = c(ID1,ID2, prox5i)) %>%
  arrange(dyad_sex, year) 


g_data_gmgmd_sex_comb <- total_gm_gmd_index %>%
  mutate(dyad_sex = "any_combo") %>%
  select(year, dyad_sex, ID1,ID2, gmgmdi) %>%
  # nest all dyad ids and indices within year and dyad sex
  nest(data = c(ID1,ID2, gmgmdi)) %>%
  arrange(year)

g_data_gm_sex_comb <- total_gm_index %>%
  mutate(dyad_sex = "any_combo") %>%
  select(year, dyad_sex, ID1,ID2, gmi) %>%
  # nest all dyad ids and indices within year and dyad sex
  nest(data = c(ID1,ID2, gmi)) %>%
  arrange(year)

g_data_prox_sex_comb <- index_5m %>%
  mutate(dyad_sex = "any_combo") %>%
  select(year, dyad_sex, ID1,ID2, prox5i) %>%
  # nest all dyad ids and indices within year and dyad sex
  nest(data = c(ID1,ID2, prox5i)) %>%
  arrange(year)


# save(g_data_gmgmd_sex_comb,
#     g_data_gm_sex_comb,
#      g_data_prox_sex_comb,
#     g_data_gmgmd_sex_sep,
#      g_data_gm_sex_sep,
#      g_data_prox_sex_sep, file = "data/list column dyadic data prox & gm by year & dyad-sex year.Rdata")

#load("data/list column dyadic data prox & gm by year & dyad-sex year.Rdata", verbose = T)

# graveyard ----
# compare grooming minutes to grooming scan ----
load("data/indices - annual dyadic grooming min.Rdata", verbose = T)
load("data/indices - annual dyadic grooming scan.Rdata", verbose = T)

par(mfrow = c(1,2))
hist(total_gm_gmd_index$gmgmdi)
hist(total_gm_gmd_index_scan$gmgmdi)

index_diff <- total_gm_gmd_index$gmgmdi - total_gm_gmd_index_scan$gmgmdi

mean(index_diff > 0) # prop new index bigger
mean(index_diff < 0) # prop new index smaller
mean(index_diff == 0) # proportion no diff

total_gm_gmd_index$gmgmdi

comp_gm_gmd <- full_join(total_gm_gmd_index, total_gm_gmd_index_scan, by = c("year", "ID1", "ID2")) %>%
  mutate(index_diff = gmgmdi.x -  gmgmdi.y) %>%
  select(year, ID1, ID2, gmgmdi.x,gmgmdi.y, index_diff, total_AB_gm_gmd.x, AB_party_min, total_AB_gm_gmd.y, total_AB_party)
View(comp_gm_gmd)

sum(comp_gm_gmd$gmgmdi.x == 0) #1239
sum(comp_gm_gmd$gmgmdi.y == 0) #1619, 380 fewer 0's in new indices

comp_gm_gmd %>%
  filter(gmgmdi.y == 0 & gmgmdi.x > 0) %>%
  View()

comp_gm_gmd %>%
  filter(gmgmdi.x == 0 & gmgmdi.y > 0) %>%
  select(year, ID1, ID2)



# ----- OLD GROOM SCAN DATA - moved 8/15 - Assemble grooming indices #####
load("data/counts - annual dyadic grooming.Rdata", verbose = T)
load("data/counts - dyadic focal party.Rdata", verbose = T)

#total grooming index - AB gm_gmd / AB total time in party where one was focal

total_gm_gmd %>% nrow() #2412
total_AB_party %>% names()
str(total_gm_gmd)
str(total_AB_party)

total_gm_gmd_index_scan <- total_gm_gmd %>%
  #join count data w total number of times individuals A and B were in same party as one was being focaled
  left_join(., total_AB_party, by = c("ID1", "ID2", "year")) %>% 
  #replace NAs for dyads never observed in same party during focal follow
  mutate_at(vars(ends_with("_party")), .funs = list(function(x) ifelse(is.na(x), 0, x))) %>%
  mutate(gmgmdi = (total_AB_gm_gmd / total_AB_party) * 100) %>% # gm gmd index, percentage of time in party spent grooming each other
  mutate(gmgmdi = ifelse(is.nan(gmgmdi), 0, gmgmdi)) %>% # for 0/0
  select(ID1, ID2, year, total_AB_gm_gmd, total_AB_party, gmgmdi, sex_ID1, sex_ID2, everything()) %>%
  mutate(dyad_sex = ifelse(sex_ID1 == "M" & sex_ID2 == "M", "male", ifelse( sex_ID1 == "F" & sex_ID2 == "F", "female", "mixed" )))
nrow(total_gm_gmd_index_scan) # 2412, 2585, 2657, 2914
head(total_gm_gmd_index_scan)

total_gm_index_scan <- total_gm %>%
  filter(ID1 != "CA" & ID2 != "CA") %>% #never really acheived community member status
  left_join(., total_AB_party, by = c("ID1", "ID2", "year")) %>%
  #replace NAs for dyads never observed in party during focal follow
  mutate_at(vars(ends_with("_party")), .funs = list(function(x) ifelse(is.na(x), 0, x))) %>%
  mutate(gmi = (total_AB_gm / total_AB_party) * 100) %>% # gm gmd index, percentage of time in party spent grooming each other
  mutate(gmi = ifelse(is.nan(gmi), 0, gmi)) %>% #for 0/0
  select(ID1, ID2, year, total_AB_gm, total_AB_party, gmi, sex_ID1, sex_ID2) %>%
  mutate(dyad_sex = ifelse(sex_ID1 == "M" & sex_ID2 == "M", "male", ifelse( sex_ID1 == "F" & sex_ID2 == "F", "female", "mixed" )))
nrow(total_gm_index) # 4824, 5168, 5312,5826

total_gmd_index_scan <- total_gmd %>%
  filter(ID1 != "CA" & ID2 != "CA") %>% #never really acheived community member status
  merge(., total_AB_party, by = c("ID1", "ID2", "year"), all.x = T) %>%
  #replace NAs for dyads never observed in party during focal follow
  mutate_at(vars(ends_with("_party")), .funs = list(function(x) ifelse(is.na(x), 0, x))) %>%
  mutate(gmdi = (total_AB_gmd / total_AB_party) * 100) %>% # gm gmd index, percentage time in party spent grooming each other
  mutate(gmdi = ifelse(is.nan(gmdi), 0, gmdi)) %>% #for 0/0
  select(ID1, ID2, year, total_AB_gmd, total_AB_party, gmdi, sex_ID1, sex_ID2) %>%
  mutate(dyad_sex = ifelse(sex_ID1 == "M" & sex_ID2 == "M", "male", ifelse( sex_ID1 == "F" & sex_ID2 == "F", "female", "mixed" )))
nrow(total_gmd_index_scan) #4824, 5168, 5312, 5826
head(total_gmd_index_scan)

total_gm_gmd_index_scan %>%
  filter(apply(., 1, function(x) any(is.na(x))))

total_gm_index_scan %>%
  filter(apply(., 1, function(x) any(is.na(x))))

total_gmd_index_scan %>%
  filter(apply(., 1, function(x) any(is.na(x))))

#of 2412 total dyads 26 never observed within same party during a focal follow in ~ 10 yrs
nrow(total_gm_gmd)
total_gm_gmd %>%
  left_join(., total_AB_party, by = c("ID1", "ID2", "year")) %>% 
  filter(is.na(total_AB_party))


#save(total_gm_gmd_index_scan, total_gm_index_scan, total_gmd_index_scan, file = "data/indices - annual dyadic grooming scan.Rdata")

# ----- Explore annual grooming indices ####
load("data/annual possible focal dyads.Rdata", verbose = T)
load("data/indices - annual dyadic grooming.Rdata", verbose = T)

source("functions/functions - data preparation.R", verbose = T)
nrow(total_gm_gmd_index) #2412

# unique female and male dyads by year
# no age filter
dir_annual_dyads %>%
  rename(Year = year) %>%
  add_dyad_attr() %>%
  filter(sex_ID1 == sex_ID2) %>%
  group_by(Year, sex_ID1) %>%
  tally()

# unique female and male dyads by year
# with age filter
dir_annual_dyads %>%
  add_dyad_attr() %>%
  add_age() %>%
  filter_age() %>%
  filter(sex_ID1 == sex_ID2) %>%
  group_by(year, sex_ID1) %>%
  tally()


#range
total_gm_gmd_index %>%
  group_by(year) %>%
  summarise(max = max(gmgmdi, na.rm = T), min = min(gmgmdi, na.rm = T),  
            median = median(gmgmdi, na.rm = T), mean = mean(gmgmdi, na.rm = T),
            sd = sd(gmgmdi, na.rm = T))

total_gm_gmd_index %>%
  filter(gmgmdi > 45)



# compare old and new indices after gm recoded ------ 
# 7.2.21 
load("data/indices - annual dyadic grooming.Rdata", verbose = T)
load("data/indices - old annual dyadic grooming.Rdata", verbose = T)


# old_total_gm_gmd_index <- total_gm_gmd_index
# old_total_gm_index <- total_gm_index
# old_total_gmd_index <- total_gmd_index
# save( old_total_gm_gmd_index, old_total_gm_index, old_total_gmd_index, file = "data/indices - old annual dyadic grooming.Rdata")

# new data has more grooming dyads and larger range of gm indices, as should be now that more grooming interactions counted
old_total_gm_index %>%
  filter(dyad_sex == "mixed") %>%
  pull(gmi) %>%
  hist()
total_gm_index %>%
  filter(dyad_sex == "mixed") %>%
  pull(gmi) %>%
  hist()

old_total_gm_index %>%
  filter(dyad_sex == "female") %>%
  pull(gmi) %>%
  hist()
total_gm_index %>%
  filter(dyad_sex == "female") %>%
  pull(gmi) %>%
  hist()

g_data_gm_sex_comb_new <- total_gm_index %>%
  mutate(dyad_sex = "any_combo") %>%
  select(year, dyad_sex, ID1,ID2, gmi) %>%
  # nest all dyad ids and indices within year and dyad sex
  nest(data = c(ID1,ID2, gmi)) %>%
  arrange(year)

g_data_gm_sex_comb_old <- old_total_gm_index %>%
  mutate(dyad_sex = "any_combo") %>%
  select(year, dyad_sex, ID1,ID2, gmi) %>%
  # nest all dyad ids and indices within year and dyad sex
  nest(data = c(ID1,ID2, gmi)) %>%
  arrange(year)

# data in list cols as should be too
# higher values present in new gm data, and more dyads with low gm indices
hist(g_data_gm_sex_comb_old$data[[1]]$gmi)
hist(g_data_gm_sex_comb_new$data[[1]]$gmi) 

# ----- Sex specific networks of indices ####

fem_gmgmdi <- total_gm_gmd_index %>%
  filter(sex_ID1 == "F", sex_ID2 == "F")
nrow(fem_gmgmdi) #1054
head(fem_gmgmdi)

male_gmgmdi <- total_gm_gmd_index %>%
  filter(sex_ID1 == "M", sex_ID2 == "M")
nrow(male_gmgmdi) #421
head(male_gmgmdi)

fem_gmi <- total_gm_index %>%
  filter(sex_ID1 == "F", sex_ID2 == "F")
nrow(fem_gmi) #2108 rows w non-grooming dyads included, 401 w out
head(fem_gmi)

male_gmi <- total_gm_index %>%
  filter(sex_ID1 == "M", sex_ID2 == "M")
nrow(male_gmi) #842 rows w non-grooming dyads included
head(male_gmi)

fem_gmdi <- total_gmd_index %>%
  filter(sex_ID1 == "F", sex_ID2 == "F")
nrow(fem_gmdi) #2108 rows w non-grooming dyads included, 84 w out
head(fem_gmdi)

male_gmi %>%
  filter(total_AB_gm != 0) %>% nrow() 

male_gmdi <- total_gmd_index %>%
  filter(sex_ID1 == "M", sex_ID2 == "M")
nrow(male_gmdi) #782 rows w non-grooming dyads included,405 without
head(male_gmdi)

# ----- Sex specific 5m ####
female_prox5i <- index_5m %>%
  filter(sex_ID1 == "F", sex_ID2 == "F")

male_prox5i <- index_5m %>%
  filter(sex_ID1 == "M", sex_ID2 == "M")

nrow(female_prox5i) #1062
nrow(male_prox5i) #421


index_5m$dyad_sex <- "any_combo"
female_prox5i$dyad_sex <- "female"
male_prox5i$dyad_sex <- "male"
# --- play with same sex thresholds for prox networks -----
prox_sex_thresh <- index_5m %>%
  select(year, dyad_sex, ID1,ID2, prox5i) %>%
  filter(dyad_sex != "mixed") %>%
  arrange(dyad_sex, year)%>%
  group_by(dyad_sex,year) %>%
  summarize(mean_prox = mean(prox5i), sd_prox = sd(prox5i)) %>%
  ungroup()
prox_sex_thresh

#save(prox_sex_thresh, file="data/annual thresholds for same sex prox nets.Rdata")


# --- play w filter same sex prox network ------
# load("data/annual thresholds for same sex prox nets.Rdata", verbose = T)

see_prox_filter <- index_5m %>%
  left_join(., prox_sex_thresh, by = c("dyad_sex", "year")) %>%
  mutate(prox5i = case_when(
    dyad_sex != "mixed" & prox5i < mean_prox ~ 100, #(mean_prox - sd_prox) - ??? why 100?
    TRUE ~ prox5i
  ))


# for filtering, < mean prox = 0 is seems way too harsh - removes 2/3s of associations bt females and 1/2 of males'
# and < mean_prox - sd not harsh enough, removes no females.
# best to try calculating betweenness with weights. 3/4/20
index_5m %>% count(dyad_sex)
see_prox_filter %>% filter(prox5i== 100) %>% count(dyad_sex)


index_5m_filtered <- index_5m %>%
  left_join(., prox_sex_thresh, by = c("dyad_sex", "year")) %>%
  mutate(prox5i = case_when(
    dyad_sex != "mixed" & prox5i < mean_prox ~ 0,
    TRUE ~ prox5i
  ))


