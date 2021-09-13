# Create functions - adding sexes & ages to df, apply sex specific filter ages, fix ID errors

#1 - add_dyad_attr
#2 - add_individ_attr
#3 - add_age 
#4 - mark_short_time_pres
#5 - mark_short_time_pres_individ 
#6 - fix_ID_errors 
#7 - clean_ghosts
#8 - add_years_obs
#9 - add_final_age

# add dyad member sexes and birthdates
add_dyad_attr <- function(df, ID1 = "ID1", ID2 = "ID2", ...){
  load("data/attribute data alone.Rdata")
  
  names(df)[names(df) == ID1] <- "ID1"
  names(df)[names(df) == ID2] <- "ID2"  
  
  a <- df %>%
    left_join(., attr %>% select(chimp_id, sex, dobc, dls, year_last_seen, starts_with("immig"), ...), by = c("ID1" = "chimp_id")) %>%
    left_join(., attr %>% select(chimp_id, sex, dobc, dls, year_last_seen, starts_with("immig"),...), by = c("ID2" = "chimp_id")) %>%
    rename_at(vars(contains(".x")), list( ~ sub(".x$", "_ID1", .))) %>%
    rename_at(vars(contains(".y")), list( ~ sub(".y$", "_ID2", .)))
  return(a)
}

# add individual attribute
add_individ_attr <- function(df, ID1 = "ID1", ...){
  load("data/attribute data alone.Rdata")
  names(df)[names(df) == ID1] <- "ID1"
  #ID name not always the same
  #ID arg says, whatever is the ID name in the original DF, let's change it to "ID1" to consistently merge on it
  #leave ID as chimp_id bc when undirected, bc "chimp_id" is a good var name
  
    a <- df %>%
      left_join(., attr %>% select(chimp_id, sex, dobc, dls, year_last_seen, starts_with("immig"), ...), by = c(ID1 ="chimp_id"))  
    
  return(a)
}


# add annual attributes
add_individ_ann_attr <- function(df, ID = "chimp_id", year = "year"){
  #adds annual chimp-year attribute data, e.g. rank and prop estrous
  load("data/annual average standardized ranks.Rdata")
  load("data/female prop annual cycling.Rdata")
  
  names(df)[names(df) == ID] <- "chimp_id"
  names(df)[names(df) == year] <- "year"

  a <- df %>%
    left_join(., ann_ranks,  by = c("year", chimp_id = "ID")) %>%
    left_join(., prop_cyc, by = c("year", "chimp_id"))
  
  return(a)
}




#create ages on july 1 of observation year
add_age <- function(df, dyad = TRUE) {
  
  if(dyad == TRUE){
    b <- df %>%
      mutate(mid_year = as.Date(paste0(year,"-07-01")), 
             age_mid_year_ID1 =  as.numeric(mid_year - dobc_ID1)/365.25,
             age_mid_year_ID2 =  as.numeric(mid_year - dobc_ID2)/365.25) %>%
      select(-mid_year)
    return(b)  
  }
  
  if(dyad == FALSE){
    b <- df %>%
      mutate(mid_year = as.Date(paste0(year,"-07-01")), 
             age_mid_year =  as.numeric(mid_year - dobc)/365.25) %>%
      select(-mid_year)
    return(b)  
  }
  
}

#sex specific age filter
filter_age <- function(df, Age_F = 12, Age_M = 15, dyad = TRUE) {
  
  if(dyad == FALSE){
  f <- df %>%
    filter((sex == "F" & age_mid_year >= Age_F) | (sex == "M" & age_mid_year >= Age_M))
  }
  
  if(dyad == TRUE){
    f <- df %>%
      filter( ((sex_ID1 == "F" & age_mid_year_ID1 >= Age_F) & (sex_ID2 == "F" & age_mid_year_ID2 >= Age_F)) | #FF dyad
                ((sex_ID1 == "M" & age_mid_year_ID1 >= Age_M) & (sex_ID2 == "M" & age_mid_year_ID2 >= Age_M)) | #MM dyad
                ((sex_ID1 == "F" & age_mid_year_ID1 >= Age_F) & (sex_ID2 == "M" & age_mid_year_ID2 >= Age_M)) | #FM dyad
                ((sex_ID1 == "M" & age_mid_year_ID1 >= Age_M) & (sex_ID2 == "F" & age_mid_year_ID2 >= Age_F))) # MF dyad
  }
  return(f)
}

# mark what individuals are present for < X number of weeks in year
#load gm df for testing
#load(data/counts - annual dyadic grooming.Rdata", verbose = T)
#df <- total_gm_gmd

mark_short_time_pres <- function(df, year = year, wks_of_yr_cutoff = 26, filter_n_clean = FALSE) {

    t <- df %>%
    #create year start and end
    mutate(year_start = as.Date(paste0(year,"-01-01")),
           year_end = as.Date(paste0(year,"-12-31"))) %>%
      #temporary calc of weeks present from start of year
      mutate(temp_wks_pres_start_ID1 = as.numeric(difftime(dls_ID1, year_start, units = "weeks"))) %>%
      mutate(temp_wks_pres_start_ID2 = as.numeric(difftime(dls_ID2, year_start, units = "weeks"))) %>%
      #temporary calc of weeks present from end of year
      mutate(temp_wks_pres_end_ID1 = as.numeric(difftime(year_end, immig_date_ID1, units = "weeks"))) %>%
      mutate(temp_wks_pres_end_ID2 = as.numeric(difftime(year_end, immig_date_ID2, units = "weeks"))) %>%
    #weeks present for ID1
    mutate(weeks_pres_ID1 = case_when(
      #if year of immigration is this year then wks present is difftime immig date to end of year
      immig_year_ID1 == year ~ temp_wks_pres_end_ID1,
      #if date last seen is NA then present 52 weeks, baseline
      is.na(dls_ID1) ~ 52,
      #if date last seen is in other year then present 52 weeks
      (!is.na(dls_ID1) & (year_last_seen_ID1 != year)) ~ 52,
      #if date last seen is in this year then present for difftime from start to date last seen
      (!is.na(dls_ID1) & (year_last_seen_ID1 == year)) ~ temp_wks_pres_start_ID1)) %>%
    #weeks present for ID1
    mutate(weeks_pres_ID2 = case_when(
        #if year of immigration is this year then wks present is difftime immig date to end of year
        immig_year_ID2 == year ~ temp_wks_pres_end_ID2,
        #if date last seen is NA then present 52 weeks, baseline
        is.na(dls_ID2) ~ 52,
        #if date last seen is in other year then present 52 weeks
        (!is.na(dls_ID2) & (year_last_seen_ID2 != year)) ~ 52,
        #if date last seen is in this year then present for difftime from start to date last seen
        (!is.na(dls_ID2) & (year_last_seen_ID2 == year)) ~ temp_wks_pres_start_ID2)) %>%
    #is individ pres < half of year? then mark for removal
    mutate(short_presence_ID1 = ifelse(weeks_pres_ID1 < wks_of_yr_cutoff, 1, 0)) %>%
    mutate(short_presence_ID2 = ifelse(weeks_pres_ID2 < wks_of_yr_cutoff, 1, 0)) %>%
    select(-year_start, -year_end, -starts_with("temp"))
  
    if(filter_n_clean == TRUE){
      t <- t %>%
        filter_at(vars(starts_with("short")), all_vars(. == 0)) %>%
        select(ID1, ID2, year, sex_ID1, sex_ID2, starts_with("n_"), starts_with("total"), starts_with("age"), starts_with("weeks"))
    }
    
    
  return(t)
    
}

# mark short time pres individual
mark_short_time_pres_individ <- function(df, year = year, wks_of_yr_cutoff = 26, filter_n_clean = FALSE) {
  
  t <- df %>%
    #create year start and end
    mutate(year_start = as.Date(paste0(year,"-01-01")),
           year_end = as.Date(paste0(year,"-12-31"))) %>%
    #temporary calc of weeks present from start of year
    mutate(temp_wks_pres_start = as.numeric(difftime(dls, year_start, units = "weeks"))) %>%
    #temporary calc of weeks present from end of year
    mutate(temp_wks_pres_end = as.numeric(difftime(year_end, immig_date, units = "weeks"))) %>%
    #weeks present
    mutate(weeks_pres = case_when(
      #if year of immigration is this year then wks present is difftime immig date to end of year
      immig_year == year ~ temp_wks_pres_end,
      #if date last seen is NA then present 52 weeks, baseline
      is.na(dls) ~ 52,
      #if date last seen is in other year then present 52 weeks
      (!is.na(dls) & (year_last_seen != year)) ~ 52,
      #if date last seen is in this year then present for difftime from start to date last seen
      (!is.na(dls) & (year_last_seen == year)) ~ temp_wks_pres_start)) %>%
    #is individ pres < half of year? then mark for removal
    mutate(short_presence = ifelse(weeks_pres < wks_of_yr_cutoff, 1, 0)) %>%
    select(-year_start, -year_end, -starts_with("temp"))
  
  if(filter_n_clean == TRUE){
    t <- t %>%
      filter_at(vars(starts_with("short")), all_vars(. == 0)) %>%
      select(ID1, year, sex, starts_with("n_"), starts_with("total"), starts_with("age"), starts_with("weeks"))
  }
  
  
  return(t)
  
}

# remove spaces from any IDs
fix_ID_errors <- function(df, ID1 = "ID1", ID2 = "ID2"){
  
  names(df)[names(df) == ID1] <- "ID1"
  names(df)[names(df) == ID2] <- "ID2" 
  
  df_fixed <- df
  
  loc_ID1 <- grepl(" ", df$ID1) #locations of mistyped codes in ID1
  ID1_whack <- df[loc_ID1, "ID1"] 
  ID1_fixed <- gsub(" ", "", ID1_whack)
  df_fixed[loc_ID1, "ID1"] <- ID1_fixed
  
  loc_ID2 <- grepl(" ", df$ID2) #locations of mistyped codes in ID2
  ID2_whack <- df[loc_ID2, "ID2"]
  ID2_fixed <- gsub(" ", "", ID2_whack)
  df_fixed[loc_ID2, "ID2"] <- ID2_fixed
  
  return(df_fixed)
  
}

# remove individuals from analysis
clean_ghosts <- function(df){
total_ghosts_n_non_members <- c("NL", "KL", "OK", "PE", "CA", "GO", "HL", "HH") # CA GO HL HH non-members
  f <- df %>%  
    filter(!(ID1 %in% total_ghosts_n_non_members) & !(ID2 %in% total_ghosts_n_non_members)) %>% # turn this into data prep function eventually?
    filter(!(year > 2013 & ID1 == "ST") & !(year > 2013 & ID2 == "ST")) %>% # mis-ID'd after death
    filter(!(year > 2013 & ID1 == "PG") & !(year > 2013 & ID2 == "PG")) %>% # mis-ID'd after death
    filter(!(year == 2011 & ID1 == "GS") & !(year == 2011 & ID2 == "GS")) # lo focal and lo party membership
  # see lo individs load("data/subject years to remove for low observations (focal and party membership).Rdata")
return(f)
    }


# add years observed
add_years_obs <- function(df) {
load("data/years each subject pres in network data.Rdata")
  
years_pres %<>% rename(yrs_obs = n)
 y <- df %>%
  left_join(years_pres, by = "chimp_id")
 
 return(y) 
 
 }

# add final age - 2018 or age last seen
add_final_age <- function(df) {
  f <- df %>%
    mutate(final_age = ifelse(is.na(dls),
                              as.numeric(as.Date("2018-07-01") - dobc)/365.25,
                              as.numeric(dls - dobc)/365.25))
  return(f)
  }

# filter same sex prox networks by sex specific annual average index

#filter_sex_prox <-
#  load("data/annual thresholds for same sex prox nets.Rdata", verbose = T)

#total_5m
  

#save(add_dyad_attr, add_individ_attr, add_age, filter_age, mark_short_time_pres, fix_ID_errors, clean_ghosts,
# add_years_obs, add_final_age, file = "functions/functions - data preparation.Rdata")
