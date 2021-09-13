
# function for assessing sex age and rank effects in mixed sex networks
test_sig_gamm_mixed <- function(response, net_sex = "both", behavior, mod, modr, F_ran, F_ran_r, beta_ran, beta_ran_r){
  
  # Observed models, without and with rank as predictor
  #mod
  #modr   
  #Fs from randomized models, without and with rank as predictor
  #F_ran
  #F_ran_r
  source("functions/functions - prettify results table.R")
  
  # pull random Fs
  Fr_fa <- F_ran %>% filter(rowname == "s(age_mid_year):sexF") %>% pull(F) # F female age
  Fr_far <- F_ran_r %>% filter(rowname == "s(age_mid_year):sexF") %>% pull(F) # F female age w rank
  
  Fr_ma <- F_ran %>% filter(rowname == "s(age_mid_year):sexM") %>% pull(F) # F male age
  Fr_mar <- F_ran_r %>% filter(rowname == "s(age_mid_year):sexM") %>% pull(F) # F male age w rank
  
  Fr_mr <- F_ran_r %>% filter(rowname == "s(avg_rank):sexM") %>% pull(F) # F male rank
  Fr_fr <- F_ran_r %>% filter(rowname == "s(avg_rank):sexF") %>% pull(F) # F female rank
  
  Fr_i <- F_ran %>% filter(rowname == "s(chimp_id)") %>% pull(F) # intercept model no rank
  Fr_ir <- F_ran_r %>% filter(rowname == "s(chimp_id)") %>% pull(F) # intercept model rank
  
  # pull observed F
  Fo <- summary(mod) %>% .$s.table %>% data.frame() %>% pull(F)
  Fo_r <- summary(modr) %>% .$s.table %>% data.frame() %>% pull(F)
  
  # compare proportion of random Fs that obs F is greater than
  fa <- sum(Fo[1] > Fr_fa)/length(Fr_fa) # female age
  ma <- sum(Fo[2] > Fr_ma)/length(Fr_ma) # male age
  
  far <- sum(Fo_r[1] > Fr_far)/length(Fr_far) # female age w rank
  mar <- sum(Fo_r[2] > Fr_mar)/length(Fr_mar) # male age w rank
  
  fr <- sum(Fo_r[3] > Fr_fr)/length(Fr_fr) # female rank
  mr <- sum(Fo_r[4] > Fr_mr)/length(Fr_mr) # male rank
  
  i <- sum(Fo[3] > Fr_i)/length(Fr_i) # intercept
  ir <- sum(Fo_r[5] > Fr_ir)/length(Fr_ir) # intercept rank model
  
  #obs_F <- c(Fo, Fo_r)
  
  # pull observed sex beta
  sexB <- summary(mod)$p.table %>% .[rownames(.) == "sexM", colnames(.) == "Estimate"] #%in% c("Estimate", "Std. Error")
  sexBr <- summary(modr)$p.table %>% .[rownames(.) == "sexM", colnames(.) == "Estimate"]
  
  # pull random sex betas
  sexB_ran <- beta_ran %>% data.frame() %>% pull(Estimate)
  sexB_ran_r <- beta_ran_r %>% data.frame() %>% pull(Estimate)
  
  # compare observed beta to random
  b <- sum(sexB > sexB_ran)/length(sexB_ran)
  br <- sum(sexBr > sexB_ran_r)/length(sexB_ran_r)
  
  # observed dev explained
  dev_expl <- summary(mod)$dev.expl
  dev_expl_r <- summary(modr)$dev.expl


  preds <- c("sex", "female age", "male age", "intercept")
  d_nr <- data.frame(sna_measure = response, sex = net_sex, beh = behavior, DE = dev_expl , pred = preds, F_smooths = c(NA, Fo), B_sex = sexB, F_prop_greater_than = c(NA, fa, ma, i), B_prop_greater_than = b, stringsAsFactors = F) %>%
    prettify_table() %>% 
    mutate(sig = ifelse((!is.na(F_prop_greater_than) & F_prop_greater_than >= 0.95) | (!is.na(B_prop_greater_than) & B_prop_greater_than >= 0.95), "*", ""))
    
  preds_r <- c("sex (with rank)", "female age (with rank)", "male age (with rank)", "female rank", "male rank", "intercept (with rank)")
  d_r <- data.frame(sna_measure = response, sex = net_sex, beh = behavior, DE = dev_expl_r, pred = preds_r, F_smooths = c(NA, Fo_r), B_sex = sexBr, F_prop_greater_than = c(NA, far, mar, fr, mr, ir), B_prop_greater_than = br, stringsAsFactors = F) %>%
  prettify_table() %>%
  mutate(sig = ifelse((!is.na(F_prop_greater_than) & F_prop_greater_than >= 0.95) | (!is.na(B_prop_greater_than) & B_prop_greater_than >= 0.95), "*", ""))
  
  d <- rbind(d_nr, d_r)
  return(d)
  
}


test_sig_gamm_f_estrus <- function(response, net_sex = "mixed-sex", behavior, mod, F_ran){
  
  # Observed models, without and with rank as predictor
  source("functions/functions - prettify results table.R")
  
  # pull random Fs (Fr)
  Fr_fa <- F_ran %>% filter(rowname == "s(age_mid_year)") %>% pull(F) # age
  Fr_fe <- F_ran %>% filter(rowname == "s(prop_cyc)") %>% pull(F) # estrus
  Fr_fae <- F_ran %>% filter(rowname == "ti(age_mid_year,prop_cyc)") %>% pull(F) # estrus n age
  Fr_i <- F_ran %>% filter(rowname == "s(chimp_id)") %>% pull(F) # intercept model no rank
  
  # pull observed F (Fo)
  Fo <- summary(mod) %>% .$s.table %>% data.frame() %>% pull(F)
  
  # compare proportion of random Fs that obs F is greater than
  fa <- sum(Fo[1] > Fr_fa)/length(Fr_fa) # female age
  fe <- sum(Fo[2] > Fr_fe)/length(Fr_fe) # female estrus
  fae <- sum(Fo[3] > Fr_fae)/length(Fr_fae) # female estrus n age
  fi <- sum(Fo[4] > Fr_i)/length(Fr_i) # female intercept
  
  # observed dev.expl
  dev_expl <- summary(mod)$dev.expl
  
  preds <- c("Age", "Time in estrus", "Age * Estrus" , "Intercept")
  d_e <- data.frame(sna_measure = response, sex = net_sex, beh = behavior, DE = dev_expl , pred = preds, F_smooths = Fo, F_prop_greater_than = c(fa, fe, fae, fi), stringsAsFactors = F) %>%
    prettify_table(include_beta_sex = FALSE, include_intercept = FALSE) %>% 
    mutate(sig = ifelse(F_prop_greater_than >= 0.95, "*", ""))
  
  return(d_e)
  
}


test_sig_gamm_f_estrus_rank <- function(response, net_sex = "mixed-sex", behavior, modr, F_ran_r){
  
  # Observed models, without and with rank as predictor
  source("functions/functions - prettify results table.R")
  
  # pull random Fs (Fr)
  Fr_far <- F_ran_r %>% filter(rowname == "s(age_mid_year)") %>% pull(F) # age
  Fr_fer <- F_ran_r %>% filter(rowname == "s(prop_cyc)") %>% pull(F) # estrus
  Fr_fr <- F_ran_r %>% filter(rowname == "s(avg_rank)") %>% pull(F)
  Fr_faer <- F_ran_r %>% filter(rowname == "ti(age_mid_year,prop_cyc)") %>% pull(F) # estrus n age
  Fr_ir <- F_ran_r %>% filter(rowname == "s(chimp_id)") %>% pull(F) # intercept model no rank
  
  # pull observed F (Fo)
  For <- summary(modr) %>% .$s.table %>% data.frame() %>% pull(F)
  
  #CHECK
  # compare proportion of random Fs that obs F is greater than
  far <- sum(For[1] > Fr_far)/length(Fr_far) # female age
  fr <- sum(For[2] > Fr_fr)/length(Fr_fr) # female rank
  fer <- sum(For[3] > Fr_fer)/length(Fr_fer) # female time swollen / prop cyc
  faer <- sum(For[4] > Fr_faer)/length(Fr_faer) # female prop cyc n age
  fir <- sum(For[5] > Fr_ir)/length(Fr_ir) # female intercept
  
  # observed dev.expl
  dev_expl <- summary(modr)$dev.expl
  
  predsr <- c("Age", "Rank", "Time swollen" , "Age * Swollen" , "Intercept")
  d_er <- data.frame(sna_measure = response, sex = net_sex, beh = behavior, DE = dev_expl , pred = predsr, F_smooths = For, F_prop_greater_than = c(far, fer, fr, faer, fir), stringsAsFactors = F) %>%
    prettify_table(include_beta_sex = FALSE, include_intercept = FALSE) %>% 
    mutate(sig = ifelse(F_prop_greater_than >= 0.95, "*", ""))
  
  return(d_er)
  
}

# same sex nets
test_sig_gamm_same <- function(response, net_sex = "same", behavior, fmod, fmodr, mmod, mmodr, F_ranf, F_ranf_r, F_ranm, F_ranm_r){
  
  # Observed models, without and with rank as predictor, female and male same sex networks
  #fmod
  #fmodr   
  #mmod
  #mmodr
  #Fs from randomized models, without and with rank as predictor
  #F_ranf # female same sex
  #F_ranf_r
  #F_ranm
  #F_ranm_r # male same sex
  source("functions/functions - prettify results table.R")
  
  # pull random Fs
  Fr_fsa <- F_ranf %>% filter(rowname == "s(age_mid_year)") %>% pull(F) # F female age
  Fr_fsar <- F_ranf_r %>% filter(rowname == "s(age_mid_year)") %>% pull(F) # F female age w rank
  
  Fr_msa <- F_ranm %>% filter(rowname == "s(age_mid_year)") %>% pull(F) # F male age
  Fr_msar <- F_ranm_r %>% filter(rowname == "s(age_mid_year)") %>% pull(F) # F male age w rank
  
  Fr_fsr <- F_ranf_r %>% filter(rowname == "s(avg_rank)") %>% pull(F) # F female rank
  Fr_msr <- F_ranm_r %>% filter(rowname == "s(avg_rank)") %>% pull(F) # F male rank
  
  Fr_fsi <- F_ranf %>% filter(rowname == "s(chimp_id)") %>% pull(F) # F female intercept
  Fr_fsir <- F_ranf_r %>% filter(rowname == "s(chimp_id)") %>% pull(F) # F female intercept w rank
  Fr_msi <- F_ranm %>% filter(rowname == "s(chimp_id)") %>% pull(F) # F male intercept
  Fr_msir <- F_ranm_r %>% filter(rowname == "s(chimp_id)") %>% pull(F) # F male intercept w rank
  
  
  # pull observed F
  Fof <- summary(fmod) %>% .$s.table %>% data.frame() %>% pull(F) #
  Fof_r <- summary(fmodr) %>% .$s.table %>% data.frame() %>% pull(F)
  
  Fom <- summary(mmod) %>% .$s.table %>% data.frame() %>% pull(F) #
  Fom_r <- summary(mmodr) %>% .$s.table %>% data.frame() %>% pull(F)
  
  # compare proportion of random Fs that obs F is greater than
  fa <- sum(Fof[1] > Fr_fsa)/length(Fr_fsa) # female age
  ma <- sum(Fom[1] > Fr_msa)/length(Fr_msa) # male age
  
  far <- sum(Fof_r[1] > Fr_fsar)/length(Fr_fsar) # female age w rank
  mar <- sum(Fom_r[1] > Fr_msar)/length(Fr_msar) # male age w rank
  
  fr <- sum(Fof_r[2] > Fr_fsr)/length(Fr_fsr) # female rank
  mr <- sum(Fom_r[2] > Fr_msr)/length(Fr_msr) # male rank
  
  fi <- sum(Fof[2] > Fr_fsi)/length(Fr_fsi) # intercept
  fir <- sum(Fof_r[3] > Fr_fsir)/length(Fr_fsir) # intercept rank model
  
  mi <- sum(Fom[2] > Fr_msi)/length(Fr_msi) # intercept
  mir <- sum(Fom_r[3] > Fr_msir)/length(Fr_msir) # intercept rank model
  
  
  # observed dev.expl
  fdev_expl <- summary(fmod)$dev.expl
  mdev_expl <- summary(mmod)$dev.expl
  fdev_explr <- summary(fmodr)$dev.expl
  mdev_explr <- summary(mmodr)$dev.expl
  
  preds <- c("female age", "intercept female", "male age",  "intercept male")
  d_nr <- data.frame(sna_measure = response, sex = net_sex, beh = behavior, DE = c(rep(fdev_expl, length(Fof)), rep(mdev_expl, length(Fom))) , pred = preds, F_smooths = c(Fof, Fom), F_prop_greater_than = c(fa, fi, ma, mi), stringsAsFactors = F) %>%
    prettify_table(include_beta_sex = FALSE) %>%
    mutate(sig = ifelse(F_prop_greater_than >= 0.95, "*", ""))
  
  preds_r <- c("female age (with rank)",
    "female rank",
    "intercept female (with rank)",
    "male age (with rank)",
    "male rank",
    "intercept male (with rank)")
  d_r <- data.frame(sna_measure = response, sex = net_sex, beh = behavior, DE = c(rep(fdev_explr, length(Fof_r)), rep(mdev_explr, length(Fom_r))), pred = preds_r, F_smooths = c(Fof_r, Fom_r), F_prop_greater_than = c(far, fr, fir, mar, mr, mir), stringsAsFactors = F) %>%
    prettify_table(include_beta_sex = FALSE) %>%
    mutate(sig = ifelse(F_prop_greater_than >= 0.95, "*", ""))
  
  d <- rbind(d_nr, d_r)
  return(d)
  
}


# test sig gam number of offspring -----
test_sig_gamm_mo<- function(response, net_sex = c("mixed","same"), behavior, fmodr, F_ranf_r){
  
  # Observed models, without and with rank as predictor, female and male same sex networks
  #fmodr   
  # Random coeffs of models
  #F_ranf_r
  source("functions/functions - prettify results table.R")
  
  # pull random Fs
  Fr_fa <- F_ranf_r %>% filter(rowname == "s(age_mid_year)") %>% pull(F) # F female age w rank

  Fr_fr <- F_ranf_r %>% filter(rowname == "s(avg_rank)") %>% pull(F) # F female rank
  
  Fr_fno <- F_ranf_r %>% filter(rowname == "s(num_off)") %>% pull(F) # F female rank
  
  Fr_fi <- F_ranf_r %>% filter(rowname == "s(chimp_id)") %>% pull(F) # F female intercept w rank
  
  
  # pull observed Fs
  Fof_r <- summary(fmodr) %>% .$s.table %>% data.frame() %>% pull(F)
  
  # compare proportion of random Fs that obs F is greater than
  
  fa <- sum(Fof_r[1] > Fr_fa)/length(Fr_fa) # female age w rank
  
  fr <- sum(Fof_r[2] > Fr_fr)/length(Fr_fr) # female rank
  
  fno <- sum(Fof_r[3] > Fr_fno)/length(Fr_fno) # female rank
  
  fi <- sum(Fof_r[4] > Fr_fi)/length(Fr_fi) # intercept

  
  # observed R
  fdev.explr <- summary(fmodr)$dev.expl
  
  preds_r <- c("female age (with rank)",
               "female rank",
               "number companion offspring",
               "intercept female (with rank)")
  d_r <- data.frame(sna_measure = response, sex = net_sex, 
                    beh = behavior, DE = c(rep(fdev.explr, length(Fof_r))),
                                          pred = preds_r, F_smooths = Fof_r, 
                                          F_prop_greater_than = c(fa, fr, fno, fi), stringsAsFactors = F) %>%
    prettify_table(include_beta_sex = FALSE) %>%
    mutate(sig = ifelse(F_prop_greater_than >= 0.95, "*", ""))
  
  return(d_r)
  
}


# separate gam results into sep rank models

rank_sep <- function(table_df){
  
  x <-table_df 
  
  t1 <- x %>%
    filter(!grepl("rank", pred))  
  
  t2 <- x %>%
    filter(grepl("rank", pred))  
  
  results <- list(t1, t2)
  
  return(results)
}


