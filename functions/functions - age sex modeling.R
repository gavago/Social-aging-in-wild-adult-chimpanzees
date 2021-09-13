

# modeling every sna measure -------------
# function filters the network behavior, the network sex, and the sex of network members if a mixed sex network
# then models the relationships between sna measures and network sex and subj sex specific predictor variables

age_sex_fun_all <- function(data, 
                            beh = c("total_grooming", "prox", "grooming"), 
                            net_sex = c("any_combo", "female","male"),
                            subj_sex = NULL,
                            sex_age_int = FALSE, summary = TRUE){
  
  require(lme4)
  require(tidyverse)
  #scale shorthand function
  z. <- function(x) scale(x)
  
  #set up loop for modeling each sna measure separately
  if(beh %in% c("total_grooming", "prox")){
    sna_measures <-  c("bt", "ec", "deg", "trans")  
  }
  if(beh %in% "grooming"){
    sna_measures <-  c("bt", "ec", "deg_in", "deg_out")  
  }
  
  
  mods <- vector("list", length = length(sna_measures))
  names(mods) <- sna_measures
  
  #interaction option for model list names
  if(sex_age_int == TRUE){
    names(mods) <- paste0(sna_measures,"_int")
  }
  
  # assign a subj_sex if is NULL
  # subj_sex defaults to both if only a mixed network, net_sex = "any_combo", is specified
  if(net_sex == "any_combo" & is.null(subj_sex)){subj_sex = "both"}
  if(net_sex == "female"){subj_sex <- "F"}
  if(net_sex == "male"){subj_sex <- "M"}
  
  
  #loop
  for (i in seq(sna_measures)){
    
    #model expressions
    #both sexes 
    if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == TRUE)){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + sex + z.(avg_rank) + sex*z.(age_mid_year) + (1|chimp_id))
    }
    if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == FALSE)){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + sex + z.(avg_rank) + (1|chimp_id))
    }
    if(all(net_sex == "any_combo" & subj_sex == "M" | net_sex == "male")){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + z.(avg_rank) + (1|chimp_id))
    }
    if(all(net_sex == "any_combo" & subj_sex == "F" | net_sex == "female")){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + z.(avg_rank) + z.(prop_cyc) + (1|chimp_id))
    }

    #data
    # when subj_sex is both, change subj_sex1 filter term to M and F
    if(subj_sex == "both"){subj_sex1 <- c("M", "F")} else {subj_sex1 <- subj_sex}
    
    d <- data %>%
      filter(behavior == beh, network_sex == net_sex, sex %in% subj_sex1)
    
    #model with error handling
    mod <- tryCatch({
      mod <- glmer(f, family = Gamma(link = "log") , data = d)
    }, warning = function(w) {
      mod <- "Warning: model does not converge, max|grad|"
    }, error = function(e) {
      mod <- "Error: model does not converge" 
            }) #holy fuck it worked!!!!! I finally figured out tryCatch syntax...8o http://mazamascience.com/WorkingWithData/?p=912

    # if max|grad reached, then change integration method - give model more points to integrate random effects over
    # https://stats.stackexchange.com/questions/77313/why-cant-i-match-glmer-family-binomial-output-with-manual-implementation-of-g
    if(inherits(mod, "character")){
      if(grepl("max", mod)){
        mod <- glmer(f, family = Gamma(link = "log") , data = d)
        mod <- update(mod, nAGQ = 2) 
      }
      
    }

    if(all(!inherits(mod, "character") & summary == T)){
      mod <- summary(mod)
    }
    #storage
    mods[[i]] <- mod
  }
  
  return(mods)
  
}




# glmm - modeling function for single sna measure ------
age_sex_fun_single <- function(data, sna_measure = c("bt", "ec", "deg", "trans", "deg_in", "deg_out"), 
                               beh = c("total_grooming", "prox"), 
                               net_sex = c("any_combo", "female", "male"), 
                               subj_sex = NULL,
                               sex_age_int = FALSE, quadratic = FALSE, summary = FALSE){
  
  require(lme4)
  require(tidyverse)
  #scale shorthand function
  z. <- function(x) scale(x)
  
  
  #create appropriate age and age interaction terms 
  # depending on modeling non linear relationship
  if(quadratic == TRUE) {
    age_term <- expr(z.(age_mid_year) + z.(age_mid_year^2))
  } else { age_term <- expr(z.(age_mid_year)) }
  # in the event that an interaction is used, make them according to the quadratic argument
  if(quadratic == TRUE){
    interaction_term <- expr(z.(age_mid_year)*sex + z.(age_mid_year^2)*sex)
  } else { interaction_term <- expr(sex*z.(age_mid_year))}

  #model expressions
  #both sexes 
  if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == TRUE)){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ !!age_term + sex + z.(avg_rank) + !!interaction_term + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == FALSE)){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ !!age_term + sex + z.(avg_rank) + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "M" | net_sex == "male")){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ !!age_term + z.(avg_rank) + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "F" | net_sex == "female")){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ !!age_term + z.(avg_rank) + z.(prop_cyc) + (1|chimp_id))
  }
  
  #data
  if(subj_sex == "both"){ subj_sex1 <- c("M", "F")} else {subj_sex1 <- subj_sex}
  
  d <- data %>%
    filter(behavior == beh, network_sex == net_sex, sex %in% subj_sex1)
  #model with error handling
  mod <- tryCatch({
    mod <- glmer(f, family = Gamma(link = "log") , data = d)
  }, warning = function(w) {
    mod <- "Warning: model does not converge, max|grad|"
  }, error = function(e) {
    mod <- "Error: model does not converge" 
  }) # I finally figured out tryCatch syntax...8o http://mazamascience.com/WorkingWithData/?p=912
  
# if max|grad reached, then change integration method - give model more points to integrate random effects over
  # https://stats.stackexchange.com/questions/77313/why-cant-i-match-glmer-family-binomial-output-with-manual-implementation-of-g
  if(inherits(mod, "character")){
    if(grepl("max", mod)){
      mod <- glmer(f, family = Gamma(link = "log") , data = d)
      mod <- update(mod, nAGQ = 2) 
    }
    }
  
  #make summary
  if(all(!inherits(mod, "character") & summary == T)){
    mod <- summary(mod)
  }
  
  
  return(mod)
  
}


# glmm -modeling function for single sna measure w no extra attributes (no rank, no prop cyc)  ------
age_sex_fun_single_no_attr <- function(data, sna_measure = c("bt", "ec", "deg", "trans", "deg_in", "deg_out"), 
                               beh = c("total_grooming", "prox"), 
                               net_sex = c("any_combo", "female", "male"), 
                               subj_sex = NULL,
                               sex_age_int = FALSE, quadratic = FALSE, summary = FALSE){
  
  require(lme4)
  require(tidyverse)
  #scale shorthand function
  z. <- function(x) scale(x)
  
  
  #create appropriate age and age interaction terms 
  # depending on modeling non linear relationship
  if(quadratic == TRUE) {
    age_term <- expr(z.(age_mid_year) + z.(age_mid_year^2))
  } else { age_term <- expr(z.(age_mid_year)) }
  # in the event that an interaction is used, make them according to the quadratic argument
  if(quadratic == TRUE){
    interaction_term <- expr(z.(age_mid_year)*sex + z.(age_mid_year^2)*sex)
  } else { interaction_term <- expr(sex*z.(age_mid_year))}
  
  #model expressions
  #both sexes 
  if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == TRUE)){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ !!age_term + sex + !!interaction_term + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == FALSE)){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ !!age_term + sex + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "M" | net_sex == "male")){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ !!age_term + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "F" | net_sex == "female")){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ !!age_term + (1|chimp_id))
  }
  
  #data
  if(subj_sex == "both"){ subj_sex1 <- c("M", "F")} else {subj_sex1 <- subj_sex}
  
  d <- data %>%
    filter(behavior == beh, network_sex == net_sex, sex %in% subj_sex1)
  #model with error handling
  mod <- tryCatch({
    mod <- glmer(f, family = Gamma(link = "log") , data = d)
  }, warning = function(w) {
    mod <- "Warning: model does not converge, max|grad|"
  }, error = function(e) {
    mod <- "Error: model does not converge" 
  }) # I finally figured out tryCatch syntax...8o http://mazamascience.com/WorkingWithData/?p=912
  
  # if max|grad reached, then change integration method - give model more points to integrate random effects over
  # https://stats.stackexchange.com/questions/77313/why-cant-i-match-glmer-family-binomial-output-with-manual-implementation-of-g
  if(inherits(mod, "character")){
    if(grepl("max", mod)){
      mod <- glmer(f, family = Gamma(link = "log") , data = d)
      mod <- update(mod, nAGQ = 2) 
    }
  }
  
  #make summary
  if(all(!inherits(mod, "character") & summary == T)){
    mod <- summary(mod)
  }
  
  
  return(mod)
  
}


# peek in modeled data set ------

peep_dataset <- function(data, 
                         beh = c("total_grooming", "prox", "grooming"), 
                         net_sex = c("any_combo", "female", "male"),
                        subj_sex = NULL){
  
  filter <- dplyr::filter
  
  # assign a subj_sex if is NULL
  # subj_sex defaults to both if only a mixed network, net_sex = "any_combo", is specified
  if(net_sex == "any_combo" & is.null(subj_sex)){subj_sex = "both"}
  if(net_sex == "female"){subj_sex <- "F"}
  if(net_sex == "male"){subj_sex <- "M"}
  
  #data
  # when subj_sex is both, change subj_sex1 filter term to M and F
  if(subj_sex == "both"){subj_sex1 <- c("M", "F")} else {subj_sex1 <- subj_sex}
  
  dat <- data %>%
    filter(behavior == beh, network_sex == net_sex, sex %in% subj_sex1)
  
  zeros <- dat %>%
    select_if(is.numeric) %>%
    apply(., 2, function(x) sum(x == 0, na.rm = T))
  
  if(subj_sex == "both"){
    desc <- dat %>%
      group_by(sex) %>%
      summarise(n_obs = n(), n_chimps = n_distinct(chimp_id))  
  }
  if(subj_sex != "both"){
  desc <- dat %>%
    summarise(n_obs = n(), n_chimps = n_distinct(chimp_id))
  }
  
  df_info <- list(dat, zeros, desc)
  return(df_info)
  
}

# extract coefficient function - mostly for randomizations ------

#figure out conditional lapply or just make loop for extracting

ex_coef <- function(mod_list, pred = c("age", "sex", "rank", "prop_cyc", "int")){
  
  coef_list <- vector("list", length = 4) # one for each network measure
  names(coef_list) <- paste(names(mod_list), pred, sep = "_")
  if(pred == "int"){pred <- ":"} # for grep rownames in loop
  
  
  for(k in 1:4){
    m <- mod_list[[k]]
    
    if(inherits(m, "summary.merMod")){
    cs <- coef(m) %>% data.frame()#summary
    
    b <- cs %>% slice(grep(pred, rownames(.))) %>%
      .[1,1] #always chose the first row (when are more than 2, e.g. age and sex alone and in interaction) and first col bc = est
      }
    
    if( is.character(m)){
      b <- NA
    }
    
    coef_list[[k]] <- b
  }
  coefs <- unlist(coef_list)
  return(coefs)
}

ex_coef_single <- function(m, pred = c("age", "age_squared", "sex", "rank", "prop_cyc", "int")){
  
  #name_m <- ensym(m)

  if(pred == "int"){pred_term <- ":"} # for grep rownames in loop
  if(pred == "age"){pred_term <- "age_mid_year)"}
  if(pred == "age_squared"){pred_term <- "age_mid_year\\^2)"}
  if(!grepl("age", pred)){pred_term <- pred}
  
    if(inherits(m, "summary.merMod")){
      cs <- coef(m) %>% data.frame()#summary
      
      b <- cs %>% slice(grep(pred_term, rownames(.))) %>%
        .[1,1] #always chose the first row (when are more than 2, e.g. age and sex alone and in interaction) and first col bc = estimate
    }
    
    if( is.character(m)){
      b <- NA
    }
    
  names(b) <- pred #paste(name_m, pred, sep = "_")
  return(b)
}
