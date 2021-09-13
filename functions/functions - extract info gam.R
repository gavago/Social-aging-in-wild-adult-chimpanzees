
# NEVER USED

extract_gam_info <- function(gam){
  
  sex_row <- data.frame(rowname = "sex", `F` = NA) 
  Fs <- summary(gam) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname, `F`) %>% rbind(sex_row, .)
  coefs<- summary(gam)$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] # sex beta
  stats <- data.frame(Fs, sex_beta = coefs[1], se = coefs[2], ) %>% mutate_if(is.numeric, round, 2)

  dup_row <- duplicated(stats[,c("sex_beta", "se", "Rs")])
  stats[dup_row,c("sex_beta", "se", "Rs")] <- ""
  stats
  }

tidy(digr)
