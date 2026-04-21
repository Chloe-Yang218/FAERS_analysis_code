#######
dfs <- foreach(drug = drugs, var = aes) %do% {
  source("Specific Functions for FAERS.R", local = TRUE)
  
  
  age_knots_locs <- rcs_knots(tra_outc_newdate_temp$age, n_knots = 5)
  age_term <- paste0("ns(age, knots = c(", paste(age_knots_locs, collapse = ", "), "))")
  
  ans <- tra_outc_newdate_temp[
    , .(N = .N), 
    by = .(var_col = get(var), HO,chemcomb, combo_category ,three,continent_combine,age)  # 使用get()动态获取列
  ][, var_name := var]  
  
  setnames(ans, "var_col", var) 
  ans <- na.omit(ans)
  
 
  if (length(unique(ans[[var]])) < 2) {
    warning(paste("var", var, "no"))
    return(NULL) 
  }
  
 
  check_factor_levels <- function(data, vars) {
    sapply(vars, function(v){
      if (is.factor(data[[v]])) {
        nlevels(data[[v]]) >= 2
      } else {
        TRUE 
      }
    })
  }
  
  
  factors_to_check <- c("chemcomb", "combo_category", var)
  if (!all(check_factor_levels(ans, factors_to_check))) {
    warning(paste("no:", var))
    return(NULL)
  }
  

  var_escaped <- ifelse(
    grepl(" ", var), 
    paste0("`", var, "`"), 
    var
  )
  
  m0_formula <- reformulate(termlabels = c("combo_category","chemcomb","three","continent_combine",age_term),
                            response = "HO")
  
  m1_formula <- reformulate(termlabels = c(var_escaped, "combo_category","chemcomb","three","continent_combine",age_term),
                            response = "HO")
  

  m0 <- glm(formula = m0_formula, data = ans, weights = N, family = binomial)
  m1 <- glm(formula = m1_formula, data = ans, weights = N, family = binomial)
  
  
  anova_pval <- anova(m1, m0, test = "LRT")$`Pr(>Chi)`[2]
  
 
  df <- avg_comparisons(m1, transform_pre = "lnratioavg", variables = var, transform = exp) 
  
  df <- df %>%
    rename("or" = "estimate",
           "lci" = "conf.low",
           "uci" = "conf.high",
           "pval" = "p.value") %>%
    mutate(across(.cols = c(or, lci, uci),
                  ~ . %>% r2)) %>%
    mutate(var = var) %>% 
    mutate(pval = pval) %>%
    mutate(es = paste0("OR: ", or, " [", "95% CI: ", lci, " to ", uci, "]"))  
  
  df<- df %>%
    dplyr::mutate(table_es = es %>% stringr::str_remove_all("OR: |95% CI: ")) %>%
    dplyr::select(var, or, lci, uci, es, table_es, pval)
  
  #Get adjusted probability of mortality with and without event
  prob_df <- m1 %>% avg_predictions(variables = var) %>% data.frame
  df$event_hospitalization_rate <- prob_df %>% filter(!!sym(names(prob_df)[1]) == 'Yes') %>% spull(estimate) %>% pct_table
  df$noevent_hospitalization_rate <- prob_df %>% filter(!!sym(names(prob_df)[1]) == 'No') %>% spull(estimate) %>% pct_table
  #df$event_mortality <- prob_df %>% filter(!!sym(names(prob_df)[1]) == 'Yes') %>% spull(estimate) %>% pct_table
  #df$noevent_mortality <- prob_df %>% filter(!!sym(names(prob_df)[1]) == 'No') %>% spull(estimate) %>% pct_table
  
  #Add pval
  df$anova_pval <- anova_pval
  #Add drug and AE data
  df$ae <- var
  #df$drug <- drug
  
  #Check for contradiction between marginal p and model p
  
  df$pc <- sig(df$pval) == sig(df$anova_pval)
  #Print it out
  df
  
}

mort_rr_df <- rbindlist(dfs)
setDT(mort_rr_df)

mort_rr_df$pval_fdr <- p.adjust(mort_rr_df$pval, method = "fdr")

mort_rr_df$anova_pval_fdr <- p.adjust(mort_rr_df$anova_pval, method = "fdr")

mort_rr_df<-mort_rr_df[pc=='TRUE',]
mort_rr_df<-mort_rr_df[anova_pval_fdr<0.05,]
mort_rr_df<-mort_rr_df[pval_fdr<0.05,]

mort_rr_df <- mort_rr_df %>%
  left_join(name_mapping, by = c("var" = "Modified_Name")) %>%
  mutate(var = Original_Name) %>%
  select(-Original_Name)