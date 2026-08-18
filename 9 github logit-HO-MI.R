imp_list <- list(
  tra_outc_newdate_imp1,
  tra_outc_newdate_imp2,
  tra_outc_newdate_imp3,
  tra_outc_newdate_imp4,
  tra_outc_newdate_imp5
)
names(imp_list) <- paste0("imp", 1:5)

M <- length(imp_list)                   
ae_terms  <- original_ae_columns          
ae_labels <- original_ae_columns

source("Specific Functions for FAERS.R", local = TRUE)

dummy_dat <- imp_list[[1]]
dummy_ae  <- ae_terms[1]

age_knots_locs_dummy <- rcs_knots(dummy_dat$age, n_knots = 5)
age_term_dummy <- paste0("ns(age, knots = c(", paste(age_knots_locs_dummy, collapse = ", "), "))")

dummy_ans <- dummy_dat[
  , .(N = .N),
  by = .(var_col = get(dummy_ae), HO, chemcomb, combo_category, three, continent_combine, age)
]
dummy_ans <- na.omit(dummy_ans)
setnames(dummy_ans, "var_col", dummy_ae)

dummy_formula <- reformulate(
  termlabels = c(dummy_ae, "combo_category", "chemcomb", "three", "continent_combine", age_term_dummy),
  response = "HO"
)

dummy_model <- glm(formula = dummy_formula, data = dummy_ans, weights = N, family = binomial)
dfcom <- length(coef(dummy_model)) - 1  

#message(sprintf("dfcom (non-intercept parameters) = %d", dfcom))

library(data.table)
library(marginaleffects)
library(foreach)
library(splines)

knots_locs <- rcs_knots(imp_list[[1]]$age, n_knots = 5)
age_term   <- paste0("ns(age, knots = c(", paste(knots_locs, collapse = ", "), "))")

per_imp_results <- list()  

for (var in ae_terms) {
  
  log_rr_vec <- numeric(M)
  se_vec     <- numeric(M)
  pval_vec   <- numeric(M)
  
 
  prob_event_list   <- vector("list", M)
  prob_noevent_list <- vector("list", M)
  
  for (j in seq_len(M)) {
    dat <- imp_list[[j]]
    
   
    ans <- dat[
      , .(N = .N),
      by = .(var_col = get(var), HO, chemcomb, combo_category, three, continent_combine, age)
    ][, var_name := var]
    
    setnames(ans, "var_col", var)
    ans <- na.omit(ans)
    
   
    if (length(unique(ans[[var]])) < 2) {
      warning(sprintf("imp %d: %s one", j, var))
      log_rr_vec[j] <- NA
      se_vec[j]     <- NA
      pval_vec[j]   <- NA
      next
    }
    
    factors_to_check <- c("chemcomb", "combo_category", var)
    has_enough <- sapply(factors_to_check, function(v) {
      if (is.factor(ans[[v]])) nlevels(ans[[v]]) >= 2 else TRUE
    })
    if (!all(has_enough)) {
      warning(sprintf("imp %d: %s nono", j, var))
      log_rr_vec[j] <- NA
      se_vec[j]     <- NA
      pval_vec[j]   <- NA
      next
    }
    
    
    var_escaped <- ifelse(grepl(" ", var), paste0("`", var, "`"), var)
    
    
    m0_formula <- reformulate(
      termlabels = c("combo_category", "chemcomb", "three", "continent_combine", age_term),
      response = "HO"
    )
    m1_formula <- reformulate(
      termlabels = c(var_escaped, "combo_category", "chemcomb", "three", "continent_combine", age_term),
      response = "HO"
    )
    
    
    m0 <- glm(formula = m0_formula, data = ans, weights = N, family = binomial)
    m1 <- glm(formula = m1_formula, data = ans, weights = N, family = binomial)
    
    df_marg <- avg_comparisons(
      m1,
      variables = var,
      transform_pre = "lnratioavg"   
    )
    
    log_rr_vec[j] <- coef(df_marg)          # log RR
    se_vec[j]     <- sqrt(vcov(df_marg))    # SE of log RR
    
    
    pval_vec[j] <- df_marg$p.value
    
   
    prob_df <- avg_predictions(m1, variables = var)
    prob_event_list[[j]]   <- prob_df[prob_df[[names(prob_df)[1]]] == "Yes", "estimate"]
    prob_noevent_list[[j]] <- prob_df[prob_df[[names(prob_df)[1]]] == "No",  "estimate"]
    
    message(sprintf("  imp %d done: log_RR=%.4f, SE=%.4f", j, log_rr_vec[j], se_vec[j]))
  }
  
  
  per_imp_results[[var]] <- list(
    log_rr = log_rr_vec,
    se     = se_vec,
    pval   = pval_vec,
    prob_event   = prob_event_list,
    prob_noevent = prob_noevent_list
  )
}



library(magrittr)

pooled_results <- list()

for (var in ae_terms) {
  res <- per_imp_results[[var]]
  
  
  keep <- !(is.na(res$log_rr) | is.na(res$se))
  Qm   <- res$log_rr[keep]
  Sm   <- res$se[keep]
  Um   <- Sm^2
  m    <- length(Qm)
  
  if (m < 2) {
    warning(sprintf("%s: imp < 2, nono", var))
    pooled_results[[var]] <- data.frame(
      AE = var, log_rr = NA, se = NA, df = NA,
      RR = NA, CI_low = NA, CI_high = NA, P_Value = NA,
      m_used = m, stringsAsFactors = FALSE
    )
    next
  }
  
  
  Q_bar <- mean(Qm)                        
  U_bar <- mean(Um)                          
  B     <- sum((Qm - Q_bar)^2) / (m - 1)  
  T_total <- U_bar + (1 + 1/m) * B         
  SE_pooled <- sqrt(T_total)                
  
  
  r <- (1 + 1/m) * B / U_bar              
  v_obs <- (m - 1) / r^2                   
  v_com <- dfcom                            
  v_star <- (v_obs * v_com) / (v_obs + v_com)  
 
  df_pooled <- max(min(v_star, v_com), 1)
  
  
  t_crit <- qt(0.975, df = df_pooled)
  lo <- Q_bar - t_crit * SE_pooled
  hi <- Q_bar + t_crit * SE_pooled
  
  
  RR_pooled <- exp(Q_bar)
  CI_low    <- exp(lo)
  CI_high   <- exp(hi)
  
  
  p_pooled <- 2 * pnorm(-abs(Q_bar / SE_pooled))
  
  prob_event_pooled   <- mean(unlist(res$prob_event[keep]))
  prob_noevent_pooled <- mean(unlist(res$prob_noevent[keep]))
  
  pooled_results[[var]] <- data.frame(
    AE = var,
    log_rr   = Q_bar,
    se       = SE_pooled,
    df       = df_pooled,
    RR       = RR_pooled,
    CI_low   = CI_low,
    CI_high  = CI_high,
    P_Value  = p_pooled,
    event_hospitalization_rate     = prob_event_pooled,
    noevent_hospitalization_rate  = prob_noevent_pooled,
    m_used   = m,
    stringsAsFactors = FALSE
  )
  
  message(sprintf(
    "Pooled %s: RR=%.4f (%.4f-%.4f), p=%.4g, df=%.1f, m=%d",
    var, RR_pooled, CI_low, CI_high, p_pooled, df_pooled, m
  ))
}

final_table <- rbindlist(pooled_results, use.names = TRUE, fill = TRUE) %>%
 
  mutate(RR_CI = sprintf("%.2f (%.2f-%.2f)", RR, CI_low, CI_high)) %>%
 
  mutate(Adverse_Event = AE) %>%
  
  dplyr::select(
    Adverse_Event,
    RR,
    CI_Low   = CI_low,
    CI_High  = CI_high,
    P_Value,
    RR_CI,
    event_hospitalization_rate,
    noevent_hospitalization_rate,
    log_rr_pooled = log_rr,
    se_pooled     = se,
    df_rubin      = df,
    m_used
  ) %>%
  arrange(P_Value)

final_table <- final_table %>%
  mutate(
    FDR_adjusted_p = p.adjust(P_Value, method = "fdr"),
    Significance = case_when(
      FDR_adjusted_p < 0.001 ~ "***",
      FDR_adjusted_p < 0.01  ~ "**",
      FDR_adjusted_p < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(FDR_adjusted_p) %>%
  dplyr::select(
    Adverse_Event, RR, CI_Low, CI_High, P_Value, FDR_adjusted_p, Significance,
    RR_CI, event_hospitalization_rate, noevent_hospitalization_rate,
    log_rr_pooled, se_pooled, df_rubin, m_used
  )

