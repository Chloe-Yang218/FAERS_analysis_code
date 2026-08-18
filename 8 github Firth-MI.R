imp_list <- list(
  tra_outc_newdate_imp1,
  tra_outc_newdate_imp2,
  tra_outc_newdate_imp3,
  tra_outc_newdate_imp4,
  tra_outc_newdate_imp5
)
names(imp_list) <- paste0("imp", 1:5)

M <- length(imp_list)                     
ae_terms  <- modified_ae_columns           
ae_labels <- original_ae_columns          

knots_locs <- rcs_knots(imp_list[[1]]$age, n_knots = 5)
age_term   <- paste0("ns(age, knots = c(", paste(knots_locs, collapse = ", "), "))")

demo_formula <- as.formula(paste(
  "var_col ~ drug + chemcomb + combo_category + three + serious + continent_combine +",
  age_term
))

demo_ans <- imp_list[[1]][, .(
  var_col = get(ae_terms[1]), chemcomb, drug, combo_category,
  serious, continent_combine, three, age
)]
demo_model <- logistf(formula = demo_formula, data = demo_ans)
n_params   <- length(demo_model$coefficients)  
dfcom      <- nrow(demo_ans) - n_params         

per_imp_results <- list()

for (ae in ae_terms) {
  
  beta_vec <- numeric(M)
  var_vec  <- numeric(M)
  
  for (j in seq_len(M)) {
    dat <- imp_list[[j]]
    tryCatch({
      ans <- dat[, .(
        var_col        = get(ae),
        chemcomb       = chemcomb,
        drug           = drug,
        combo_category = combo_category,
        serious        = serious,
        continent_combine = continent_combine,
        three          = three,
        age            = age
      )]
      
      formula_str <- paste(
        "var_col ~ drug + chemcomb + combo_category + three + serious + continent_combine +",
        age_term
      )
      model <- logistf(formula = as.formula(formula_str), data = ans)
      
      
      coef_names <- names(model$coefficients)
      idx <- grep("^drug1$", coef_names)
      if (length(idx) == 0) idx <- grep("^drug$", coef_names)
      if (length(idx) == 0) stop("drug1 term not found in model coefficients")
      
      beta_vec[j] <- model$coefficients[idx]
      
      var_vec[j] <- model$var[idx, idx]
      
    }, error = function(e) {
      message(sprintf("  imp %d error for %s: %s", j, ae, e$message))
      beta_vec[j] <- NA
      var_vec[j]  <- NA
    })
  }
  
  per_imp_results[[ae]] <- data.frame(
    imp  = seq_len(M),
    beta = beta_vec,
    var  = var_vec
  )
}

pool_one <- function(beta_vec, var_vec, dfcom = Inf) {
  keep <- !(is.na(beta_vec) | is.na(var_vec)) 
  Qm   <- beta_vec[keep] ##β
  Um   <- var_vec[keep] ##SE2
  m    <- length(Qm) 
  
  if (m < 2) {
    return(data.frame(
      Q_bar = Qm, U_bar = Um, B = NA, T_total = NA,
      se = NA, df = NA, lo = NA, hi = NA,
      OR = exp(Qm), OR_lo = NA, OR_hi = NA, p.value = NA, r = NA
    ))
  }
  
  Q_bar <- mean(Qm)
  
  U_bar <- mean(Um)
  
  B <- sum((Qm - Q_bar)^2) / (m - 1)
  
  T_total <- U_bar + (1 + 1 / m) * B
  
  se <- sqrt(T_total)
  
  r <- if (U_bar > 0) (1 + 1 / m) * B / U_bar else 0
  
  if (is.finite(dfcom)) {
    v_obs <- (m - 1) / r^2
    v <- ((dfcom + 1) / (dfcom + 3)) * dfcom * v_obs / (dfcom + v_obs)
  } else {
    v <- (m - 1) / r^2
  }
  
  tcrit <- qt(0.975, df = v)
  lo   <- Q_bar - tcrit * se
  hi   <- Q_bar + tcrit * se
  
  OR     <- exp(Q_bar)
  OR_lo  <- exp(lo)
  OR_hi  <- exp(hi)
  
  tstat <- Q_bar / se
  p_val <- 2 * pt(abs(tstat), df = v, lower.tail = FALSE)
  
  data.frame(
    Q_bar   = Q_bar,
    U_bar   = U_bar,
    B       = B,
    T_total = T_total,
    se      = se,
    df      = v,
    lo      = lo,
    hi      = hi,
    OR      = OR,
    OR_lo   = OR_lo,
    OR_hi   = OR_hi,
    p.value = p_val,
    r       = r,
    stringsAsFactors = FALSE
  )
}


pooled_list <- list()
for (ae in ae_terms) {
  df <- per_imp_results[[ae]]
  pooled_list[[ae]] <- pool_one(df$beta, df$var, dfcom = dfcom)
}

final_table <- bind_rows(pooled_list, .id = "AE_modified") %>%
  mutate(
    AE    = ae_labels[match(AE_modified, ae_terms)],
    OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, OR_lo, OR_hi)
  ) %>%
  dplyr::select(
    Adverse_Event   = AE,
    OR,
    CI_Low          = OR_lo,
    CI_High         = OR_hi,
    P_Value         = p.value,
    OR_CI,
   
    beta_pooled     = Q_bar,
    se_pooled       = se,
    df_rubin        = df,
    rel_incr_var    = r
  ) %>%
  arrange(P_Value)

setDT(final_table)

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
    Adverse_Event, OR, CI_Low, CI_High, P_Value, FDR_adjusted_p, Significance,
    OR_CI, beta_pooled, se_pooled, df_rubin, rel_incr_var
  )

final_table <- final_table %>%
  dplyr::select(Adverse_Event, OR, CI_Low, CI_High, OR_CI, FDR_adjusted_p,Significance)


