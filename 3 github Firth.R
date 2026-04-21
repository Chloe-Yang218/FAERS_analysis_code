for(pt_name in all_pt){
  tra_outc_newdate_temp <- tra_outc_newdate_temp %>%
    mutate(
      !!pt_name := factor(
       
        as.integer(map_lgl(pt, ~ pt_name %in% strsplit(.x, ";", fixed = TRUE)[[1]])),
        levels = c(0, 1),
        labels = c("No", "Yes")
      )
    )
}

original_ae_columns <- names(tra_outc_newdate_temp)[21:51]
modified_ae_columns <- gsub(" ", "_", original_ae_columns)
modified_ae_columns <- gsub("-", "_", modified_ae_columns)
names(tra_outc_newdate_temp)[21:51] <- modified_ae_columns

library(logistf)

for (ae in modified_ae_columns) {
  tryCatch({
    
    ans <- tra_outc_newdate_temp[, .(
      var_col = get(ae),
      chemcomb,
      drug,
      combo_category,
      serious,
      continent_combine,
      three,
      age
    )]
    
    model <- logistf(
      formula = var_col ~ drug + chemcomb + combo_category +three+serious+continent_combine+age_term,
      data = ans
      
    )
    
    coef_df <- data.frame(
      term = names(model$coefficients),
      coef = model$coefficients,
      conf.low = model$ci.lower,
      conf.high = model$ci.upper,
      p.value = model$prob,
      stringsAsFactors = FALSE
    )
    
    model_summary <- coef_df %>%
      filter(term == "drug1") %>%
      mutate(
        AE = ae,
        estimate = exp(coef),
        ci_low = exp(conf.low),
        ci_high = exp(conf.high),
        OR_CI = sprintf("%.2f (%.2f-%.2f)", estimate, exp(conf.low), exp(conf.high))
      ) %>%
      dplyr::select(
        Adverse_Event = AE,
        Term = term,
        OR = estimate,
        CI_Low = ci_low,
        CI_High = ci_high,
        P_Value = p.value,
        OR_CI
      )
    
    results_list[[ae]] <- model_summary
  }, error = function(e) {
    message(paste("Error in", ae, ":", e$message))
  })
}
final_table <- bind_rows(results_list) %>% 
  arrange(P_Value)
setDT(final_table)

final_table <- bind_rows(results_list) %>% 
  arrange(P_Value) %>% 
 
  mutate(
    FDR_adjusted_p = p.adjust(P_Value, method = "fdr"),
  
    Significance = case_when(
      FDR_adjusted_p < 0.001 ~ "***",
      FDR_adjusted_p < 0.01  ~ "**",
      FDR_adjusted_p < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>% 
  #
  dplyr::select(
    Adverse_Event,
    Term,
    OR,
    CI_Low,
    CI_High,
    P_Value,
    FDR_adjusted_p,
    Significance,
    OR_CI
  )

# 
name_mapping <- data.frame(
  Modified_Name = modified_ae_columns,
  Original_Name = original_ae_columns,
  stringsAsFactors = FALSE
)

final_table <- final_table %>%
  left_join(name_mapping, by = c("Adverse_Event" = "Modified_Name")) %>%
  mutate(Adverse_Event = Original_Name) %>%
  dplyr::select(-Original_Name)
setDT(final_table)
