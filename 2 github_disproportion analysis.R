###disproportion analysis


signal_detection <- function(dt1, dt2) {
 
  a_n <- dt1 %>%
    group_by(pt) %>%
    summarise(a = n()) %>%
    arrange(desc(a))
  
  ab_n <- a_n %>%
    mutate(b = sum(a) - a)
  
  
  c_n <- dt2 %>%
    group_by(pt) %>%
    summarise(c = n()) %>%
    arrange(desc(c))
  
  cd_n <- c_n %>%
    mutate(d = sum(c) - c)
  
  
  df_abcd <- left_join(ab_n, cd_n, by = "pt")
  
  df_abcdN_pt <- df_abcd %>%
    mutate(N = a + b + c + d)
  

  SIGNALS_pt <- df_abcdN_pt %>%
    mutate(a = as.numeric(a),
           b = as.numeric(b),
           c = as.numeric(c),
           d = as.numeric(d)) %>%
    # ROR
    mutate(ROR = a * d / (b * c)) %>%
    mutate(
      ROR_upper = exp(1) ^ (log(ROR) + 1.96 * sqrt(1 / a + 1 / b + 1 / c + 1 / d)),
      ROR_lower = exp(1) ^ (log(ROR) - 1.96 * sqrt(1 / a + 1 / b + 1 / c + 1 / d))
    ) %>%
    mutate(ROR_signal = case_when(
      ROR_lower > 1 & a >= 3 ~ "Y",
      .default = "N"
    )) %>%
    # PRR
    mutate(PRR = (a / (a + b)) / (c / (c + d))) %>%
    mutate(
      PRR_upper = exp(1) ^ (log(PRR) + 1.96 * sqrt(1 / a - 1 / (a + b) + 1 / c - 1 / (c + d))),
      PRR_lower = exp(1) ^ (log(PRR) - 1.96 * sqrt(1 / a - 1 / (a + b) + 1 / c - 1 / (c + d))),
      x2 = ((a * d - b * c) ^ 2) * (a + b + c + d) / ((a + b) * (c + d) * (a + c) * (b + d))
    ) %>%
    mutate(PRR_signal = case_when(
      PRR >= 2 & a >= 3 & x2 >= 4 ~ "Y",
      .default = "N"
    )) %>%
    # EBGM
    mutate(EBGM = (a * (a + b + c + d)) / ((a + c) * (a + b))) %>%
    mutate(
      EBGM_upper = exp(1) ^ (log(EBGM) + 1.96 * sqrt(1 / a + 1 / b + 1 / c + 1 / d)),
      EBGM_lower = exp(1) ^ (log(EBGM) - 1.96 * sqrt(1 / a + 1 / b + 1 / c + 1 / d))
    ) %>%
    mutate(MGPS_signal = case_when(
      EBGM_lower > 2 ~ "Y",
      .default = "N"
    )) %>%
    # IC (BCPNN)
    mutate(
      α1 = 1,
      β1 = 1,
      α = 2,
      β = 2,
      γ11 = 1,
      C = a + b + c + d,
      Cx = a + b,
      Cy = a + c,
      Cxy = a,
      γ = γ11 * (C + α) * (C + β) / (Cx + α1) / (Cy + β1),
      IC = log2(a * (a + b + c + d) / (a + b) / (a + c)),
      γ = γ11 * (C + α) * (C + β) / (Cx + α1) / (Cy + β1),
      E_IC = log2((Cxy + γ11) * (C + α) * (C + β) / (C + γ) / (Cx + α1) / (Cy + β1)),
      V_IC = (log(2) ^ 2) ^ (-1) * ((C - Cxy + γ - γ11) / (Cxy + γ11) / (1 + C + γ) + (C - Cx + α - α1) / (Cx + α1) / (1 + C + α) + (C - Cy + β - β1) / (Cy + β1) / (1 + C + β)),
      IC_upper = E_IC + 2 * sqrt(V_IC),
      IC025 = E_IC - 2 * sqrt(V_IC)  # 即IC_lower
    ) %>%
    select(-γ, -V_IC, -α1, -β1, -α, -β, -γ11, -C, -Cxy, -Cy, -N) %>%
    mutate(BCPNN_signal = case_when(
      IC025 <= 0 ~ "N",
      IC025 > 0 ~ "Y",
    ))
  
  # 添加置信区间
  SIGNALS <- SIGNALS_pt %>%
    mutate(ROR_95CI = paste0("(", round(ROR_lower, 2), "-", round(ROR_upper, 2), ")"),
           PRR_95CI = paste0("(", round(PRR_lower, 2), "-", round(PRR_upper, 2), ")"),
           EBGM_95CI = paste0("(", round(EBGM_lower, 2), "-", round(EBGM_upper, 2), ")"),
           IC_95CI = paste0("(", round(IC025, 2), "-", round(IC_upper, 2), ")")) %>%
    mutate(ROR = round(ROR, 2),
           PRR = round(PRR, 2),
           EBGM = round(EBGM, 2),
           IC = round(IC, 2))
  
  
  setDT(SIGNALS)
  
  
  positive_SIGNALS <- SIGNALS[ROR_signal == 'Y', ]
  
  return(list(SIGNALS = SIGNALS, positive_SIGNALS = positive_SIGNALS))
}
