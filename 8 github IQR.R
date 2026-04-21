split_pts <- strsplit(dt_bs$pt, ";", fixed = TRUE)
lengths <- lengths(split_pts)
dt_expanded <- dt_bs[rep(seq_len(nrow(dt_bs)), lengths), ]
dt_expanded$pt <- unlist(split_pts)

setDT(dt_expanded)

dt_iqr_pt_bs <- dt_expanded %>%
  group_by(pt) %>%
  summarise(
    n = sum(!is.na(aetime)), 
    median_aetime = median(aetime, na.rm = TRUE),
    q1 = quantile(aetime, 0.25, na.rm = TRUE),
    q3 = quantile(aetime, 0.75, na.rm = TRUE),
    iqr_aetime = paste0(q1, "-", q3)
  ) %>%
  filter(n > 0)
