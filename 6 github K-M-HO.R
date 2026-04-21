library(survival)   
library(survminer)   

dt_km<-dt
dt_km$aetime<-dt_km$aetime+0.5

dt_km$HO <- as.numeric(dt_km$HO) - 1  

surv_obj <- Surv(time = dt_km$aetime, event = dt_km$HO)

km_fit <- survfit(surv_obj ~ drug, data = dt_km)

pdf("Kaplan_Meier_Curve.pdf", width = 10, height = 8)

km_plot <- ggsurvplot(km_fit,
                      data = dt_km,
                      pval = TRUE,
                      pval.coord = c(0.1, 0.1),
                      conf.int = TRUE,
                      risk.table = TRUE,
                      xlab = "Time (days)",
                      ylab = "Hospitalization-free Probability",
                      title = "Kaplan-Meier Curve: Time to Hospitalization by Drug Type",
                      legend.title = "Drug Group",
                      legend.labs = c("BT", "BS"),
                      palette = c("#E7B800", "#2E9FDF"),
                      break.time.by = 30,
                      ggtheme = theme_minimal())

print(km_plot, newpage = FALSE)
dev.off()

