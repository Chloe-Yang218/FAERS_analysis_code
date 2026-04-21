######################Log-rank
library(survival)
library(survminer)


all_pt<-c('chills','nausea','dyspnoea','vomiting','tachycardia','rash',
          'infusion related reaction','headache')

all_pt_2<-c('ejection fraction decreased')

all_hlt<-c("cardiac disorders nec","cardiac function diagnostic procedures","heart failures nec")

####
tra_outc_newdate_temp<-dt

tra_outc_newdate_temp <- tra_outc_newdate_temp %>%
  mutate(
    status = as.integer(map_lgl(hlt, ~ any(strsplit(.x, ";", fixed = TRUE)[[1]] %in% all_hlt)))
  )


dt<-tra_outc_newdate_temp

dt$aetime<-dt$aetime+0.5

fit<-survfit(Surv(aetime,status) ~drug,
             data=dt)


library(scales)
library(foreach)
library(ggrepel)
library(showtext)
library(sysfonts)

#
font_add("Times New Roman", regular = "C:\\Windows\\Fonts\\times.ttf")
showtext_auto()
font_families()

surv_obj <- Surv(time = dt$aetime, event = dt$status)

# 
km_fit <- survfit(
  surv_obj ~ drug, 
  data = dt
)

km_plot <- ggsurvplot(
  km_fit,
  data = dt,
  pval = TRUE,
  risk.table = TRUE,
  palette = c("#2E9FDF", "#FC4E07"),
  xlab = "Time (Days)",
  ylab = "Event-Free Proportion",
  legend.title = "Drug Group",
  legend.labs = c("BT", "BS"),
  break.time.by = 100,
  risk.table.y.text = FALSE,
  risk.table.height = 0.25,
  surv.median.line = "h",
  

  font.title = c(18, "bold", "black"),       
  font.x = c(16, "bold", "black"),          
  font.y = c(16, "bold", "black"),           
  font.tickslab = c(14, "bold", "black"),   
  font.legend = c(14, "bold", "black"),     
  pval.size = 10,                            
  
  ggtheme = theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      axis.line = element_line()
    ),
  tables.theme = theme_cleantable() +
    theme(
      axis.text.x = element_text(size = 12),    # 
      title = element_text(size = 14)           # 
    )
)

pdf("K-M.pdf", 
    width = 10, height = 8)
print(km_plot, newpage = FALSE)
dev.off()
