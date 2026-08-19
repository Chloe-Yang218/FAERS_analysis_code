library(fitdistrplus)

all_pt<-c('chills','nausea','dyspnoea','vomiting','tachycardia','rash',
          'infusion related reaction','headache')

all_pt_2<-c('ejection fraction decreased')

all_hlt<-c("cardiac disorders nec","cardiac function diagnostic procedures",
           "heart failures nec")

tra_outc_newdate_temp<-dt_faers_new_miceage

tra_outc_newdate_temp <- tra_outc_newdate_temp %>%
  mutate(
    contains_target = factor(
      as.integer(map_lgl(pt, ~ any(strsplit(.x, ";", fixed = TRUE)[[1]] %in% all_pt))),
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

dt<-tra_outc_newdate_temp[contains_target=='Yes',]

dt<-dt[!is.na(aetime)]
dt$aetime<-dt$aetime+0.5

dt_her<-dt[drug=='1',]
dt_other<-dt[drug=='0',]

times_her<-dt_her$aetime
times_other<-dt_other$aetime

fit <- fitdist(times_other, "weibull")
coef(fit) # 
confint(fit) # 

resu<-cbind(t(t(coef(fit))),confint(fit))
colnames(resu)[1]<-'coef'
resu

