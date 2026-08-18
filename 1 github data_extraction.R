###########data extraction

library(dplyr)
library(stringr)
library(data.table)
library(readxl)
library(writexl)
library(openxlsx)
library(tibble)
library(lubridate)
library(readr)
library(progressr)

#Raw data can be downloaded from the FDA's official website

########################demo----------------------------------------------------------------------

# Read DEMO files in batches: large file size + year-specific format variations

#########demo 1224

setwd("C:\\Users\\86138\\Desktop\\FAERS 2004Q1-2024Q2\\2012q4-2024q2")

files <- list.files(pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
demo_files_1224<-files[grepl("demo",files,ignore.case = TRUE)]

age_multipliers <- c("DY" = 1 / 30.4, "DEC" = 120, "HR" = 1 / 730, "WK" = 1 / 4.3, "MON" = 1, "YR" = 12)

extract_demo_1224 <- function(demo_files_1224) {
  df_demo_1224 <- rbindlist(lapply(demo_files_1224, function(file) {
    df <- fread(file, sep = "$", colClasses = "character", na.strings = "")
    required_columns <- c("gndr_cod", "age_cod", "sex", "fda_dt", "event_dt", 
                          "occur_country", "report_country", "rept_dt", "occp_cod")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      df[, (missing_columns) := NA_character_]
    }
    
   
    if ("sex" %in% names(df)) {
      df[, sex := as.character(sex)]
    } else if ("gndr_cod" %in% names(df)) {
      df[, sex := as.character(gndr_cod)]
    } else {
      df[, sex := NA_character_]
    }
    
    df[, sex := fifelse(sex == "M", "M", fifelse(sex == "F", "F", NA_character_))]
    
    
    if ("wt" %in% names(df) & "wt_cod" %in% names(df)) {
      df[, wt := as.numeric(wt)]
      df[, wt := fifelse(wt_cod == "KG", round(wt, 2),
                         fifelse(wt_cod == "LBS", round(wt * 0.45, 2),
                                 fifelse(wt_cod == "GMS", round(wt / 1000, 2), NA_real_)))]
    }
    
    
    df[, age := as.numeric(age)]
    df[, age := round(age * fifelse(age_cod %in% names(age_multipliers), 
                                    age_multipliers[age_cod], 0), 2)]
    df[, age := fifelse(age < 0, NA_real_, fifelse(age > 1200, NA_real_, age))]
    
    return(df) 
  }), fill = TRUE)
  
  df_demo_1224 <- df_demo_1224[, .(primaryid, caseid,sex, age, wt, fda_dt, rept_dt, event_dt, occur_country, report_country, occp_cod)]
  
  return(df_demo_1224)  
}
df_demo_1224 <- extract_demo_1224(demo_files_1224)


#####demo 0412

setwd("C:\\Users\\86138\\Desktop\\FAERS 2004Q1-2024Q2\\2004q1-2012q3")
files_0412 <- list.files(pattern = "\\.TXT$", recursive = TRUE, full.names = TRUE)

demo_files_0412_csv <- list.files(path = "C:\\Users\\86138\\Desktop\\FAERS 2004Q1-2024Q2\\2004q1-2012q3\\demo_04q1-12q3_csv", pattern = "*.csv", full.names = TRUE)

age_multipliers <- c("DY" = 1/30.4, "DEC" = 120, "HR" = 1 / 730, "WK" = 1 / 4.3, "MON" = 1, "YR" = 12)
extract_demo_columns_0412_csv <- function(demo_files_0412_csv) {
  df_demo_0412_csv <- lapply(demo_files_0412_csv, function(file) {
    df <- read_csv(file) 
    df[] <- lapply(df, as.character) 
    required_columns<-c("occur_country",
                        "report_country"
    )
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      df[missing_columns] <- NA
    }
    
    df<-df%>%
      select(primaryid,caseid,fda_dt,gndr_cod,age,age_cod,wt,wt_cod,event_dt,occur_country,report_country,rept_dt,occp_cod)
    
    if ("sex" %in% colnames(df)) {
      df$sex<- as.character(df$sex)
    } else if ("gndr_cod" %in% colnames(df)) {
      df$sex<- as.character(df$gndr_cod)
    } else {
      df$sex<- NA
    }
    
    if ("wt" %in% names(df) & "wt_cod" %in% names(df)) {
      df <- df %>%
        mutate(wt = as.numeric(wt),
               wt = case_when(
                 is.na(wt_cod) ~ NA_real_,
                 wt_cod == "KG" ~ round(wt, 2),
                 wt_cod == "LBS" ~ round(wt * 0.45, 2),
                 wt_cod == "GMS" ~ round(wt / 1000, 2),
                 TRUE ~ NA_real_
               ))
    }
    
    df <- df %>%
      mutate(
        age = as.numeric(age),
        age = round(age * recode(age_cod, !!!age_multipliers, .default = 0), 2))
    
   
    df <- df %>%
      mutate(age = ifelse(age < 0 | age > 1200, NA_real_, age),
             wt = ifelse(wt < 0, NA_real_, wt),
             sex = ifelse(!sex %in% c("F", "M"), NA_character_, sex))
    
    df[] <- lapply(df, as.character) 
    return(df)  
  }) %>% bind_rows() 
  return(df_demo_0412_csv)
}
df_demo_0412_csv <- extract_demo_columns_0412_csv(demo_files_0412_csv)
setDT(df_demo_0412_csv)
df_demo_0412_csv <- df_demo_0412_csv[,.(primaryid, caseid,sex, age, wt, fda_dt, rept_dt, event_dt, occur_country, report_country, occp_cod)]

#####demo 0424
df_demo_0424_initial <- rbindlist(list(df_demo_0412_csv,df_demo_1224))
save(df_demo_0424_initial,file="df_demo_0424_initial.Rdata")
###21558936

######demo 24q3
setwd('D:\\aaa\\FAERS 2004Q1-2025Q2\\2012q4-2024q3\\24q3')

files <- list.files(pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
demo_files_24q3<-files[grepl("demo",files,ignore.case = TRUE)]

age_multipliers <- c("DY" = 1 / 30.4, "DEC" = 120, "HR" = 1 / 730, "WK" = 1 / 4.3, "MON" = 1, "YR" = 12)
extract_demo_24q3 <- function(demo_files_24q3) {
  df_demo_24q3 <- rbindlist(lapply(demo_files_24q3, function(file) {
    df <- fread(file, sep = "$", colClasses = "character", na.strings = "")
    required_columns <- c("gndr_cod", "age_cod", "sex", "fda_dt", "event_dt", 
                          "occur_country", "report_country", "rept_dt", "occp_cod")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      df[, (missing_columns) := NA_character_]
    }
    
    
    if ("sex" %in% names(df)) {
      df[, sex := as.character(sex)]
    } else if ("gndr_cod" %in% names(df)) {
      df[, sex := as.character(gndr_cod)]
    } else {
      df[, sex := NA_character_]
    }
    
    df[, sex := fifelse(sex == "M", "M", fifelse(sex == "F", "F", NA_character_))]
    
    
    if ("wt" %in% names(df) & "wt_cod" %in% names(df)) {
      df[, wt := as.numeric(wt)]
      df[, wt := fifelse(wt_cod == "KG", round(wt, 2),
                         fifelse(wt_cod == "LBS", round(wt * 0.45, 2),
                                 fifelse(wt_cod == "GMS", round(wt / 1000, 2), NA_real_)))]
    }
    
   
    df[, age := as.numeric(age)]
    df[, age := round(age * fifelse(age_cod %in% names(age_multipliers), 
                                    age_multipliers[age_cod], 0), 2)]
    df[, age := fifelse(age < 0, NA_real_, fifelse(age > 1200, NA_real_, age))]
    
    return(df) 
  }), fill = TRUE)
  
  df_demo_24q3 <- df_demo_24q3[, .(primaryid, caseid,sex, age, wt, fda_dt, rept_dt, event_dt, occur_country, report_country, occp_cod)]
  
  return(df_demo_24q3)  
}
df_demo_24q3 <- extract_demo_24q3(demo_files_24q3)


#####demo 0424q3
df_demo_0424q3_initial <- rbindlist(list(df_demo_0424_initial,df_demo_24q3))
save(df_demo_0424q3_initial,file="df_demo_0424q3_initial.Rdata")
###21964449


########Delete records 

setwd('D:\\aaa\\FAERS 2004Q1-2025Q2\\2012q4-2024q3')

files <- list.files(pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
dele_files<-files[grepl("dele",files,ignore.case = TRUE)]

df_dele <- data.frame()
dele_list <- lapply(dele_files, function(file) {
  read.table(file, header = FALSE, col.names = NA, fill = TRUE, sep = "\n")
})
df_dele <- do.call(rbind, dele_list)
df_dele <- setDT(df_dele)
df_dele <- df_dele %>% unique()
names(df_dele) <- c("caseid")

df_demo_0424q3_by_delete <- df_demo_0424q3_initial[!(caseid %in% df_dele$caseid)]
save(df_demo_0424q3_by_delete,file="df_demo_0424q3_by_delete.Rdata")
###21861655

###Duplication records (by caseid and FDA_dt)

df_demo_0424q3_by_delete$fda_dt <- as.numeric(df_demo_0424q3_by_delete$fda_dt)
df_demo_0424q3_by_caseid <- df_demo_0424q3_by_delete[order(fda_dt), tail(.SD, 1), by = .(caseid)]

df_demo_0424q3_by_caseid$primaryid <- as.numeric(df_demo_0424q3_by_caseid$primaryid)
df_demo_0424q3_by_caseid <- df_demo_0424q3_by_caseid[order(primaryid), tail(.SD, 1), by = .(caseid)]
##18278477

##check
duplicates <- df_demo_0424q3_by_caseid [duplicated(df_demo_0424q3_by_caseid $caseid) | duplicated(df_demo_0424q3_by_caseid $caseid, fromLast = TRUE), ]
if (nrow(duplicates) > 0) {
  print("duplicate")
} else {
  print("no-duplicate")
}
#no-duplicate

df_demo_0424q3_by_caseid$primaryid <- as.character(df_demo_0424q3_by_caseid$primaryid)
save(df_demo_0424q3_by_caseid,file="df_demo_0424q3_by_caseid.Rdata")


#######################reac,indi,drug,ther...---------------------------------------------------------------------------

##The data extraction protocol for other datasets follows the same procedure as that used for the DEMO files

####0412

setwd("C:\\Users\\86138\\Desktop\\FAERS 2004Q1-2024Q2\\2004q1-2012q3")
files_0412 <- list.files(pattern = "\\.TXT$", recursive = TRUE, full.names = TRUE)
drug_files_0412<-files_0412[grepl("drug",files_0412,ignore.case = TRUE)]
indi_files_0412<-files_0412[grepl("indi",files_0412,ignore.case = TRUE)]
reac_files_0412<-files_0412[grepl("reac",files_0412,ignore.case = TRUE)]
ther_files_0412<-files_0412[grepl("ther",files_0412,ignore.case = TRUE)]
rpsr_files_0412<-files_0412[grepl("rpsr",files_0412,ignore.case = TRUE)]

###ther0412
extract_ther_columns_0412 <- function(ther_files_0412) {
  df_ther_0412 <- lapply(ther_files_0412, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE,row.names = NULL)
    df <- df %>%
      select(row.names,`DRUG_SEQ`,`ISR`) %>%
      rename(primaryid =row.names, start_dt =`DRUG_SEQ`,drug_seq=`ISR`) 
    df[] <- lapply(df, as.character)
    return(df)  
  }) %>% bind_rows()
  return(df_ther_0412)
}
df_ther_0412 <- extract_ther_columns_0412(ther_files_0412)
setDT(df_ther_0412)

###reac0412
reac_files_0412_csv <- list.files(path = "C:\\Users\\86138\\Desktop\\FAERS 2004Q1-2024Q2\\2004q1-2012q3\\reac_04q1-12q3_csv", pattern = "*.csv", full.names = TRUE)
extract_reac_columns_0412_csv <- function(reac_files_0412_csv) {
  df_reac_0412_csv <- lapply(reac_files_0412_csv, function(file) {
    df <- read_csv(file) 
    df[] <- lapply(df, as.character) 
    df<-df%>%
      rename(primaryid=ISR,
             pt=PT)%>%
      mutate(pt = str_to_lower(pt),
             pt = str_trim(pt))%>%
      select(primaryid,pt)
    df[] <- lapply(df, as.character) 
    return(df)  
  }) %>% bind_rows() 
  return(df_reac_0412_csv)
}
df_reac_0412_csv <- extract_reac_columns_0412_csv(reac_files_0412_csv)

###indi0412
extract_indi_columns_0412 <- function(indi_files_0412) {
  df_indi_0412 <- lapply(indi_files_0412, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE,row.names = NULL)
    df <- df %>%
      select(`ISR`,`INDI_PT`) %>%
      rename(primaryid =`ISR`, indi_pt = `INDI_PT`) 
    
    df$indi_pt <- trimws(tolower(df$indi_pt))
    df[] <- lapply(df, as.character)
    return(df)  
  }) %>% bind_rows()
  return(df_indi_0412)
}
df_indi_0412 <- extract_indi_columns_0412(indi_files_0412)

##drug0412
drug_files_0412_csv <- list.files(path = "C:\\Users\\86138\\Desktop\\FAERS 2004Q1-2024Q2\\2004q1-2012q3\\drug_04q1-12q3_csv", pattern = "*.csv", full.names = TRUE)
extract_drug_columns_0412_csv <- function(drug_files_0412_csv) {
  df_drug_0412_csv <- lapply(drug_files_0412_csv, function(file) {
    df <- read_csv(file) 
    df[] <- lapply(df, as.character) 
    #print(colnames(df))
    required_columns<-c("prod_ai")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      df[missing_columns] <- NA
    }
    df<-df%>%
      filter(role_cod == "PS")%>%
      mutate(prod_ai = str_to_lower(prod_ai),
             drugname = str_to_lower(drugname),
             prod_ai = str_trim(prod_ai),
             drugname = str_trim(drugname))%>%
      select(primaryid,prod_ai,drugname,drug_seq)
    df[] <- lapply(df, as.character) 
    return(df)  
  }) %>% bind_rows() 
  return(df_drug_0412_csv)
}
df_drug_0412_csv <- extract_drug_columns_0412_csv(drug_files_0412_csv)
setDT(df_drug_0412_csv)

#####1224
setwd("C:\\Users\\86138\\Desktop\\FAERS 2004Q1-2024Q2\\2012q4-2024q2")

files <- list.files(pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
drug_files_1224<-files[grepl("drug",files,ignore.case = TRUE)]
reac_files_1224<-files[grepl("reac",files,ignore.case = TRUE)]
indi_files_1224<-files[grepl("indi",files,ignore.case = TRUE)]
ther_files_1224<-files[grepl("ther",files,ignore.case = TRUE)]
outc_files_1224<-files[grepl("outc",files,ignore.case = TRUE)]

####ther1224
extract_ther_columns_1224 <- function(ther_files_1224) {
  df_ther_1224 <- lapply(ther_files_1224, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE,row.names = NULL)
    names(df) <- tolower(names(df))
    df <- df %>% select(primaryid,dsg_drug_seq,start_dt)
    return(df)
  }) %>% bind_rows()
  return(df_ther_1224)
}
df_ther_1224 <- extract_ther_columns_1224(ther_files_1224)
setDT(df_ther_1224)
df_ther_0424_initial <-rbind(df_ther_0412,df_ther_1224)
save(df_ther_0424_initial,file="df_ther_0424_initial.Rdata")

#######reac1224
extract_reac_columns_1224 <- function(reac_files_1224) {
  df_reac_1224 <- lapply(reac_files_1224, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE, row.names = NULL)
    df <- df %>% 
      select(primaryid,pt) %>%
      return(df)
  }) %>% bind_rows()  
  return(df_reac_1224)
}
df_reac_1224 <- extract_reac_columns_1224(reac_files_1224)

df_reac_1224 <- as.data.frame(df_reac_1224)
df_reac_0412_csv <- as.data.frame(df_reac_0412_csv)
df_reac_0424_initial <-rbind(df_reac_0412_csv,df_reac_1224)
save(df_reac_0424_initial,file="df_reac_0424_initial.Rdata")

#######indi1224
extract_indi_columns_1224 <- function(indi_files_1224) {
  df_indi_1224 <- rbindlist(lapply(indi_files_1224, function(file) {
    df <- fread(file, sep = "$", header = TRUE, na.strings = "")
    df[, indi_pt := trimws(tolower(indi_pt))]
    return(df[, .(primaryid,indi_drug_seq,indi_pt)])  
  }), fill = TRUE)  
  return(df_indi_1224)
}
df_indi_1224 <- extract_indi_columns_1224(indi_files_1224)

df_indi_1224 <- as.data.frame(df_indi_1224)
df_indi_0412 <- as.data.frame(df_indi_0412)
df_indi_0424_initial <-rbind(df_indi_0412,df_indi_1224)
setDT(df_indi_0424_initial)
save(df_indi_0424_initial,file="df_indi_0424_initial.Rdata")

########drug1224
extract_drug_columns_1224 <- function(drug_files_1224) {
  df_drug_1224 <- rbindlist(lapply(drug_files_1224, function(file) {
    df <- fread(file, sep = "$", colClasses = "character", na.strings = "")
    required_columns <- c("prod_ai", "role_cod", "drugname","drug_seq")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      df[, (missing_columns) := NA_character_]
    }
    df <- df[role_cod == "PS"]
    df[, prod_ai := trimws(tolower(prod_ai))]
    df[, drugname := trimws(tolower(drugname))]
    df <- df[, .(primaryid, prod_ai, drugname,drug_seq)]
    return(df)
  }), fill = TRUE)
  return(df_drug_1224)
}
df_drug_1224 <- extract_drug_columns_1224(drug_files_1224)

correct_order <- c("primaryid", "prod_ai","drugname","drug_seq")
df_drug_0412_csv <- df_drug_0412_csv[, ..correct_order]
df_drug_1224 <- df_drug_1224[, ..correct_order]
df_drug_0424_initial <- rbindlist(list(df_drug_0412_csv, df_drug_1224), use.names = TRUE, fill = TRUE)

save(df_drug_0424_initial,file="df_drug_0424_initial.Rdata")

######24q3
setwd('D:\\aaa\\FAERS 2004Q1-2025Q2\\2012q4-2024q3\\24q3')

files <- list.files(pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
drug_files_24q3<-files[grepl("drug",files,ignore.case = TRUE)]
reac_files_24q3<-files[grepl("reac",files,ignore.case = TRUE)]
indi_files_24q3<-files[grepl("indi",files,ignore.case = TRUE)]
ther_files_24q3<-files[grepl("ther",files,ignore.case = TRUE)]
outc_files_24q3<-files[grepl("outc",files,ignore.case = TRUE)]

####ther24q3
extract_ther_columns_24q3 <- function(ther_files_24q3) {
  df_ther_24q3 <- lapply(ther_files_24q3, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE,row.names = NULL)
    names(df) <- tolower(names(df))
    df <- df %>% select(primaryid,dsg_drug_seq,start_dt)
    return(df)
  }) %>% bind_rows()
  return(df_ther_24q3)
}

df_ther_24q3 <- extract_ther_columns_24q3(ther_files_24q3)
setDT(df_ther_24q3)
df_ther_0424q3_initial <-rbind(df_ther_0424,df_ther_24q3)
save(df_ther_0424q3_initial,file="df_ther_0424q3_initial.Rdata")

#######reac24q3
extract_reac_columns_24q3 <- function(reac_files_24q3) {
  df_reac_24q3 <- rbindlist(lapply(reac_files_24q3, function(file) {
    df <- fread(file, sep = "$", header = TRUE, na.strings = "")
    #cat("File:", file, "- Rows read:", nrow(df), "\n")
    df[, pt := trimws(tolower(pt))]
    return(df[, .(primaryid, pt)])  
  }),fill=TRUE)  
  return(df_reac_24q3)
}
df_reac_24q3 <- extract_reac_columns_24q3(reac_files_24q3)

df_reac_24q3 <- as.data.frame(df_reac_24q3)
df_reac_0424q3_initial <-rbind(df_reac_0424,df_reac_24q3)
save(df_reac_0424q3_initial,file="df_reac_0424q3_initial.Rdata")

#######indi24q3
extract_indi_columns_24q3 <- function(indi_files_24q3) {
  df_indi_24q3 <- rbindlist(lapply(indi_files_24q3, function(file) {
    df <- fread(file, sep = "$", header = TRUE, na.strings = "")
    df[, indi_pt := trimws(tolower(indi_pt))]
    return(df[, .(primaryid,indi_drug_seq,indi_pt)])  
  }), fill = TRUE)  
  return(df_indi_24q3)
}
df_indi_24q3 <- extract_indi_columns_24q3(indi_files_24q3)

df_indi_24q3 <- as.data.frame(df_indi_24q3)
df_indi_0424q3_initial <-rbind(df_indi_0424,df_indi_24q3)
setDT(df_indi_0424q3_initial)
save(df_indi_0424q3_initial,file="df_indi_0424q3_initial.Rdata")

########drug24q3
extract_drug_columns_24q3 <- function(drug_files_24q3) {
  df_drug_24q3 <- rbindlist(lapply(drug_files_24q3, function(file) {
    df <- fread(file, sep = "$", colClasses = "character", na.strings = "")
    required_columns <- c("prod_ai", "role_cod", "drugname","drug_seq")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      df[, (missing_columns) := NA_character_]
    }
    df <- df[role_cod == "PS"]
    df[, prod_ai := trimws(tolower(prod_ai))]
    df[, drugname := trimws(tolower(drugname))]
    df <- df[, .(primaryid, prod_ai, drugname,drug_seq)]
    return(df)
  }), fill = TRUE)
  return(df_drug_24q3)
}
df_drug_24q3 <- extract_drug_columns_24q3(drug_files_24q3)

correct_order <- c("primaryid", "prod_ai","drugname","drug_seq")
df_drug_0424a3_initial <- rbindlist(list(df_drug_0424, df_drug_24q3), use.names = TRUE, fill = TRUE)

save(df_drug_0424q3_initial,file="df_drug_0424q3_initial.Rdata")


#####indi==pt
df_reac_0424q3_unique_primaryid <- df_reac_0424q3_by_demo[, .(pt = paste0(pt, collapse = ";")), by = .(primaryid)]

df_indi_0424q3_unique_primaryid <- df_indi_0424q3_by_demo[, .(indi_pt = paste0(indi_pt, collapse = ";")), by = .(primaryid)]

library(pbapply)
sunique_fast_with_progress <- function(x, sep = ";") {
  pbsapply(strsplit(x, sep), function(y) paste(sort(unique(y)), collapse = sep))
}

df_reac_0424q3_unique_primaryid[, pt := sunique_fast_with_progress(pt)]
save(df_reac_0424q3_unique_primaryid,file="df_reac_0424q3_unique_primaryid.Rdata")

df_indi_0424q3_unique_primaryid[, indi_pt := sunique_fast_with_progress(indi_pt)]
save(df_indi_0424q3_unique_primaryid,file="df_indi_0424q3_unique_primaryid.Rdata")

reac_indi_0424q3 <- merge(df_reac_0424q3_unique_primaryid, df_indi_0424q3_unique_primaryid, by = "primaryid", all.x = TRUE)
save(reac_indi_0424q3,file="reac_indi_0424q3.Rdata")

pt_indi_same_0424q3 <- reac_indi_0424q3[pt == indi_pt,]
####114398
save(pt_indi_same_0424q3,file="pt_indi_same_0424q3.Rdata")

#####The initial data cleaning employed a time window anchored to the BT’s approval date, which was subsequently adjusted to account for the introduction of BS

####Out-BT-windows records

df_demo_0424q3_by_date <- df_demo_0424q3_by_caseid %>%
  filter(fda_dt >= 19980925)
save(df_demo_0424q3_by_date,file="C:\\Users\\86138\\Desktop\\code\\df_demo_0424q3_by_date.Rdata")
###18278469


#####Duplication records (by indi_pt and pt)

df_demo_0424q3_final <- df_demo_0424q3_by_date[!primaryid %in% pt_indi_same_0424q3$primaryid]
save(df_demo_0424q3_final,file="C:\\Users\\86138\\Desktop\\code\\df_demo_0424q3_final.Rdata")
###18164071


###by BS date：Out-BS-windows records

fda_dt_other <-df_demo_0424q3_final[primaryid %in% other_drug_reac_newdate$primaryid,fda_dt]
min(fda_dt_other)
##2018-01-31

df_demo_final_newdate <- df_demo_0424q3_final %>%
  filter(fda_dt >= 20180131)
##9712139
save(df_demo_final_newdate,file="C:\\Users\\86138\\Desktop\\code\\df_demo_final_newdate.Rdata")


####################JADER
######reac
jader_reac_202501 <- read.csv("C:\\Users\\11111\\Desktop\\jader\\reac202501.csv",
                              fileEncoding = "Shift-JIS")

setDT(jader_drug_202501)
rows_with_semicolon <- grepl(";", jader_drug_202501$投与開始日)
jader_drug_with_semicolon <- jader_drug_202501[rows_with_semicolon, ]
jader_drug_without_semicolon_sp <-jader_drug_202501[!rows_with_semicolon, ]

jader_drug_with_semicolon_sp <- jader_drug_with_semicolon %>%
  separate_rows(`投与開始日`, sep = ";") %>%
  ungroup() 

jader_drug_with_semicolon_sp$投与開始日 <- as.numeric(jader_drug_with_semicolon_sp$投与開始日)
jader_drug_without_semicolon_sp$投与開始日 <-as.numeric(jader_drug_without_semicolon_sp$投与開始日)

library(tidyverse)
library(lubridate) 

ja_drug_new1 <- jader_drug_with_semicolon_sp %>%
  mutate(
    年 = as.numeric(str_extract(投与開始日, "^\\d{4}")),
    月 = as.numeric(str_extract(投与開始日, "(?<=-)\\d{1,2}(?=-)")),
    日 = as.numeric(str_extract(投与開始日, "(?<=-)\\d{1,2}$"))
  ) %>%
  group_by(識別番号, 医薬品.一般名., 医薬品.販売名.) %>%
  arrange(desc(年), desc(月), desc(日)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-年, -月, -日)

ja_drug_new2 <- jader_drug_without_semicolon_sp %>%
  mutate(
    年 = as.numeric(str_extract(投与開始日, "^\\d{4}")),
    月 = as.numeric(str_extract(投与開始日, "(?<=-)\\d{1,2}(?=-)")),
    日 = as.numeric(str_extract(投与開始日, "(?<=-)\\d{1,2}$"))
  ) %>%
  group_by(識別番号, 医薬品.一般名., 医薬品.販売名.) %>%
  arrange(desc(年), desc(月), desc(日)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-年, -月, -日)

setDT(ja_drug_new1)
setDT(ja_drug_new2)
ja_drug_new1[,rn:=NULL]

ja_drug_after_duplicate <-rbindlist(list(ja_drug_new1,ja_drug_new2))

setDT(jader_reac_202501)
setDT(ja_drug_after_duplicate)
ja_drug_reac_202501<- left_join(ja_drug_after_duplicate,jader_reac_202501,by="識別番号")


##############CVARD

report_links <-read.delim("report_links.txt", sep = "$", header = FALSE, row.names = NULL)
duplicate_links <- data.frame(report_id=report_links[,2],duplicate_or_link=report_links[,3])

links <- duplicate_links %>%
  filter(!duplicate_or_link== "Duplicate")
duplicate <-duplicate_links %>%
  filter(duplicate_or_link== "Duplicate")

reports <- read.delim("reports.txt", sep = "$", header = FALSE, row.names = NULL)
report <- data.frame(report_id = reports[, 1],datreceived=reports[,4],version_no=reports[,3],report_no=reports[, 2],occp_cod=reports[,35])
setDT(report)
report$report_id <-as.character(report$report_id)
report$report_no <-as.character(report$report_no)


#######
report_after_dulpicate <-report%>%
  filter(!report_id %in% duplicate$report_id )

indication_unique <- indication[, .(indi = paste0(indi, collapse = ";")), by = .(report_id)]
sunique <- function(x, sep = ";") {
  x %>% str_split(sep) %>% lapply(., unique) %>% lapply(., sort) %>% sapply(., paste, collapse = ";")
}
indication_unique[, indi := indi %>% sunique]
setDT(indication_unique)
indication_unique$report_id <-as.character(indication_unique$report_id)

reactions <- read.delim("reactions.txt", sep = "$", header = FALSE, row.names = NULL)
reac <- data.table(report_id = reactions[, 2],pt=reactions[,6],soc=reactions[,8])
reac[, pt := trimws(tolower(pt))]
reac[, soc := trimws(tolower(soc))]
reac$pt <- gsub("^\\$", "", reac$pt)
reac$pt <- gsub("\\$$", "", reac$pt)
reac$report_id <-as.character(reac$report_id)

reac_unique <- reac[, .(pt = paste0(pt, collapse = ";")), by = .(report_id)]
reac_unique[, pt := pt %>% sunique]

reac_indi <- merge(reac_unique, indication_unique, by = "report_id", all.x = TRUE)
pt_indi_same <- reac_indi[pt == indi,]

setDT(report_after_dulpicate)
report_final <-report_after_dulpicate[!report_after_dulpicate$report_id %in% pt_indi_same$report_id]
report_final$report_id <-as.character(report_final$report_id)
report_final$report_no <-as.character(report_final$report_no)


report_drug <- read.delim("report_drug.txt", sep = "$", header = FALSE, row.names = NULL)
drug <- data.table(drugname = report_drug[, 4],report_id = report_drug[, 2],)

