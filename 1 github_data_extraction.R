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

files <- list.files(pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
demo_files_1224<-files[grepl("demo",files,ignore.case = TRUE)]
drug_files_1224<-files[grepl("drug",files,ignore.case = TRUE)]
reac_files_1224<-files[grepl("reac",files,ignore.case = TRUE)]
indi_files_1224<-files[grepl("indi",files,ignore.case = TRUE)]
ther_files_1224<-files[grepl("ther",files,ignore.case = TRUE)]
outc_files_1224<-files[grepl("outc",files,ignore.case = TRUE)]
dele_files<-files[grepl("dele",files,ignore.case = TRUE)]
rpsr_files_1224<-files[grepl("rpsr",files,ignore.case = TRUE)]

extract_drug_columns_1224 <- function(drug_files_1224) {
  
  df_drug_1224 <- lapply(drug_files_1224, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE, row.names = NULL)
    required_columns<-c("prod_ai","role_cod","drugname")
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      df[missing_columns] <- NA
    }
    
    df[] <- lapply(df, as.character)
    
    df <- df %>% 
      select(primaryid,drug_seq,role_cod) %>%
      mutate(drug_therapy_link= paste0(primaryid, drug_seq))
    return(df)  
  }) %>% bind_rows()
  return(df_drug_1224)
}
df_drug_1224 <- extract_drug_columns_1224(drug_files_1224)

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

dele_caseids <- lapply(dele_files, function(file) {
  case_ids <- readLines(file)
  return(case_ids)
}) %>% unlist() 

age_multipliers <- c("DY" = 1/30.4, "DEC" = 120, "HR" = 1 / 730, "WK" = 1 / 4.3, "MON" = 1, "YR" = 12)
extract_demo_1224 <- function(demo_files_1224, dele_caseids) {
  
  
  handlers("progress")
  progress <- progressor(steps = 3)
  
  df_demo_1224 <- lapply(demo_files_1224, function(file) {
    df <- read_delim(file, delim = "$", col_types = cols())
    
    required_columns<-c("gndr_cod","age_cod","sex","fda_dt","event_dt","start_dt","occur_country",
                        "report_country","rept_dt","occp_cod"
    )
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
      df[missing_columns] <- NA
    }
    
    df[] <- lapply(df, as.character)
    
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
      mutate(fda_dt = as.Date(fda_dt, format = "%Y%m%d"),
             event_dt = as.Date(event_dt, format = "%Y%m%d"),
             start_dt = as.Date(start_dt, format = "%Y%m%d"),
             rept_dt = as.Date(rept_dt, format = "%Y%m%d"),
             age = as.numeric(age),
             age = round(age * recode(age_cod, !!!age_multipliers, .default = 0), 2)) 
    #filter(!is.na(age) & age >= 0 & (sex %in% c("F", "M","UNK","NS")))%>%
    #filter(!is.na(wt), wt >= 0)
    return(df)
  }) %>% 
    bind_rows() 
  progress("data")
  
  df_demo_1224 <- df_demo_1224 %>%
    filter(!caseid %in% dele_caseids)%>%
    
    df_demo_1224 <- df_demo_1224 %>%
    group_by(primaryid) %>%
    filter(fda_dt == max(fda_dt)) %>%  
    ungroup() %>%
    group_by(caseid) %>%
    filter(primaryid == max(primaryid[fda_dt == max(fda_dt)])) %>%  # 在 FDA_DT 相同的情况下选择较高的 PRIMARYID
    ungroup() %>%
    filter(!caseid %in% dele_caseids) %>%
    select(primaryid, sex, age, wt, fda_dt, event_dt, start_dt, occur_country, report_country, rept_dt, occp_cod)
  
  return(df_demo_1224)
}
with_progress({
  df_demo_1224 <- extract_demo_1224(demo_files_1224, dele_caseids)
})


extract_demo_columns_1224 <- function(demo_files_1224) {
  df_demo_1224 <- lapply(demo_files_1224, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE, row.names = NULL)
    df <- df %>% 
      select(primaryid,event_dt,fda_dt,rept_dt) %>%
      return(df)
  }) %>% bind_rows()  
  return(df_demo_1224)
}
df_demo_1224 <- extract_demo_columns_1224(demo_files_1224)

extract_indi_columns <- function(indi_files) {
  df_indi <- lapply(indi_files, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE,row.names = NULL)
    names(df) <- tolower(names(df))
    if ("isr" %in% names(df)) {
      df <- df %>% rename(primaryid = isr)
    }
    df <- df %>% select(primaryid,indi_pt)
    return(df)
  }) %>% bind_rows()
  return(df_indi)
}
df_indi <- extract_indi_columns(indi_files)

targeted_drug_indi<-df_indi%>%
  filter(primaryid %in% targeted_drug$primaryid)

extract_ther_columns <- function(ther_files_1224) {
  df_ther <- lapply(ther_files_1224, function(file) {
    df <- read.delim(file, sep = "$", header = TRUE,row.names = NULL)
    names(df) <- tolower(names(df))
    df <- df %>% select(primaryid,dsg_drug_seq,start_dt)
    return(df)
  }) %>% bind_rows()
  return(df_ther)
}
df_ther <- extract_ther_columns(ther_files_1224)

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


