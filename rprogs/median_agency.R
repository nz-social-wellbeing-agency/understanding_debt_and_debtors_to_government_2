PROJECT_FOLDER = "~/Network-Shares/DataLabNas/MAA/MAA2020-01/debt to government_Phase2/rprogs"
SANDPIT = "[IDI_Sandpit]"
USERCODE = "[IDI_UserCode]"
OUR_SCHEMA = "[DL-MAA2020-01]"

# inputs
# TIDY_TABLE = "[d2gP2_tidy_table_July]"
TIDY_TABLE = "[d2gP2_tidy_table_hhld]"

## setup ------------------------------------------------------------------------------------------

setwd(PROJECT_FOLDER)
source("utility_functions.R")
source("dbplyr_helper_functions.R")
source("table_consistency_checks.R")

library("xlsx")
## access dataset ---------------------------------------------------------------------------------

db_con = create_database_connection(database = "IDI_Sandpit")
working_table = create_access_point(db_con, SANDPIT, OUR_SCHEMA, TIDY_TABLE)

## overall counts for each agency
{
  c_all_ird = working_table %>%
    filter(ird_debtor == 1)%>%
    summarise(cnt = n(),
              moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
              msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
              ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
    mutate(group = "all_debtor",
           debtor = "ird")%>%
    select(debtor, group, cnt, moj, msd, ird)%>%
    collect()
  
  c_all_msd = working_table %>%
    filter(msd_debtor == 1)%>%
    summarise(cnt = n(),
              moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
              msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
              ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
    mutate(group = "all_debtor",
           debtor = "msd")%>%
    select(debtor, group, cnt,moj,msd,ird)%>%
    collect()
  
  c_all_moj = working_table %>%
    filter(moj_debtor == 1)%>%
    summarise(cnt = n(),
              moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
              msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
              ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
    mutate(group = "all_debtor",
           debtor = "moj")%>%
    select(debtor, group, cnt,moj,msd,ird)%>%
    collect()
  c_all = rbind(c_all_ird, c_all_msd, c_all_moj)
}


##################################################################################################################

##overall median numbers for each agency

{
m_all_ird = working_table %>%
  filter(ird_debtor == 1)%>%
  mutate( median = median(ird_Y2020Sep))%>%
  select(median)%>%
  distinct()%>%
  mutate(group = "all_debtor",
         debtor = "ird")%>%
  select(debtor, group, median)%>%
  collect()

m_all_msd = working_table %>%
  filter(msd_debtor == 1)%>%
  mutate( median = median(msd_Y2020Sep))%>%
  select(median)%>%
  distinct()%>%
  mutate(group = "all_debtor",
         debtor = "msd")%>%
  select(debtor, group, median)%>%
  collect()

m_all_moj = working_table %>%
  filter(moj_debtor == 1)%>%
  mutate( median = median(moj_Y2020Sep))%>%
  select(median)%>%
  distinct()%>%
  mutate(group = "all_debtor",
         debtor = "moj")%>%
  select(debtor, group, median)%>%
  collect()
m_all = rbind(m_all_ird, m_all_msd, m_all_moj)
  }


##################################################################################
## beneficiary,low incoe, in hhld with kids count for each agency
# filter_group = c("ben_ind", "low_income", "child_at_address")
# filter_group = c("ben_ind", "low_income_hhld", "hhld_child")

c_fun <- function(flt) {
  c_ird = working_table %>%
    filter(!!sym(flt)==1 & ird_debtor == 1)%>%
    summarise(cnt = n(),
              moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
              msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
              ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
    mutate(group = flt,
           debtor = "ird")%>%
    select(debtor, group, cnt,moj,msd,ird)%>%
    collect()
  
  c_msd = working_table %>%
    filter(!!sym(flt)==1 & msd_debtor == 1)%>%
    summarise(cnt = n(),
              moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
              msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
              ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
    mutate(group = flt,
           debtor = "msd")%>%
    select(debtor, group, cnt,moj,msd,ird)%>%
    collect()
  
  c_moj = working_table %>%
    filter(!!sym(flt)==1 & moj_debtor == 1)%>%
    summarise(cnt = n(),
              moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
              msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
              ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
    mutate(group = flt,
           debtor = "moj")%>%
    select(debtor, group, cnt,moj,msd,ird)%>%
    collect()
  c = rbind(c_ird, c_msd, c_moj)
}

c_ben <- c_fun("ben_ind")
c_low <- c_fun("low_income_hhld")
c_kid <- c_fun("hhld_child")

##################################################################################
##beneficiary, low_income, in hhld with kids median numbers for each agency

m_fun <- function(flt) {
  m_ird = working_table %>%
    filter(!!sym(flt)==1 & ird_debtor == 1)%>%
    mutate( median = median(ird_Y2020Sep))%>%
    select(median)%>%
    distinct()%>%
    mutate(group = flt,
           debtor = "ird")%>%
    select(debtor, group, median)%>%
    collect()
  
  m_msd = working_table %>%
    filter(!!sym(flt)==1 & msd_debtor == 1)%>%
    mutate( median = median(msd_Y2020Sep))%>%
    select(median)%>%
    distinct()%>%
    mutate(group = flt,
           debtor = "msd")%>%
    select(debtor, group, median)%>%
    collect()
  
  m_moj = working_table %>%
    filter(!!sym(flt)==1 & moj_debtor == 1)%>%
    mutate( median = median(moj_Y2020Sep))%>%
    select(median)%>%
    distinct()%>%
    mutate(group = flt,
           debtor = "moj")%>%
    select(debtor, group, median)%>%
    collect()
  m = rbind(m_ird, m_msd, m_moj)
}

m_ben <- m_fun("ben_ind")
m_low <- m_fun("low_income_hhld")
m_kid <- m_fun("hhld_child")

####################################################################################
##overall
##count
count_all = working_table %>%
  filter(moj_debtor==1 | msd_debtor ==1 | ird_debtor==1)%>%
  summarise(cnt = n(),
            moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
            msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
            ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
  mutate(debtor = "overall",
         group = "all_debtor")%>%
  select(debtor,group,cnt,moj,msd,ird)%>%
  collect()

results_cnt_blt <- function(flt){
  results_count_ben = working_table %>%
    filter(!!sym(flt) ==1 & (moj_debtor==1 | msd_debtor ==1 | ird_debtor==1))%>%
    summarise(cnt = n(),
              moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
              msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
              ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
    mutate(debtor = "overall",
           group = flt)%>%
    select(debtor,group,cnt,moj,msd,ird)%>%
    collect()
}

cnt_ben<-results_cnt_blt("ben_ind")
cnt_low<-results_cnt_blt("low_income_hhld")
cnt_kid<-results_cnt_blt("hhld_child")


## median
median_all = working_table %>%
  filter(moj_debtor==1 | msd_debtor ==1 | ird_debtor==1)%>%
  mutate(all_type = coalesce(moj_Y2020Sep,0) + coalesce(msd_Y2020Sep,0) + coalesce(ird_Y2020Sep, 0))%>%
  mutate( median = median(all_type))%>%
  select(median)%>%
  distinct()%>%
  mutate(debtor = "overall",
         group = "all_debtor")%>%
  select(debtor,group,median)%>%
  collect()

results_median_blt <- function(flt){
  results_median_ben = working_table %>%
    filter(!!sym(flt) ==1 & (moj_debtor==1 | msd_debtor ==1 | ird_debtor==1))%>%
    mutate(all_type = coalesce(moj_Y2020Sep,0) + coalesce(msd_Y2020Sep,0) + coalesce(ird_Y2020Sep, 0))%>%
    mutate( median = median(all_type))%>%
    select(median)%>%
    distinct()%>%
    mutate(debtor = "overall",
           group = flt)%>%
    select(debtor,group,median)%>%
    collect()
}

median_ben<-results_median_blt("ben_ind")
median_low<-results_median_blt("low_income_hhld")
median_kid<-results_median_blt("hhld_child")


## conclude

count_all <- rbind(c_all,count_all, c_ben,cnt_ben, c_low, cnt_low, c_kid, cnt_kid)
median_all <- rbind(m_all,median_all, m_ben,median_ben, m_low,median_low, m_kid, median_kid)

output <- left_join(count_all, median_all,by = c("debtor" = "debtor", "group"="group"))


output <- as.data.frame(output)
file<-'summary_median_overall.xlsx'
write.xlsx(output, file, sheetName = "individual",row.names = FALSE)

