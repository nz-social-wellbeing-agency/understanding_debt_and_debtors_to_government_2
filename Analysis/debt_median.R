#####################################################################################################
#' Description: Statistics for Venn plot
#'
#' Input: Tidied debt table for D2GP2 project -- [d2gP2_tidy_table_July]
#'
#' Output: Excel/CSV file of summaries by BLC, cnt, total median
#' 
#' Author:  Freya Li
#' 
#' Dependencies: dbplyr_helper_functions.R, utility_functions.R, table_consistency_checks.R
#' 
#' Issues:

#' 
#' History (reverse order):
#' 2021-07-21 FL 
#####################################################################################################

PROJECT_FOLDER = "~/Network-Shares/DataLabNas/MAA/MAA2020-01/debt to government_Phase2/rprogs"
SANDPIT = "[IDI_Sandpit]"
USERCODE = "[IDI_UserCode]"
OUR_SCHEMA = "[DL-MAA2020-01]"

# inputs
TIDY_TABLE = "[d2gP2_tidy_table_July]"
TIDY_TABLE = "[d2gP2_tidy_table_hhld]"

# controls
DEVELOPMENT_MODE = FALSE
VERBOSE = "all" # {"all", "details", "heading", "none"}

## setup ------------------------------------------------------------------------------------------

setwd(PROJECT_FOLDER)
source("utility_functions.R")
source("dbplyr_helper_functions.R")
source("table_consistency_checks.R")

library("xlsx")
## access dataset ---------------------------------------------------------------------------------

db_con = create_database_connection(database = "IDI_Sandpit")
working_table = create_access_point(db_con, SANDPIT, OUR_SCHEMA, TIDY_TABLE)


# filter_group = c("ben_ind", "low_income", "child_at_address")
filter_group = c("ben_ind", "low_income_hhld", "hhld_child")


##count of debtor and amount of debt

summ_fun <- function(flt) {
  summ = working_table%>%
    filter(!!sym(flt) == 1)%>%
    group_by(moj_debtor, msd_debtor, ird_debtor)%>%
    summarise(cnt = n(),
              moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0), na.rm = TRUE),
              msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0), na.rm = TRUE),
              ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0), na.rm = TRUE))%>%
    mutate(group = flt)%>%
    select(moj_debtor, msd_debtor, ird_debtor, group, cnt, moj, msd, ird)%>%
    collect()
}


output = list()

for(flt in filter_group){
  
  output[[flt]] = summ_fun(flt)
  
}

results_summ_sub = data.table::rbindlist(output, use.names = TRUE, fill= TRUE)



results_summ_all <- working_table%>%
  group_by(moj_debtor, msd_debtor, ird_debtor)%>%
  summarise(cnt = n(),
            moj = sum(ifelse(moj_Y2020Sep>1, moj_Y2020Sep,0)),
            msd = sum(ifelse(msd_Y2020Sep>1, msd_Y2020Sep,0)),
            ird = sum(ifelse(ird_Y2020Sep>1, ird_Y2020Sep,0)))%>%
  mutate(group ="all_debtor")%>%
  select(moj_debtor, msd_debtor, ird_debtor, group, cnt, moj, msd, ird)%>%
  collect()


results_summ_all <- as.data.frame(results_summ_all)
results_summ <- rbind(results_summ_sub, results_summ_all)

##############################################################################################################################################
## median

median_fun <-function(flt){
  summ = working_table%>%
    filter(!!sym(flt) == 1)%>%
    group_by(moj_debtor, msd_debtor, ird_debtor)%>%
    mutate(sub_type = coalesce(moj_Y2020Sep,0) + coalesce(msd_Y2020Sep,0) + coalesce(ird_Y2020Sep, 0))%>%
    mutate(median = median(sub_type))%>%
    select(median)%>%
    distinct ()%>%
    mutate(group = flt)%>%
    select(moj_debtor, msd_debtor, ird_debtor, group, median)%>%
    collect()
}

output_m = list()

for(flt in filter_group){
  
  output_m[[flt]] = median_fun(flt)
  
}


results_median_sub = data.table::rbindlist(output_m, use.names = TRUE, fill= TRUE)


results_median_all = working_table %>%
  group_by(moj_debtor, msd_debtor, ird_debtor)%>%
  mutate(all_type = coalesce(moj_Y2020Sep,0) + coalesce(msd_Y2020Sep,0) + coalesce(ird_Y2020Sep, 0))%>%
  mutate( median = median(all_type))%>%
  select(median)%>%
  distinct()%>%
  mutate(group = "all_debtor")%>%
  select(moj_debtor, msd_debtor, ird_debtor, group, median)%>%
  collect()


results_median_all <- as.data.frame(results_median_all)
results_median <- rbind(results_median_sub, results_median_all)


individual_median <- results_summ %>%
  left_join(results_median, by = c("moj_debtor" = "moj_debtor",
                                   "msd_debtor" = "msd_debtor",
                                   "ird_debtor" = "ird_debtor",
                                   "group" = "group"))



file<-'summary_median.xlsx'
write.xlsx(individual_median, file, sheetName = "individual",row.names = FALSE,append = FALSE)