#####################################################################################################
#' Description: Debt to Government phase 2
#'
#' Input: Tidied debt table for D2GP2 project ---household
#'        [d2gP2_tidy_table_hhld]
#'
#' Output: Excel/CSV file of monthly debt
#' 
#' Author:  Freya Li
#' 
#' Dependencies: dbplyr_helper_functions.R, utility_functions.R, table_consistency_checks.R
#' 
#' Issues:
#' 
#' History (reverse order):
#' 2021-09-22 SA review
#' 2021-07-09 FL household
#' 2021-06-22 FL 
#' 2021-06-16 SA for DPMC June request
#' 2021-04-30 FL 
#####################################################################################################
# locations
PROJECT_FOLDER = "~/Network-Shares/DataLabNas/MAA/MAA2020-01/debt to government_Phase2/rprogs"
SANDPIT = "[IDI_Sandpit]"
USERCODE = "[IDI_UserCode]"
OUR_SCHEMA = "[DL-MAA2020-01]"

# inputs
TIDY_TABLE = "[d2gP2_tidy_table_hhld]"

# outputs
RESULTS_FILE_SUM = "[d2gP2_debt_summary_hhld]"
RESULTS_FILE_CNT = "[d2gP2_debt_count_hhld]"
# controls
DEVELOPMENT_MODE = FALSE
VERBOSE = "details" # {"all", "details", "heading", "none"}

## setup ------------------------------------------------------------------------------------------

setwd(PROJECT_FOLDER)
source("utility_functions.R")
source("dbplyr_helper_functions.R")
source("table_consistency_checks.R")

run_time_inform_user("beginning set up", context = "heading", print_level = VERBOSE)

## specific columns of interest -------------------------------------------------------------------
#### columns to use for every grouping ----
always_grouping_columns = c(
  "moj_debtor",
  "msd_debtor",
  "ird_debtor",
  "ben_ind",
  "low_income_hhld",
  "hhld_child"
)

#### columns to sum & count positive values ----
debt_values = c(
  "moj_Y2019Jan",
  "moj_Y2019Feb",
  "moj_Y2019Mar",
  "moj_Y2019Apr",
  "moj_Y2019May",
  "moj_Y2019Jun",
  "moj_Y2019Jul",
  "moj_Y2019Aug",
  "moj_Y2019Sep",
  "moj_Y2019Oct",
  "moj_Y2019Nov",
  "moj_Y2019Dec",
  "moj_Y2020Jan",
  "moj_Y2020Feb",
  "moj_Y2020Mar",
  "moj_Y2020Apr",
  "moj_Y2020May",
  "moj_Y2020Jun",
  "moj_Y2020Jul",
  "moj_Y2020Aug",
  "moj_Y2020Sep",
  "moj_pre_2019",
  "moj_principle_2019",
  "moj_principle_2020",
  "moj_penalty_2019",
  "moj_penalty_2020",
  "moj_payment_2019",
  "moj_payment_2020",
  "moj_write_off_2019",
  "moj_write_off_2020",
  "moj_reversal_2019",
  "moj_reversal_2020",
  "moj_Y2020Sep_fine",
  "moj_pre_2019_fine",
  "moj_principle_2019_fine",
  "moj_principle_2020_fine",
  "moj_penalty_2019_fine",
  "moj_penalty_2020_fine",
  "moj_payment_2019_fine",
  "moj_payment_2020_fine",
  "moj_remittal_2019_fine",
  "moj_remittal_2020_fine",
  "moj_reversal_2019_fine",
  "moj_reversal_2020_fine",
  "moj_Y2020Sep_fcco",
  "moj_pre_2019_fcco",
  "moj_principle_2019_fcco",
  "moj_principle_2020_fcco",
  "moj_payment_2019_fcco",
  "moj_payment_2020_fcco",
  "moj_write_off_2019_fcco",
  "moj_write_off_2020_fcco",
  "moj_payment_2019_AO",
  "moj_payment_2020_AO",
  
  "msd_Y2019Jan",
  "msd_Y2019Feb",
  "msd_Y2019Mar",
  "msd_Y2019Apr",
  "msd_Y2019May",
  "msd_Y2019Jun",
  "msd_Y2019Jul",
  "msd_Y2019Aug",
  "msd_Y2019Sep",
  "msd_Y2019Oct",
  "msd_Y2019Nov",
  "msd_Y2019Dec",
  "msd_Y2020Jan",
  "msd_Y2020Feb",
  "msd_Y2020Mar",
  "msd_Y2020Apr",
  "msd_Y2020May",
  "msd_Y2020Jun",
  "msd_Y2020Jul",
  "msd_Y2020Aug",
  "msd_Y2020Sep",
  "msd_pre_2019",
  "msd_principle_2019",
  "msd_principle_2020",
  "msd_payment_2019",
  "msd_payment_2020",
  "msd_write_off_2019",
  "msd_write_off_2020",
  "msd_Y2020Sep_overpayment",
  "msd_pre_2019_OV",
  "msd_principle_2019_OV",
  "msd_principle_2020_OV",
  "msd_pay_write_2019_OV",
  "msd_pay_write_2020_OV",
  "msd_Y2020Sep_assistance",
  "msd_pre_2019_RA",
  "msd_principle_2019_RA",
  "msd_principle_2020_RA",
  "msd_pay_write_2019_RA",
  "msd_pay_write_2020_RA",
  
  "ird_Y2019Jan",
  "ird_Y2019Feb",
  "ird_Y2019Mar",
  "ird_Y2019Apr",
  "ird_Y2019May",
  "ird_Y2019Jun",
  "ird_Y2019Jul",
  "ird_Y2019Aug",
  "ird_Y2019Sep",
  "ird_Y2019Oct",
  "ird_Y2019Nov",
  "ird_Y2019Dec",
  "ird_Y2020Jan",
  "ird_Y2020Feb",
  "ird_Y2020Mar",
  "ird_Y2020Apr",
  "ird_Y2020May",
  "ird_Y2020Jun",
  "ird_Y2020Jul",
  "ird_Y2020Aug",
  "ird_Y2020Sep",
  "ird_pre_2019",
  "ird_principle_2019",
  "ird_principle_2020",
  "ird_penalty_2019",
  "ird_penalty_2020",
  "ird_interest_2019",
  "ird_interest_2020",
  "ird_payment_2019",
  "ird_payment_2020",
  "ird_remission_2019",
  "ird_remission_2020",
  "ird_maintain_neg_2019",
  "ird_maintain_pos_2019",
  "ird_maintain_neg_2020",
  "ird_maintain_pos_2020",
  "ird_Y2020Sep_Donation_Tax_Credits",
  "ird_pre_2019_DTC",
  "ird_principle_2019_DTC",
  "ird_penalty_2019_DTC",
  "ird_interest_2019_DTC",
  "ird_maintain_pos_2019_DTC",
  "ird_payment_2019_DTC",
  "ird_remission_2019_DTC",
  "ird_maintain_neg_2019_DTC",
  "ird_principle_2020_DTC",
  "ird_penalty_2020_DTC",
  "ird_interest_2020_DTC",
  "ird_maintain_pos_2020_DTC",
  "ird_payment_2020_DTC",
  "ird_remission_2020_DTC",
  "ird_maintain_neg_2020_DTC",
  
  "ird_Y2020Sep_Employment_Activities",
  "ird_pre_2019_EA",
  "ird_principle_2019_EA",
  "ird_penalty_2019_EA",
  "ird_interest_2019_EA",
  "ird_maintain_pos_2019_EA",
  "ird_payment_2019_EA",
  "ird_remission_2019_EA",
  "ird_maintain_neg_2019_EA",
  "ird_principle_2020_EA",
  "ird_penalty_2020_EA",
  "ird_interest_2020_EA",
  "ird_maintain_pos_2020_EA",
  "ird_payment_2020_EA",
  "ird_remission_2020_EA",
  "ird_maintain_neg_2020_EA",
  "ird_Y2020Sep_GST",
  "ird_pre_2019_GST",
  "ird_principle_2019_GST",
  "ird_penalty_2019_GST",
  "ird_interest_2019_GST",
  "ird_maintain_pos_2019_GST",
  "ird_payment_2019_GST",
  "ird_remission_2019_GST",
  "ird_maintain_neg_2019_GST",
  "ird_principle_2020_GST",
  "ird_penalty_2020_GST",
  "ird_interest_2020_GST",
  "ird_maintain_pos_2020_GST",
  "ird_payment_2020_GST",
  "ird_remission_2020_GST",
  "ird_maintain_neg_2020_GST",
  "ird_Y2020Sep_Other",
  "ird_pre_2019_Other",
  "ird_principle_2019_Other",
  "ird_penalty_2019_Other",
  "ird_interest_2019_Other",
  "ird_maintain_pos_2019_Other",
  "ird_payment_2019_Other",
  "ird_remission_2019_Other",
  "ird_maintain_neg_2019_Other",
  "ird_principle_2020_Other",
  "ird_penalty_2020_Other",
  "ird_interest_2020_Other",
  "ird_maintain_pos_2020_Other",
  "ird_payment_2020_Other",
  "ird_remission_2020_Other",
  "ird_maintain_neg_2020_Other",
  "ird_Y2020Sep_Receiving_Carer",
  "ird_pre_2019_RC",
  "ird_principle_2019_RC",
  "ird_penalty_2019_RC",
  "ird_interest_2019_RC",
  "ird_maintain_pos_2019_RC",
  "ird_payment_2019_RC",
  "ird_remission_2019_RC",
  "ird_maintain_neg_2019_RC",
  "ird_principle_2020_RC",
  "ird_penalty_2020_RC",
  "ird_interest_2020_RC",
  "ird_maintain_pos_2020_RC",
  "ird_payment_2020_RC",
  "ird_remission_2020_RC",
  "ird_maintain_neg_2020_RC",
  
  "ird_Y2020Sep_Income_Tax",
  "ird_pre_2019_IT",
  "ird_principle_2019_IT",
  "ird_principle_2020_IT",
  "ird_penalty_2019_IT",
  "ird_penalty_2020_IT",
  "ird_interest_2019_IT",
  "ird_interest_2020_IT",
  "ird_payment_2019_IT",
  "ird_payment_2020_IT",
  "ird_remission_2019_IT",
  "ird_remission_2020_IT",
  "ird_maintain_neg_2019_IT",
  "ird_maintain_pos_2019_IT",
  "ird_maintain_neg_2020_IT",
  "ird_maintain_pos_2020_IT",
  "ird_Y2020Sep_Liable_Parent",
  "ird_pre_2019_LP",
  "ird_principle_2019_LP",
  "ird_principle_2020_LP",
  "ird_penalty_2019_LP",
  "ird_penalty_2020_LP",
  "ird_interest_2019_LP",
  "ird_interest_2020_LP",
  "ird_payment_2019_LP",
  "ird_payment_2020_LP",
  "ird_remission_2019_LP",
  "ird_remission_2020_LP",
  "ird_maintain_neg_2019_LP",
  "ird_maintain_pos_2019_LP",
  "ird_maintain_neg_2020_LP",
  "ird_maintain_pos_2020_LP",
  "ird_Y2020Sep_Families",
  "ird_pre_2019_Families",
  "ird_principle_2019_Families",
  "ird_principle_2020_Families",
  "ird_penalty_2019_Families",
  "ird_penalty_2020_Families",
  "ird_interest_2019_Families",
  "ird_interest_2020_Families",
  "ird_payment_2019_Families",
  "ird_payment_2020_Families",
  "ird_remission_2019_families",
  "ird_remission_2020_Families",
  "ird_maintain_neg_2019_families",
  "ird_maintain_pos_2019_families",
  "ird_maintain_neg_2020_Families",
  "ird_maintain_pos_2020_Families",
  "ird_Y2020Sep_Student_Loan",
  "ird_pre_2019_SL",
  "ird_principle_2019_SL",
  "ird_principle_2020_SL",
  "ird_penalty_2019_SL",
  "ird_penalty_2020_SL",
  "ird_interest_2019_SL",
  "ird_interest_2020_SL",
  "ird_payment_2019_SL",
  "ird_payment_2020_SL",
  "ird_remission_2019_SL",
  "ird_remission_2020_SL",
  "ird_maintain_neg_2019_SL",
  "ird_maintain_pos_2019_SL",
  "ird_maintain_neg_2020_SL",
  "ird_maintain_pos_2020_SL"
  
)


#### columns to count ----

demographics = c(
  "days_disability_ben_365",
  "census_dsblty_ind",

  "moj_payment_3mth",
  "moj_payment_6mth",
  "moj_payment_9mth",
  "moj_payment_12mth",
  "moj_persistence_3mth",
  "moj_persistence_6mth",
  "moj_persistence_9mth",
  "moj_persistence_12mth",
  "moj_persistence_15mth",
  "moj_persistence_18mth",
  "moj_persistence_21mth",
  "msd_payment_3mth",
  "msd_payment_6mth",
  "msd_payment_9mth",
  "msd_payment_12mth",
  "msd_persistence_3mth",
  "msd_persistence_6mth",
  "msd_persistence_9mth",
  "msd_persistence_12mth",
  "msd_persistence_15mth",
  "msd_persistence_18mth",
  "msd_persistence_21mth",
  "ird_payment_3mth",
  "ird_payment_6mth",
  "ird_payment_9mth",
  "ird_payment_12mth",
  "ird_persistence_3mth",
  "ird_persistence_6mth",
  "ird_persistence_9mth",
  "ird_persistence_12mth",
  "ird_persistence_15mth",
  "ird_persistence_18mth",
  "ird_persistence_21mth",
  
  "pay_vs_income_moj=0-.005",
  "pay_vs_income_moj=0.005-.01",
  "pay_vs_income_moj=0.01-.0.05",
  "pay_vs_income_moj=0.05-.1",
  "pay_vs_income_moj=0.1-.2",
  "pay_vs_income_moj=.2-.4",
  "pay_vs_income_moj=.4-.7",
  "pay_vs_income_moj=.7-1",
  "pay_vs_income_moj>1",
  "pay_vs_income_msd=0-.005",
  "pay_vs_income_msd=0.005-.01",
  "pay_vs_income_msd=0.01-.0.05",
  "pay_vs_income_msd=0.05-.1",
  "pay_vs_income_msd=0.1-.2",
  "pay_vs_income_msd=.2-.4",
  "pay_vs_income_msd=.4-.7",
  "pay_vs_income_msd=.7-1",
  "pay_vs_income_msd>1",
  "pay_vs_income_ird=0-.005",
  "pay_vs_income_ird=0.005-.01",
  "pay_vs_income_ird=0.01-.0.05",
  "pay_vs_income_ird=0.05-.1",
  "pay_vs_income_ird=0.1-.2",
  "pay_vs_income_ird=.2-.4",
  "pay_vs_income_ird=.4-.7",
  "pay_vs_income_ird=.7-1",
  "pay_vs_income_ird>1",
  "pay_vs_income_all=0-.005",
  "pay_vs_income_all=0.005-.01",
  "pay_vs_income_all=0.01-.0.05",
  "pay_vs_income_all=0.05-.1",
  "pay_vs_income_all=0.1-.2",
  "pay_vs_income_all=.2-.4",
  "pay_vs_income_all=.4-.7",
  "pay_vs_income_all=.7-1",
  "pay_vs_income_all>1",
  
  "debt_vs_income_moj=0-.005",
  "debt_vs_income_moj=0.005-.01",
  "debt_vs_income_moj=0.01-.0.05",
  "debt_vs_income_moj=0.05-.1",
  "debt_vs_income_moj=0.1-.2",
  "debt_vs_income_moj=.2-.4",
  "debt_vs_income_moj=.4-.7",
  "debt_vs_income_moj=.7-1",
  "debt_vs_income_moj>1",
  "debt_vs_income_msd=0-.005",
  "debt_vs_income_msd=0.005-.01",
  "debt_vs_income_msd=0.01-.0.05",
  "debt_vs_income_msd=0.05-.1",
  "debt_vs_income_msd=0.1-.2",
  "debt_vs_income_msd=.2-.4",
  "debt_vs_income_msd=.4-.7",
  "debt_vs_income_msd=.7-1",
  "debt_vs_income_msd>1",
  "debt_vs_income_ird=0-.005",
  "debt_vs_income_ird=0.005-.01",
  "debt_vs_income_ird=0.01-.0.05",
  "debt_vs_income_ird=0.05-.1",
  "debt_vs_income_ird=0.1-.2",
  "debt_vs_income_ird=.2-.4",
  "debt_vs_income_ird=.4-.7",
  "debt_vs_income_ird=.7-1",
  "debt_vs_income_ird>1",
  "debt_vs_income_all=0-.005",
  "debt_vs_income_all=0.005-.01",
  "debt_vs_income_all=0.01-.0.05",
  "debt_vs_income_all=0.05-.1",
  "debt_vs_income_all=0.1-.2",
  "debt_vs_income_all=.2-.4",
  "debt_vs_income_all=.4-.7",
  "debt_vs_income_all=.7-1",
  "debt_vs_income_all>1"
)

## summary functions ------------------------------------------------------------------------------

#### count values function ----

count_values = function(input_table, grouping_cols, focus_col){
  assert(is.character(grouping_cols), "grouping cols must be provided as character string")
  assert(is.character(focus_col), "focus col must be provided as character string")
  assert(length(focus_col) == 1, "only one focus col each function call")
  assert(is.tbl(input_table), "input table must be provided as tbl")
  assert(all(grouping_cols %in% colnames(input_table)), "some column names not found in table")
  assert(focus_col %in% colnames(input_table), "focus col not found in table")
  
  results = input_table %>%
    group_by(!!!syms(c(grouping_cols, focus_col))) %>%
    summarise(num = n(), .groups = "drop") %>%
    mutate(counted_col = focus_col) %>%
    rename(counted_col_value = !!sym(focus_col)) %>%
    collect() %>%
    select(all_of(c(grouping_cols, "counted_col", "counted_col_value", "num")))
}

#### sum & count positive values function ----

positives_count_sum_values = function(input_table, grouping_cols, focus_col){
  assert(is.character(grouping_cols), "grouping cols must be provided as character string")
  assert(is.character(focus_col), "focus col must be provided as character string")
  assert(length(focus_col) == 1, "only one focus col each function call")
  assert(is.tbl(input_table), "input table must be provided as tbl")
  assert(all(grouping_cols %in% colnames(input_table)), "some column names not found in table")
  assert(focus_col %in% colnames(input_table), "focus col not found in table")
  
  results = input_table %>%
    filter(!!sym(focus_col) > 0) %>%
    group_by(!!!syms(grouping_cols)) %>%
    summarise(num_pos = n(),
              sum_pos = sum(!!sym(focus_col), na.rm = TRUE),
              .groups = "drop") %>%
    mutate(summarised_col = focus_col) %>%
    collect() %>%
    select(all_of(c(grouping_cols, "summarised_col", "num_pos", "sum_pos")))
}

## connect to database ----------------------------------------------------------------------------

run_time_inform_user("accessing table", context = "heading", print_level = VERBOSE)

db_con = create_database_connection(database = "IDI_Sandpit")
working_table = create_access_point(db_con, SANDPIT, OUR_SCHEMA, TIDY_TABLE)

if(DEVELOPMENT_MODE)
  working_table = working_table %>% filter(identity_column %% 100 == 0)


## produce summary --------------------------------------------------------------------------------

run_time_inform_user("summarising", context = "heading", print_level = VERBOSE)

# lists to collect output
this_output_pos = list()
this_output_cnt = list()

# count all values & output
for(col in demographics){
  this_output_cnt[[col]] = count_values(working_table, always_grouping_columns, col)
  run_time_inform_user(glue::glue("summarising {col} complete"), context = "all", print_level = VERBOSE)
}

# count & sum all positive values
for(col in debt_values){
  this_output_pos[[col]] = positives_count_sum_values(working_table, always_grouping_columns, col)
  run_time_inform_user(glue::glue("summarising {col} complete"), context = "all", print_level = VERBOSE)
}

# close connection
close_database_connection(db_con)

## output results ---------------------------------------------------------------------------------

run_time_inform_user("outputing results", context = "heading", print_level = VERBOSE)

results_cnt = data.table::rbindlist(this_output_cnt,fill=TRUE) %>%
  tidyr::pivot_wider(names_from = "counted_col", values_from = "num") #%>%
#   filter(this_grouper ==  'total_debt_groups' | grouper_value == 1)
results_pos = data.table::rbindlist(this_output_pos,fill=TRUE) %>%
  tidyr::pivot_wider(names_from = "summarised_col", values_from = c("num_pos", "sum_pos")) #%>%
#   filter(this_grouper ==  'total_debt_groups' | grouper_value == 1)

results_cnt = results_cnt%>%
  filter(counted_col_value == 1)

save(results_cnt, file = "results_cnt_hhld.RData")
save(results_pos, file = "results_pos_hhld.RData")

write.csv(results_cnt, "results_cnt_hhld.csv")
write.csv(results_pos, "results_pos_hhld.csv")

run_time_inform_user("GRAND COMPLETION", context = "heading", print_level = VERBOSE)
