#####################################################################################################
#' Description: Tidy assembled household data
#'
#' Input: Rectangular debt table produced by run_assembly for D2GP2 project
#'        ---> Equivalised hhld income.sql --  [d2gP2_rectangular_hhld_EquInc]
#'
#' Output: Tidied debt table for D2GP2 project
#' 
#' Author: Simon Anastasiadis
#' Modified: Freya Li
#' 
#' Dependencies: dbplyr_helper_functions.R, utility_functions.R, table_consistency_checks.R
#' 
#' Notes: 
#' 1. Low income household: less than 50% median equivalised household income
#' 2. Equivalied household income: devide household income by the scale factor calculated for each household 
#' 3. Scale factor: (1) Using a single-person household as a base, assign a value of 1 to the first adult in
#'    the household, 0.5 to each additional adult member, and 0.3 to each children
#'                  (2) Square root scale, taking the square root of the number of people in the household.
#' 
#' Issues:
#' 
#' History (reverse order):
#' 2021-09-22 SA review
#' 2021-07-09 FL V4 household 
#' 2021-06-22 FL V3 
#' 2021-06-15 SA V3 for DPMC June request
#' 2021-03-23 FL V2
#' 2020-06-24 SA v1
#####################################################################################################
## parameters -------------------------------------------------------------------------------------

# locations
PROJECT_FOLDER = "~/Network-Shares/DataLabNas/MAA/MAA2020-01/debt to government_Phase2/rprogs"
SANDPIT = "[IDI_Sandpit]"
USERCODE = "[IDI_UserCode]"
OUR_SCHEMA = "[DL-MAA2020-01]"

# inputs
ASSEMBLED_TABLE = "[d2gP2_rectangular_hhld_EquInc]"
# outputs
TIDY_TABLE = "[d2gP2_tidy_table_hhld]"

# controls
DEVELOPMENT_MODE = FALSE
VERBOSE = "details" # {"all", "details", "heading", "none"}

## setup ------------------------------------------------------------------------------------------

setwd(PROJECT_FOLDER)
source("utility_functions.R")
source("dbplyr_helper_functions.R")
source("table_consistency_checks.R")

## access dataset ---------------------------------------------------------------------------------

run_time_inform_user("GRAND START", context = "heading", print_level = VERBOSE)

db_con = create_database_connection(database = "IDI_Sandpit")

working_table = create_access_point(db_con, SANDPIT, OUR_SCHEMA, ASSEMBLED_TABLE)

if(DEVELOPMENT_MODE)
   working_table = working_table %>% filter(identity_column %% 100 == 0)

## error checking ---------------------------------------------------------------------------------

run_time_inform_user("error checks begun", context = "heading", print_level = VERBOSE)

# one person per period
assert_all_unique(working_table, c('identity_column', 'label_summary_period'))
# at least 1000 rows
assert_size(working_table, ">", 1000)

run_time_inform_user("error checks complete", context = "heading", print_level = VERBOSE)

## threshold for low household income -------------------------------------------------------------
#'
#' Using the entire table, calculate the median of the equivalised household income.
#' The threshold for low income is then set at half this value.
 
median_calculation_1 <- working_table %>%   
  summarise( median1 = median(equ_hhld_income1)) %>%
  collect()

LOW_INCOME_HHLD_THRESHOLD = 0.5 * median_calculation_1[1,1]

## check dataset variables ------------------------------------------------------------------------

# sort columns
# working_table = working_table %>%
#   select(sort(colnames(working_table)))

# during development inspect state of table
# explore::explore_shiny(collect(working_table))

## prep / cleaning in SQL -------------------------------------------------------------------------
working_table = working_table %>%

   mutate(
      # indicators for different debt types
      moj_debtor = ifelse(moj_Y2020Sep > 1, 1, 0),
      moj_fine_debtor =ifelse(moj_Y2020Sep_fine > 1, 1, 0),
      moj_fcco_debtor = ifelse(moj_Y2020Sep_fcco > 1, 1, 0),
      msd_debtor = ifelse(msd_Y2020Sep > 1, 1, 0),
      msd_ov_debtor = ifelse(msd_Y2020Sep_overpayment > 1, 1, 0),
      msd_ra_debtor = ifelse(msd_Y2020Sep_assistance > 1, 1, 0),
      ird_debtor = ifelse(ird_Y2020Sep > 1, 1, 0),
      ird_it_debtor = ifelse(ird_Y2020Sep_Income_Tax > 1, 1, 0),
      ird_lp_debtor = ifelse(ird_Y2020Sep_Liable_Parent > 1, 1, 0),
      ird_wff_debtor = ifelse(ird_Y2020Sep_Families > 1, 1, 0),
      ird_osl_debtor = ifelse(ird_Y2020Sep_Student_Loan > 1, 1, 0),
      #total_debt
      total_debt = coalesce(ifelse(moj_Y2020Sep > 1, moj_Y2020Sep, 0), 0) +
         coalesce(ifelse(msd_Y2020Sep > 1, msd_Y2020Sep, 0), 0) +
         coalesce(ifelse(ird_Y2020Sep > 1, ird_Y2020Sep, 0), 0),
      
      # other variables
      ben_ind = ifelse(ben_ind >= 1, 1, 0),
      hhld_child = ifelse(child_num_hhld >= 1, 1, 0),
      low_income_hhld = ifelse(is.na(equ_hhld_income1) | equ_hhld_income1 < LOW_INCOME_HHLD_THRESHOLD, 1, 0),
      
      days_disability_ben_365 = ifelse(is.na(days_disability_ben), NA,
                                       ifelse(days_disability_ben >= 365, 1, 0)),
      
      ##payment to income ratio
      pay_vs_income_moj = ifelse(coalesce(moj_payment_2019,0)+coalesce(moj_payment_2020,0) > 0 & total_income_ben > 1,
                                 round((coalesce(moj_payment_2019,0)+coalesce(moj_payment_2020,0))/(0.875*total_income_ben), 4),
                                 NA),
      pay_vs_income_msd = ifelse(coalesce(msd_payment_2019,0)+coalesce(msd_payment_2020,0) > 0 & total_income_ben > 1,
                                 round((coalesce(msd_payment_2019,0)+coalesce(msd_payment_2020,0))/(0.875*total_income_ben), 4),
                                 NA),
      pay_vs_income_ird = ifelse(coalesce(ird_payment_2019,0)+coalesce(ird_payment_2020,0) > 0 & total_income_ben > 1,
                                 round((coalesce(ird_payment_2019,0)+coalesce(ird_payment_2020,0))/(0.875*total_income_ben), 4),
                                 NA),
      pay_vs_income_all = ifelse(coalesce(ird_payment_2019, 0) + coalesce(ird_payment_2020,0) +
                                    coalesce(moj_payment_2019, 0) + coalesce(moj_payment_2020, 0) +
                                    coalesce(msd_payment_2019, 0) + coalesce(msd_payment_2020, 0) > 0 
                                 & total_income_ben > 1,
                                 round((coalesce(ird_payment_2019, 0) + coalesce(ird_payment_2020,0) +
                                           coalesce(moj_payment_2019, 0) + coalesce(moj_payment_2020, 0) +
                                           coalesce(msd_payment_2019, 0) + coalesce(msd_payment_2020, 0))/(0.875*total_income_ben), 4),
                                 NA),
      
      ##debt to income ratio
      debt_vs_income_moj = ifelse(coalesce(moj_Y2020Sep, 0) > 0 & total_income_ben >1,
                                  round(coalesce(moj_Y2020Sep, 0)/(0.5*total_income_ben), 4), NA),
      debt_vs_income_msd = ifelse(coalesce(msd_Y2020Sep, 0) > 0 & total_income_ben >1,
                                  round(coalesce(msd_Y2020Sep, 0)/(0.5*total_income_ben), 4), NA),
      debt_vs_income_ird = ifelse(coalesce(ird_Y2020Sep, 0) > 0 & total_income_ben >1,
                                  round(coalesce(ird_Y2020Sep, 0)/(0.5*total_income_ben), 4), NA),
      debt_vs_income_all = ifelse(coalesce(moj_Y2020Sep, 0) + coalesce(msd_Y2020Sep, 0) + coalesce(ird_Y2020Sep, 0) > 0
                                  & total_income_ben > 1,
                                  round((coalesce(moj_Y2020Sep, 0) + coalesce(msd_Y2020Sep, 0) + coalesce(ird_Y2020Sep, 0))/(0.5*total_income_ben), 4), NA)
      )%>%
   mutate(
      ##payment vs income
      `pay_vs_income_moj=0-.005` = ifelse(pay_vs_income_moj > 0 & pay_vs_income_moj <= 0.005, 1, NA),
      `pay_vs_income_moj=0.005-.01` = ifelse(pay_vs_income_moj > 0.005 & pay_vs_income_moj <= 0.01, 1, NA),
      `pay_vs_income_moj=0.01-.0.05` = ifelse(pay_vs_income_moj > 0.01 & pay_vs_income_moj <= 0.05, 1, NA),
      `pay_vs_income_moj=0.05-.1` = ifelse(pay_vs_income_moj > 0.05 & pay_vs_income_moj <= 0.1, 1, NA),
      `pay_vs_income_moj=0.1-.2` = ifelse(pay_vs_income_moj > 0.1 & pay_vs_income_moj <= 0.2, 1, NA),
      `pay_vs_income_moj=.2-.4` = ifelse(pay_vs_income_moj > 0.2 & pay_vs_income_moj <= 0.4, 1, NA),
      `pay_vs_income_moj=.4-.7` = ifelse(pay_vs_income_moj > 0.4 & pay_vs_income_moj <= 0.7, 1, NA),
      `pay_vs_income_moj=.7-1` = ifelse(pay_vs_income_moj > 0.7 & pay_vs_income_moj <= 1, 1, NA),
      `pay_vs_income_moj>1` = ifelse(pay_vs_income_moj > 1, 1, NA),
      `pay_vs_income_msd=0-.005` = ifelse(pay_vs_income_msd > 0 & pay_vs_income_msd <= 0.005, 1, NA),
      `pay_vs_income_msd=0.005-.01` = ifelse(pay_vs_income_msd > 0.005 & pay_vs_income_msd <= 0.01, 1, NA),
      `pay_vs_income_msd=0.01-.0.05` = ifelse(pay_vs_income_msd > 0.01 & pay_vs_income_msd <= 0.05, 1, NA),
      `pay_vs_income_msd=0.05-.1` = ifelse(pay_vs_income_msd > 0.05 & pay_vs_income_msd <= 0.1, 1, NA),
      `pay_vs_income_msd=0.1-.2` = ifelse(pay_vs_income_msd > 0.1 & pay_vs_income_msd <= 0.2, 1, NA),
      `pay_vs_income_msd=.2-.4` = ifelse(pay_vs_income_msd > 0.2 & pay_vs_income_msd <= 0.4, 1, NA),
      `pay_vs_income_msd=.4-.7` = ifelse(pay_vs_income_msd > 0.4 & pay_vs_income_msd <= 0.7, 1, NA),
      `pay_vs_income_msd=.7-1` = ifelse(pay_vs_income_msd > 0.7 & pay_vs_income_msd <= 1, 1, NA),
      `pay_vs_income_msd>1` = ifelse(pay_vs_income_msd > 1, 1, NA),
      `pay_vs_income_ird=0-.005` = ifelse(pay_vs_income_ird > 0 & pay_vs_income_ird <= 0.005, 1, NA),
      `pay_vs_income_ird=0.005-.01` = ifelse(pay_vs_income_ird > 0.005 & pay_vs_income_ird <= 0.01, 1, NA),
      `pay_vs_income_ird=0.01-.0.05` = ifelse(pay_vs_income_ird > 0.01 & pay_vs_income_ird <= 0.05, 1, NA),
      `pay_vs_income_ird=0.05-.1` = ifelse(pay_vs_income_ird > 0.05 & pay_vs_income_ird <= 0.1, 1, NA),
      `pay_vs_income_ird=0.1-.2` = ifelse(pay_vs_income_ird > 0.1 & pay_vs_income_ird <= 0.2, 1, NA),
      `pay_vs_income_ird=.2-.4` = ifelse(pay_vs_income_ird > 0.2 & pay_vs_income_ird <= 0.4, 1, NA),
      `pay_vs_income_ird=.4-.7` = ifelse(pay_vs_income_ird > 0.4 & pay_vs_income_ird <= 0.7, 1, NA),
      `pay_vs_income_ird=.7-1` = ifelse(pay_vs_income_ird > 0.7 & pay_vs_income_ird <= 1, 1, NA),
      `pay_vs_income_ird>1` = ifelse(pay_vs_income_ird > 1, 1, NA),
      `pay_vs_income_all=0-.005` = ifelse(pay_vs_income_all > 0 & pay_vs_income_all <= 0.005, 1, NA),
      `pay_vs_income_all=0.005-.01` = ifelse(pay_vs_income_all > 0.005 & pay_vs_income_all <= 0.01, 1, NA),
      `pay_vs_income_all=0.01-.0.05` = ifelse(pay_vs_income_all > 0.01 & pay_vs_income_all <= 0.05, 1, NA),
      `pay_vs_income_all=0.05-.1` = ifelse(pay_vs_income_all > 0.05 & pay_vs_income_all <= 0.1, 1, NA),
      `pay_vs_income_all=0.1-.2` = ifelse(pay_vs_income_all > 0.1 & pay_vs_income_all <= 0.2, 1, NA),
      `pay_vs_income_all=.2-.4` = ifelse(pay_vs_income_all > 0.2 & pay_vs_income_all <= 0.4, 1, NA),
      `pay_vs_income_all=.4-.7` = ifelse(pay_vs_income_all > 0.4 & pay_vs_income_all <= 0.7, 1, NA),
      `pay_vs_income_all=.7-1` = ifelse(pay_vs_income_all > 0.7 & pay_vs_income_all <= 1, 1, NA),
      `pay_vs_income_all>1` = ifelse(pay_vs_income_all > 1, 1, NA),

      ##debt vs income
      `debt_vs_income_moj=0-.005` = ifelse(debt_vs_income_moj > 0 & debt_vs_income_moj <= 0.005, 1, NA),
      `debt_vs_income_moj=0.005-.01` = ifelse(debt_vs_income_moj > 0.005 & debt_vs_income_moj <= 0.01, 1, NA),
      `debt_vs_income_moj=0.01-.0.05` = ifelse(debt_vs_income_moj > 0.01 & debt_vs_income_moj <= 0.05, 1, NA),
      `debt_vs_income_moj=0.05-.1` = ifelse(debt_vs_income_moj > 0.05 & debt_vs_income_moj <= 0.1, 1, NA),
      `debt_vs_income_moj=0.1-.2` = ifelse(debt_vs_income_moj > 0.1 & debt_vs_income_moj <= 0.2, 1, NA),
      `debt_vs_income_moj=.2-.4` = ifelse(debt_vs_income_moj > 0.2 & debt_vs_income_moj <= 0.4, 1, NA),
      `debt_vs_income_moj=.4-.7` = ifelse(debt_vs_income_moj > 0.4 & debt_vs_income_moj <= 0.7, 1, NA),
      `debt_vs_income_moj=.7-1` = ifelse(debt_vs_income_moj > 0.7 & debt_vs_income_moj <= 1, 1, NA),
      `debt_vs_income_moj>1` = ifelse(debt_vs_income_moj > 1, 1, NA),
      `debt_vs_income_msd=0-.005` = ifelse(debt_vs_income_msd > 0 & debt_vs_income_msd <= 0.005, 1, NA),
      `debt_vs_income_msd=0.005-.01` = ifelse(debt_vs_income_msd > 0.005 & debt_vs_income_msd <= 0.01, 1, NA),
      `debt_vs_income_msd=0.01-.0.05` = ifelse(debt_vs_income_msd > 0.01 & debt_vs_income_msd <= 0.05, 1, NA),
      `debt_vs_income_msd=0.05-.1` = ifelse(debt_vs_income_msd > 0.05 & debt_vs_income_msd <= 0.1, 1, NA),
      `debt_vs_income_msd=0.1-.2` = ifelse(debt_vs_income_msd > 0.1 & debt_vs_income_msd <= 0.2, 1, NA),
      `debt_vs_income_msd=.2-.4` = ifelse(debt_vs_income_msd > 0.2 & debt_vs_income_msd <= 0.4, 1, NA),
      `debt_vs_income_msd=.4-.7` = ifelse(debt_vs_income_msd > 0.4 & debt_vs_income_msd <= 0.7, 1, NA),
      `debt_vs_income_msd=.7-1` = ifelse(debt_vs_income_msd > 0.7 & debt_vs_income_msd <= 1, 1, NA),
      `debt_vs_income_msd>1` = ifelse(debt_vs_income_msd > 1, 1, NA),
      `debt_vs_income_ird=0-.005` = ifelse(debt_vs_income_ird > 0 & debt_vs_income_ird <= 0.005, 1, NA),
      `debt_vs_income_ird=0.005-.01` = ifelse(debt_vs_income_ird > 0.005 & debt_vs_income_ird <= 0.01, 1, NA),
      `debt_vs_income_ird=0.01-.0.05` = ifelse(debt_vs_income_ird > 0.01 & debt_vs_income_ird <= 0.05, 1, NA),
      `debt_vs_income_ird=0.05-.1` = ifelse(debt_vs_income_ird > 0.05 & debt_vs_income_ird <= 0.1, 1, NA),
      `debt_vs_income_ird=0.1-.2` = ifelse(debt_vs_income_ird > 0.1 & debt_vs_income_ird <= 0.2, 1, NA),
      `debt_vs_income_ird=.2-.4` = ifelse(debt_vs_income_ird > 0.2 & debt_vs_income_ird <= 0.4, 1, NA),
      `debt_vs_income_ird=.4-.7` = ifelse(debt_vs_income_ird > 0.4 & debt_vs_income_ird <= 0.7, 1, NA),
      `debt_vs_income_ird=.7-1` = ifelse(debt_vs_income_ird > 0.7 & debt_vs_income_ird <= 1, 1, NA),
      `debt_vs_income_ird>1` = ifelse(debt_vs_income_ird > 1, 1, NA),
      `debt_vs_income_all=0-.005` = ifelse(debt_vs_income_all > 0 & debt_vs_income_all <= 0.005, 1, NA),
      `debt_vs_income_all=0.005-.01` = ifelse(debt_vs_income_all > 0.005 & debt_vs_income_all <= 0.01, 1, NA),
      `debt_vs_income_all=0.01-.0.05` = ifelse(debt_vs_income_all > 0.01 & debt_vs_income_all <= 0.05, 1, NA),
      `debt_vs_income_all=0.05-.1` = ifelse(debt_vs_income_all > 0.05 & debt_vs_income_all <= 0.1, 1, NA),
      `debt_vs_income_all=0.1-.2` = ifelse(debt_vs_income_all > 0.1 & debt_vs_income_all <= 0.2, 1, NA),
      `debt_vs_income_all=.2-.4` = ifelse(debt_vs_income_all > 0.2 & debt_vs_income_all <= 0.4, 1, NA),
      `debt_vs_income_all=.4-.7` = ifelse(debt_vs_income_all > 0.4 & debt_vs_income_all <= 0.7, 1, NA),
      `debt_vs_income_all=.7-1` = ifelse(debt_vs_income_all > 0.7 & debt_vs_income_all <= 1, 1, NA),
      `debt_vs_income_all>1` = ifelse(debt_vs_income_all > 1, 1, NA),
      
      moj_payment_3mth = ifelse(moj_payment_3mth >= 1, 1, NA),
      moj_payment_6mth = ifelse(moj_payment_6mth >= 1, 1, NA),
      moj_payment_9mth = ifelse(moj_payment_9mth >= 1, 1, NA),
      moj_payment_12mth = ifelse(moj_payment_12mth >= 1, 1, NA),
      moj_persistence_3mth = ifelse(moj_persistence_3mth >= 1, 1, NA),
      moj_persistence_6mth = ifelse(moj_persistence_6mth >= 1, 1, NA),
      moj_persistence_9mth = ifelse(moj_persistence_9mth >= 1, 1, NA),
      moj_persistence_12mth = ifelse(moj_persistence_12mth >= 1, 1, NA),
      moj_persistence_15mth = ifelse(moj_persistence_15mth >= 1, 1, NA),
      moj_persistence_18mth = ifelse(moj_persistence_18mth >= 1, 1, NA),
      moj_persistence_21mth = ifelse(moj_persistence_21mth >= 1, 1, NA),
      msd_payment_3mth = ifelse(msd_payment_3mth >= 1, 1, NA),
      msd_payment_6mth = ifelse(msd_payment_6mth >= 1, 1, NA),
      msd_payment_9mth = ifelse(msd_payment_9mth >= 1, 1, NA),
      msd_payment_12mth = ifelse(msd_payment_12mth >= 1, 1, NA),
      msd_persistence_3mth = ifelse(msd_persistence_3mth >= 1, 1 ,NA),
      msd_persistence_6mth = ifelse(msd_persistence_6mth >= 1, 1, NA),
      msd_persistence_9mth = ifelse(msd_persistence_9mth >= 1, 1, NA),
      msd_persistence_12mth = ifelse(msd_persistence_12mth >= 1, 1, NA),
      msd_persistence_15mth = ifelse(msd_persistence_15mth >= 1, 1, NA),
      msd_persistence_18mth = ifelse(msd_persistence_18mth >= 1, 1, NA),
      msd_persistence_21mth = ifelse(msd_persistence_21mth >= 1, 1, NA),
      ird_payment_3mth = ifelse(ird_payment_3mth >= 1, 1, NA),
      ird_payment_6mth = ifelse(ird_payment_6mth >= 1, 1, NA),
      ird_payment_9mth = ifelse(ird_payment_9mth >= 1, 1, NA),
      ird_payment_12mth = ifelse(ird_payment_12mth >= 1, 1, NA),
      ird_persistence_3mth = ifelse(ird_persistence_3mth >= 1, 1, NA),
      ird_persistence_6mth = ifelse(ird_persistence_6mth >= 1, 1, NA),
      ird_persistence_9mth = ifelse(ird_persistence_9mth >= 1, 1, NA),
      ird_persistence_12mth = ifelse(ird_persistence_12mth >= 1, 1, NA),
      ird_persistence_15mth = ifelse(ird_persistence_15mth >= 1, 1, NA),
      ird_persistence_18mth = ifelse(ird_persistence_18mth >= 1, 1, NA),
      ird_persistence_21mth = ifelse(ird_persistence_21mth >= 1, 1, NA),
      
      # remove nulls
      moj_debtor = ifelse(is.na(moj_debtor), 0, moj_debtor),
      moj_fine_debtor = ifelse(is.na(moj_fine_debtor), 0, moj_fine_debtor),
      moj_fcco_debtor = ifelse(is.na(moj_fcco_debtor), 0, moj_fcco_debtor),
      msd_debtor = ifelse(is.na(msd_debtor), 0, msd_debtor),
      msd_ov_debtor = ifelse(is.na(msd_ov_debtor), 0, msd_ov_debtor),
      msd_ra_debtor = ifelse(is.na(msd_ra_debtor), 0, msd_ra_debtor),
      ird_debtor = ifelse(is.na(ird_debtor), 0, ird_debtor),
      ird_it_debtor = ifelse(is.na(ird_it_debtor), 0, ird_it_debtor),
      ird_lp_debtor = ifelse(is.na(ird_lp_debtor), 0, ird_lp_debtor),
      ird_wff_debtor = ifelse(is.na(ird_wff_debtor), 0, ird_wff_debtor),
      ird_osl_debtor = ifelse(is.na(ird_osl_debtor), 0, ird_osl_debtor),
      hhld_child = ifelse(is.na(hhld_child), 0, hhld_child),
      low_income_hhld = ifelse(is.na(low_income_hhld), 0, low_income_hhld)
   )

## review tidied dataset --------------------------------------------------------------------------

# during development inspect state of table
# explore::explore_shiny(collect(working_table))

## write for output -------------------------------------------------------------------------------

run_time_inform_user("saving output table", context = "heading", print_level = VERBOSE)
written_tbl = write_to_database(working_table, db_con, SANDPIT, OUR_SCHEMA, TIDY_TABLE, OVERWRITE = TRUE)
# index
run_time_inform_user("indexing", context = "details", print_level = VERBOSE)
create_clustered_index(db_con, SANDPIT, OUR_SCHEMA, TIDY_TABLE, "identity_column")
# compress
run_time_inform_user("compressing", context = "details", print_level = VERBOSE)
compress_table(db_con, SANDPIT, OUR_SCHEMA, TIDY_TABLE)

# close connection
close_database_connection(db_con)
run_time_inform_user("grand completion", context = "heading", print_level = VERBOSE)
