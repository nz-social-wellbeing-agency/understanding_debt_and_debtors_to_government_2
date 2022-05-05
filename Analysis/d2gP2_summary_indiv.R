#####################################################################################################
#' Description: Debt to Government phase 2
#'
#' Input: Tidied debt table for D2GP2 project
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
TIDY_TABLE = "[d2gP2_tidy_table_July]"

# outputs
RESULTS_FILE_SUM = "[d2gP2_debt_summary_July]"
RESULTS_FILE_CNT = "[d2gP2_debt_count_July]"
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
  "low_income",
  "child_at_address"#,
  # "confident_child_at_address"
)

#### columns to be used once each when grouping ----
once_grouping_columns = c(
  "is_alive",
  "moj_fine_debtor",
  "moj_fcco_debtor",
  "msd_ov_debtor",
  "msd_ra_debtor",
  "ird_it_debtor",
  "ird_lp_debtor",
  "ird_wff_debtor",
  "ird_osl_debtor",
  "total_debt_groups",
  "child_age_group",
  "ws_ben_mth_group",
  "abatement_indicator",
  "income_variability",
  "days_disability_ben_365",
  "census_dsblty_ind"
 
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

#### columns to count ---
demographics = c(
  "eth_asian",
  "eth_european",
  "eth_maori",
  "eth_MELAA",
  "eth_other",
  "eth_pacific",
  "eth_MELAA_other",
  "sex_code=1",
  "sex_code=2",
  "age_b_15",
  "age_15_19",
  "age_20_24",
  "age_25_29",
  "age_30_34",
  "age_35_39",
  "age_40_44",
  "age_45_49",
  "age_50_54",
  "age_55_59",
  "age_60_64",
  "age_o_65",
  ##"child_age_group",
  "confident_child_at_address",
  "area_type=Large urban area",
  "area_type=Major urban area",
  "area_type=Medium urban area",
  "area_type=Rural other",
  "area_type=Rural settlement",
  "area_type=Small urban area",
  
  "region_code=01",
  "region_code=02",
  "region_code=03",
  "region_code=04",
  "region_code=05",
  "region_code=06",
  "region_code=07",
  "region_code=08",
  "region_code=09",
  "region_code=12",
  "region_code=13",
  "region_code=14",
  "region_code=15",
  "region_code=16",
  "region_code=17",
  "region_code=18",
  "ben_JS",
  "ben_SPS",
  "ben_SLP",
  "ben_NZS",
  "ben_OTH",
  #"total_income_ben_r",
  #"WAS_income_r",
  #"WHP_income_r",
  #"BEN_income_r", 
  "main_activity_employed",
  "main_activity_study",
  "main_activity_NEET",
  "days_overseas_365",
  "days_employed_365",
  "employment_end",
  "days_study_365",
  "days_neet_365",
  "days_sickness_benefit_365",
  "sickness_benefit_start",
  "days_dead_365",
  "days_corrections_any_365",
  "days_corrections_prison_365",
  ##"ws_ben_mth_r",
  ##"abatement_indicator",
  ##"income_volatility",
  "days_social_housing_365",
  ##"days_disability_ben_365",
  ##"census_dsblty_ind",
  ##"cen_dffcl_act_ind",
  # 
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
  
  "moj_payment_3mth_fine",
  "moj_payment_6mth_fine",
  "moj_payment_9mth_fine",
  "moj_payment_12mth_fine",
  "moj_persistence_3mth_fine",
  "moj_persistence_6mth_fine",
  "moj_persistence_9mth_fine",
  "moj_persistence_12mth_fine",
  "moj_persistence_15mth_fine",
  "moj_persistence_18mth_fine",
  "moj_persistence_21mth_fine",
  "moj_payment_3mth_fcco",
  "moj_payment_6mth_fcco",
  "moj_payment_9mth_fcco",
  "moj_payment_12mth_fcco",
  "moj_persistence_3mth_fcco",
  "moj_persistence_6mth_fcco",
  "moj_persistence_9mth_fcco",
  "moj_persistence_12mth_fcco",
  "moj_persistence_15mth_fcco",
  "moj_persistence_18mth_fcco",
  "moj_persistence_21mth_fcco",
  "msd_payment_3mth_OV",
  "msd_payment_6mth_OV",
  "msd_payment_9mth_OV",
  "msd_payment_12mth_OV",
  "msd_persistence_3mth_OV",
  "msd_persistence_6mth_OV",
  "msd_persistence_9mth_OV",
  "msd_persistence_12mth_OV",
  "msd_persistence_15mth_OV",
  "msd_persistence_18mth_OV",
  "msd_persistence_21mth_OV",
  "msd_payment_3mth_RA",
  "msd_payment_6mth_RA",
  "msd_payment_9mth_RA",
  "msd_payment_12mth_RA",
  "msd_persistence_3mth_RA",
  "msd_persistence_6mth_RA",
  "msd_persistence_9mth_RA",
  "msd_persistence_12mth_RA",
  "msd_persistence_15mth_RA",
  "msd_persistence_18mth_RA",
  "msd_persistence_21mth_RA",
  
  "ird_payment_3mth_DTC",
  "ird_payment_6mth_DTC",
  "ird_payment_9mth_DTC",
  "ird_payment_12mth_DTC",
  "ird_persistence_3mth_DTC",
  "ird_persistence_6mth_DTC",
  "ird_persistence_9mth_DTC",
  "ird_persistence_12mth_DTC",
  "ird_persistence_15mth_DTC",
  "ird_persistence_18mth_DTC",
  "ird_persistence_21mth_DTC",
  "ird_payment_3mth_EA",
  "ird_payment_6mth_EA",
  "ird_payment_9mth_EA",
  "ird_payment_12mth_EA",
  "ird_persistence_3mth_EA",
  "ird_Peristsence_6mth_EA",
  "ird_Persistence_9mth_EA",
  "ird_persistence_12mth_EA",
  "ird_persistence_15mth_EA",
  "ird_persistence_18mth_EA",
  "ird_persistence_21mth_EA",
  "ird_payment_3mth_GST",
  "ird_payment_6mth_GST",
  "ird_payment_9mth_GST",
  "ird_payment_12mth_GST",
  "ird_persistence_3mth_GST",
  "ird_persistence_6mth_GST",
  "ird_persistence_9mth_GST",
  "ird_persistence_12mth_GST",
  "ird_persistence_15mth_GST",
  "ird_persistence_18mth_GST",
  "ird_persistence_21mth_GST",
  "ird_payment_3mth_O",
  "ird_payment_6mth_O",
  "ird_payment_9mth_O",
  "ird_payment_12mth_O",
  "ird_persistence_3mth_O",
  "ird_persistence_6mth_O",
  "ird_persistence_9mth_O",
  "ird_persistence_12mth_O",
  "ird_persistence_15mth_O",
  "ird_persistence_18mth_O",
  "ird_persistence_21mth_O",
  "ird_payment_3mth_RC",
  "ird_payment_6mth_RC",
  "ird_payment_9mth_RC",
  "ird_payment_12mth_RC",
  "ird_persistence_3mth_RC",
  "ird_persistence_6mth_RC",
  "ird_persistence_9mth_RC",
  "ird_persistence_12mth_RC",
  "ird_persistence_15mth_RC",
  "ird_Persistence_18mth_RC",
  "ird_Persistence_21mth_RC",
  "ird_payment_3mth_IT",
  "ird_payment_6mth_IT",
  "ird_payment_9mth_IT",
  "ird_payment_12mth_IT",
  "ird_persistence_3mth_IT",
  "ird_persistence_6mth_IT",
  "ird_persistence_9mth_IT",
  "ird_persistence_12mth_IT",
  "ird_persistence_15mth_IT",
  "ird_persistence_18mth_IT",
  "ird_persistence_21mth_IT",
  "ird_payment_3mth_LP",
  "ird_payment_6mth_LP",
  "ird_payment_9mth_LP",
  "ird_payment_12mth_LP",
  "ird_payment_12mth_LP",
  "ird_persistence_3mth_LP",
  "ird_persistence_6mth_LP",
  "ird_persistence_9mth_LP",
  "ird_persistence_12mth_LP",
  "ird_persistence_15mth_LP",
  "ird_persistence_18mth_LP",
  "ird_persistence_21mth_LP",
  "ird_payment_3mth_F",
  "ird_payment_6mth_F",
  "ird_payment_9mth_F",
  "ird_payment_12mth_F",
  "ird_persistence_3mth_F",
  "ird_persistence_6mth_F",
  "ird_persistence_9mth_F",
  "ird_persistence_12mth_F",
  "ird_persistence_15mth_F",
  "ird_persistence_18mth_F",
  "ird_persistence_21mth_F",
  "ird_payment_3mth_SL",
  "ird_payment_6mth_SL",
  "ird_payment_9mth_SL",
  "ird_payment_12mth_SL",
  "ird_persistence_3mth_SL",
  "ird_persistence_6mth_SL",
  "ird_persistence_9mth_SL",
  "ird_persistence_12mth_SL",
  "ird_persistence_15mth_SL",
  "ird_persistence_18mth_SL",
  "ird_persistence_21mth_SL",
  "life_satisfaction=0-3",
  "life_satisfaction=4-6",
  "life_satisfaction=7-10",
  "life_worthwhile=0-3",
  "life_worthwhile=4-6",
  "life_worthwhile=7-10",
  "mwi_afford_300_item=11-12",
  "mwi_afford_300_item=13",
  "mwi_afford_300_item=14-15",
  "mwi_enough_income=11",
  "mwi_enough_income=12-13",
  "mwi_enough_income=14",
  "mwi_material_wellbeing=0-5",
  "mwi_material_wellbeing=6-10",
  "mwi_material_wellbeing=11-15",
  "mwi_material_wellbeing=16-20",
  "mwi_not_pay_bills_on_time=11",
  "mwi_not_pay_bills_on_time=12",
  "mwi_not_pay_bills_on_time=13",
  "mental_health=0-33",
  "mental_health=34-66",
  "mental_health=67-100",
  "family_wellbeing=0-3",
  "family_wellbeing=4-6",
  "family_wellbeing=7-10",
  "highest_qualification=none",
  "highest_qualification=cert",
  "highest_qualification=grad",
  "highest_qualification=bach",
  "highest_qualification=post",
  "deprivation=1",
  "deprivation=2",
  "deprivation=3",
  "deprivation=4",
  "deprivation=5",
  "deprivation=6",
  "deprivation=7",
  "deprivation=8",
  "deprivation=9",
  "deprivation=10",
  
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

output_pos = list()
output_cnt = list()

for(this_grouping_column in once_grouping_columns){
  # lists to collect output
  this_output_pos = list()
  this_output_cnt = list()
  # combined grouping columns
  current_grouping_columns = c(always_grouping_columns, this_grouping_column)
  
  # count all values & output
  for(col in demographics){
    this_output_cnt[[col]] = count_values(working_table, current_grouping_columns, col)
    run_time_inform_user(glue::glue("summarising {col} complete"), context = "all", print_level = VERBOSE)
  }
  output_cnt[[this_grouping_column]] = data.table::rbindlist(this_output_cnt,fill=TRUE) %>%
    mutate(this_grouper = this_grouping_column) %>%
    rename(grouper_value = !!sym(this_grouping_column)) %>%
    filter(counted_col_value == 1)
  
  # count & sum all positive values
  for(col in debt_values){
    this_output_pos[[col]] = positives_count_sum_values(working_table, current_grouping_columns, col)
    run_time_inform_user(glue::glue("summarising {col} complete"), context = "all", print_level = VERBOSE)
  }
  output_pos[[this_grouping_column]] = data.table::rbindlist(this_output_pos,fill=TRUE) %>%
    mutate(this_grouper = this_grouping_column) %>%
    rename(grouper_value = !!sym(this_grouping_column))
  
  run_time_inform_user(glue::glue("variable {this_grouping_column} complete"), context = "details", print_level = VERBOSE)
}

# close connection
close_database_connection(db_con)

## output results ---------------------------------------------------------------------------------

run_time_inform_user("outputing results", context = "heading", print_level = VERBOSE)





 results_cnt = data.table::rbindlist(output_cnt,fill=TRUE) %>%
   tidyr::pivot_wider(names_from = "counted_col", values_from = "num") #%>%
#   filter(this_grouper ==  'total_debt_groups' | grouper_value == 1)
 results_pos = data.table::rbindlist(output_pos,fill=TRUE) %>%
   tidyr::pivot_wider(names_from = "summarised_col", values_from = c("num_pos", "sum_pos")) #%>%
#   filter(this_grouper ==  'total_debt_groups' | grouper_value == 1)

save(results_cnt, file = "results_cnt_July.RData")
save(results_pos, file = "results_pos_July.RData")

write.csv(results_cnt, "results_cnt_July.csv")
write.csv(results_pos, "results_pos_July.csv")



run_time_inform_user("GRAND COMPLETION", context = "heading", print_level = VERBOSE)


