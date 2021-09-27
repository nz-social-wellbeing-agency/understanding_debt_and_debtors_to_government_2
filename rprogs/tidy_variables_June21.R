#####################################################################################################
#' Description: Tidy assembled data
#'
#' Input: Rectangular debt table produced by run_assembly for D2GP2 project
#'
#' Output: Tidied debt table for D2GP2 project
#' 
#' Author: Simon Anastasiadis
#' Modified: Freya Li
#' 
#' Dependencies: dbplyr_helper_functions.R, utility_functions.R, table_consistency_checks.R
#' 
#' Notes: 
#' Living wage is calculated as 21.625 x 40 x 52 = 44980. Where 21.625 is the average hourly
#' living wage calculated as the average of the living wage rate for 2019 ($21.15) and the
#' living wage rate for 2020 ($22.10)
#' 
#' Issues:
#' 
#' History (reverse order):
#' 2021-09-22 SA review
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
ASSEMBLED_TABLE = "[d2gP2_rectangular_July]"
LOW_INCOME_THRESHOLD = 44980

# outputs
TIDY_TABLE = "[d2gP2_tidy_table_July]"

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

## check dataset variables ------------------------------------------------------------------------

# sort columns
# working_table = working_table %>%
#   select(sort(colnames(working_table)))

# during development inspect state of table
# explore::explore_shiny(collect(working_table))

## prep / cleaning in SQL -------------------------------------------------------------------------

working_table = working_table %>%
  
  filter(
    # keep people who were alive, residents, and not dead during the study period
    is_alive == 1,
    residential_population_indicator == 1,
    is.na(audit_dead)
  ) %>%
  mutate(
    # reverse some signs to set positive valued transactions as the norm
    msd_pay_write_2019_OV = -1.0 * msd_pay_write_2019_OV,
    msd_pay_write_2020_OV = -1.0 * msd_pay_write_2020_OV,
    msd_pay_write_2019_RA = -1.0 * msd_pay_write_2019_RA,
    msd_pay_write_2020_RA = -1.0 * msd_pay_write_2020_RA,
    # indicators for different debt types
    moj_debtor = ifelse(moj_Y2020Sep > 1, 1, 0),
    moj_fine_debtor = ifelse(moj_Y2020Sep_fine > 1, 1, 0),
    moj_fcco_debtor = ifelse(moj_Y2020Sep_fcco > 1, 1, 0),
    msd_debtor = ifelse(msd_Y2020Sep > 1, 1, 0),
    msd_ov_debtor = ifelse(msd_Y2020Sep_overpayment > 1, 1, 0),
    msd_ra_debtor = ifelse(msd_Y2020Sep_assistance > 1, 1, 0),
    ird_debtor = ifelse(ird_Y2020Sep > 1, 1, 0),
    ird_it_debtor = ifelse(ird_Y2020Sep_Income_Tax > 1, 1, 0),
    ird_lp_debtor = ifelse(ird_Y2020Sep_Liable_Parent > 1, 1, 0),
    ird_wff_debtor = ifelse(ird_Y2020Sep_Families > 1, 1, 0),
    ird_osl_debtor = ifelse(ird_Y2020Sep_Student_Loan > 1, 1, 0),
    # total debt
    total_debt = coalesce(ifelse(moj_Y2020Sep > 1, moj_Y2020Sep, 0), 0) +
      coalesce(ifelse(msd_Y2020Sep > 1, msd_Y2020Sep, 0), 0) +
      coalesce(ifelse(ird_Y2020Sep > 1, ird_Y2020Sep, 0), 0),
    # age and age groups
    age = 2020 - birth_year,
    age_eldest_child_address = 2020 - eldest_child_address,
    age_youngest_child_address = 2020 - Youngest_child_address,
    
    # MWI uses 77 as non-response indicator.
    life_satisfaction = ifelse(life_satisfaction > 10, NA, life_satisfaction),
    life_worthwhile = ifelse(life_worthwhile > 10, NA, life_worthwhile),
    mental_health = ifelse(mental_health > 100, NA, mental_health),
    mwi_afford_300_item= ifelse(mwi_afford_300_item > 75, NA, mwi_afford_300_item),
    mwi_not_pay_bills_on_time = ifelse(mwi_not_pay_bills_on_time > 75, NA, mwi_not_pay_bills_on_time),
    mwi_enough_income = ifelse(mwi_enough_income > 75, NA, mwi_enough_income),
    mwi_material_wellbeing_index = ifelse(mwi_material_wellbeing_index > 75, NA, mwi_material_wellbeing_index),
    family_wellbeing = ifelse(family_wellbeing > 10, NA, family_wellbeing),
    
    # other variables
    ben_ind = ifelse(ben_ind >= 1, 1, 0),
    child_at_address = ifelse(child_at_address >= 1, 1, 0),
    confident_child_at_address = ifelse(child_at_address == 1
                                        & number_dependent_children >= 1, 1, 0),
    low_income = ifelse(is.na(total_income_ben) | total_income_ben < LOW_INCOME_THRESHOLD, 1, 0),
    eth_MELAA_other = ifelse(eth_other == 1 | eth_MELAA == 1, 1, 0),
    ## payment to income ratio (0.875 = 21 months of debt data / 24 months of income data)
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
                                round((coalesce(moj_Y2020Sep, 0) + coalesce(msd_Y2020Sep, 0) + coalesce(ird_Y2020Sep, 0))/(0.5*total_income_ben), 4), NA),
    
    inc_sd_2019 = ifelse(is.na(inc_sd_2019), 0, inc_sd_2019),
    inc_sd_2020 = ifelse(is.na(inc_sd_2020), 0, inc_sd_2020),
    
    days_disability_ben_365 = ifelse(is.na(days_disability_ben), NA,
                                     ifelse(days_disability_ben >= 365, 1, 0))
  ) %>%

  ########################################## start_here
    
  # vars that use vars from previous mutate require their own mutate because of database processing
  mutate(
    # total debt groups
    total_debt_groups = dplyr::case_when(
      total_debt >=     1 & total_debt <  1000 ~ "0-1k",
      total_debt >=  1000 & total_debt <  5000 ~ "1-5k",
      total_debt >=  5000 & total_debt < 10000 ~ "5-10k",
      total_debt >= 10000 ~ ">10k"),
    # age and age group
    age_b_15 = ifelse(age < 15, 1, NA),
    age_15_19 = ifelse(age >= 15 & age < 20, 1, NA),
    age_20_24 = ifelse(age >= 20 & age < 25, 1, NA),
    age_25_29 = ifelse(age >= 25 & age < 30, 1, NA),
    age_30_34 = ifelse(age >= 30 & age < 35, 1, NA),
    age_35_39 = ifelse(age >= 35 & age < 40, 1, NA),
    age_40_44 = ifelse(age >= 40 & age < 45, 1, NA),
    age_45_49 = ifelse(age >= 45 & age < 50, 1, NA),
    age_50_54 = ifelse(age >= 50 & age < 55, 1, NA),
    age_55_59 = ifelse(age >= 55 & age < 60, 1, NA),
    age_60_64 = ifelse(age >= 60 & age < 65, 1, NA),
    age_o_65 = ifelse(age >= 65, 1, NA),
    age_group = dplyr::case_when(
      age < 15 ~ '0 - 14',
      age >= 15 & age < 20 ~ '15 - 19',
      age >= 20 & age < 25 ~ '20 - 24',
      age >= 25 & age < 30 ~ '25 - 29',
      age >= 30 & age < 35 ~ '30 - 34',
      age >= 35 & age < 40 ~ '35 - 39',
      age >= 40 & age < 45 ~ '40 - 44',
      age >= 45 & age < 50 ~ '45 - 49',
      age >= 50 & age < 55 ~ '50 - 54',
      age >= 55 & age < 60 ~ '55 - 59',
      age >= 60 & age < 65 ~ '60 - 64',
      age >= 65 ~ 'Over 65'),
    child_age_group = dplyr::case_when(
      age_youngest_child_address <= 3 ~ '0--3',
      age_youngest_child_address >= 4 & age_youngest_child_address <= 6 ~ '4--6',
      age_youngest_child_address >= 7 & age_youngest_child_address <= 12 ~ '7--12',
      age_youngest_child_address >= 13 & age_youngest_child_address <= 18 ~ '13--18'
    ),
    
    ## change one variable to multiple variables with different category groups
    `life_satisfaction=0-3` = ifelse(0 <= life_satisfaction & life_satisfaction <= 3, 1, 0), 
    `life_satisfaction=4-6` = ifelse(4 <= life_satisfaction & life_satisfaction <= 7, 1, 0),
    `life_satisfaction=7-10` = ifelse(8 <= life_satisfaction & life_satisfaction <= 10, 1, 0),
    `life_worthwhile=0-3` = ifelse(0 <= life_worthwhile & life_worthwhile <= 3, 1, 0), 
    `life_worthwhile=4-6` = ifelse(4 <= life_worthwhile & life_worthwhile <= 7, 1, 0),
    `life_worthwhile=7-10` = ifelse(8 <= life_worthwhile & life_worthwhile <= 10, 1, 0),
    `mwi_afford_300_item=11-12` = ifelse(11 <= mwi_afford_300_item & mwi_afford_300_item <=12, 1, 0),
    `mwi_afford_300_item=13` = ifelse(mwi_afford_300_item == 13, 1, 0),
    `mwi_afford_300_item=14-15` = ifelse(14 <= mwi_afford_300_item & mwi_afford_300_item <=15, 1, 0),
    `mwi_enough_income=11` = ifelse(mwi_enough_income == 11, 1, 0),
    `mwi_enough_income=12-13` = ifelse(12 <= mwi_enough_income & mwi_enough_income <= 13, 1, 0),
    `mwi_enough_income=14` = ifelse(mwi_enough_income == 14, 1, 0),
    `mwi_material_wellbeing=0-5` = ifelse(0 <= mwi_material_wellbeing_index & mwi_material_wellbeing_index <= 5, 1, 0),
    `mwi_material_wellbeing=6-10` = ifelse(6 <= mwi_material_wellbeing_index & mwi_material_wellbeing_index <= 10, 1, 0),
    `mwi_material_wellbeing=11-15` = ifelse(11 <= mwi_material_wellbeing_index & mwi_material_wellbeing_index <= 15, 1, 0),
    `mwi_material_wellbeing=16-20` = ifelse(16 <= mwi_material_wellbeing_index & mwi_material_wellbeing_index <= 20, 1, 0),
    `mwi_not_pay_bills_on_time=11` = ifelse(mwi_not_pay_bills_on_time == 11, 1, 0),
    `mwi_not_pay_bills_on_time=12` = ifelse(mwi_not_pay_bills_on_time == 12, 1, 0),
    `mwi_not_pay_bills_on_time=13` = ifelse(mwi_not_pay_bills_on_time == 13, 1, 0),
    
    `mental_health=0-33` = ifelse(0 <= mental_health & mental_health <= 33, 1, 0),
    `mental_health=34-66` = ifelse(34 <= mental_health & mental_health <= 66, 1, 0),
    `mental_health=67-100` = ifelse(67 <= mental_health & mental_health <= 100, 1, 0),
    `family_wellbeing=0-3` = ifelse(0 <= family_wellbeing & family_wellbeing <= 3, 1, 0), 
    `family_wellbeing=4-6` = ifelse(4 <= family_wellbeing & family_wellbeing <= 7, 1, 0),
    `family_wellbeing=7-10` = ifelse(8 <= family_wellbeing & family_wellbeing <= 10, 1, 0),
    `highest_qualification=none` = ifelse(is.na(highest_qualification) & 2018 - birth_year <= 65, 1,0),
    `highest_qualification=cert` = ifelse(1 <= highest_qualification & highest_qualification <= 4, 1, 0),
    `highest_qualification=grad` = ifelse(5 <= highest_qualification & highest_qualification <= 6, 1, 0),
    `highest_qualification=bach` = ifelse(7 <= highest_qualification & highest_qualification <= 8, 1, 0),
    `highest_qualification=post` = ifelse(9 <= highest_qualification & highest_qualification <= 10, 1, 0),
    `deprivation=1` = ifelse(deprivation == 1, 1, 0),
    `deprivation=2` = ifelse(deprivation == 2, 1, 0),
    `deprivation=3` = ifelse(deprivation == 3, 1, 0),
    `deprivation=4` = ifelse(deprivation == 4, 1, 0),
    `deprivation=5` = ifelse(deprivation == 5, 1, 0),
    `deprivation=6` = ifelse(deprivation == 6, 1, 0),
    `deprivation=7` = ifelse(deprivation == 7, 1, 0),
    `deprivation=8` = ifelse(deprivation == 8, 1, 0),
    `deprivation=9` = ifelse(deprivation == 9, 1, 0),
    `deprivation=10` = ifelse(deprivation == 10, 1, 0),
    
    ##payment vs income
    `pay_vs_income_moj=0-.005` = ifelse(pay_vs_income_moj >= 0 & pay_vs_income_moj <= 0.005, 1, NA),
    `pay_vs_income_moj=0.005-.01` = ifelse(pay_vs_income_moj > 0.005 & pay_vs_income_moj <= 0.01, 1, NA),
    `pay_vs_income_moj=0.01-.0.05` = ifelse(pay_vs_income_moj > 0.01 & pay_vs_income_moj <= 0.05, 1, NA),
    `pay_vs_income_moj=0.05-.1` = ifelse(pay_vs_income_moj > 0.05 & pay_vs_income_moj <= 0.1, 1, NA),
    `pay_vs_income_moj=0.1-.2` = ifelse(pay_vs_income_moj > 0.1 & pay_vs_income_moj <= 0.2, 1, NA),
    `pay_vs_income_moj=.2-.4` = ifelse(pay_vs_income_moj > 0.2 & pay_vs_income_moj <= 0.4, 1, NA),
    `pay_vs_income_moj=.4-.7` = ifelse(pay_vs_income_moj > 0.4 & pay_vs_income_moj <= 0.7, 1, NA),
    `pay_vs_income_moj=.7-1` = ifelse(pay_vs_income_moj > 0.7 & pay_vs_income_moj <= 1, 1, NA),
    `pay_vs_income_moj>1` = ifelse(pay_vs_income_moj > 1, 1, NA),
    `pay_vs_income_msd=0-.005` = ifelse(pay_vs_income_msd >= 0 & pay_vs_income_msd <= 0.005, 1, NA),
    `pay_vs_income_msd=0.005-.01` = ifelse(pay_vs_income_msd > 0.005 & pay_vs_income_msd <= 0.01, 1, NA),
    `pay_vs_income_msd=0.01-.0.05` = ifelse(pay_vs_income_msd > 0.01 & pay_vs_income_msd <= 0.05, 1, NA),
    `pay_vs_income_msd=0.05-.1` = ifelse(pay_vs_income_msd > 0.05 & pay_vs_income_msd <= 0.1, 1, NA),
    `pay_vs_income_msd=0.1-.2` = ifelse(pay_vs_income_msd > 0.1 & pay_vs_income_msd <= 0.2, 1, NA),
    `pay_vs_income_msd=.2-.4` = ifelse(pay_vs_income_msd > 0.2 & pay_vs_income_msd <= 0.4, 1, NA),
    `pay_vs_income_msd=.4-.7` = ifelse(pay_vs_income_msd > 0.4 & pay_vs_income_msd <= 0.7, 1, NA),
    `pay_vs_income_msd=.7-1` = ifelse(pay_vs_income_msd > 0.7 & pay_vs_income_msd <= 1, 1, NA),
    `pay_vs_income_msd>1` = ifelse(pay_vs_income_msd > 1, 1, NA),
    `pay_vs_income_ird=0-.005` = ifelse(pay_vs_income_ird >= 0 & pay_vs_income_ird <= 0.005, 1, NA),
    `pay_vs_income_ird=0.005-.01` = ifelse(pay_vs_income_ird > 0.005 & pay_vs_income_ird <= 0.01, 1, NA),
    `pay_vs_income_ird=0.01-.0.05` = ifelse(pay_vs_income_ird > 0.01 & pay_vs_income_ird <= 0.05, 1, NA),
    `pay_vs_income_ird=0.05-.1` = ifelse(pay_vs_income_ird > 0.05 & pay_vs_income_ird <= 0.1, 1, NA),
    `pay_vs_income_ird=0.1-.2` = ifelse(pay_vs_income_ird > 0.1 & pay_vs_income_ird <= 0.2, 1, NA),
    `pay_vs_income_ird=.2-.4` = ifelse(pay_vs_income_ird > 0.2 & pay_vs_income_ird <= 0.4, 1, NA),
    `pay_vs_income_ird=.4-.7` = ifelse(pay_vs_income_ird > 0.4 & pay_vs_income_ird <= 0.7, 1, NA),
    `pay_vs_income_ird=.7-1` = ifelse(pay_vs_income_ird > 0.7 & pay_vs_income_ird <= 1, 1, NA),
    `pay_vs_income_ird>1` = ifelse(pay_vs_income_ird > 1, 1, NA),
    `pay_vs_income_all=0-.005` = ifelse(pay_vs_income_all >= 0 & pay_vs_income_all <= 0.005, 1, NA),
    `pay_vs_income_all=0.005-.01` = ifelse(pay_vs_income_all > 0.005 & pay_vs_income_all <= 0.01, 1, NA),
    `pay_vs_income_all=0.01-.0.05` = ifelse(pay_vs_income_all > 0.01 & pay_vs_income_all <= 0.05, 1, NA),
    `pay_vs_income_all=0.05-.1` = ifelse(pay_vs_income_all > 0.05 & pay_vs_income_all <= 0.1, 1, NA),
    `pay_vs_income_all=0.1-.2` = ifelse(pay_vs_income_all > 0.1 & pay_vs_income_all <= 0.2, 1, NA),
    `pay_vs_income_all=.2-.4` = ifelse(pay_vs_income_all > 0.2 & pay_vs_income_all <= 0.4, 1, NA),
    `pay_vs_income_all=.4-.7` = ifelse(pay_vs_income_all > 0.4 & pay_vs_income_all <= 0.7, 1, NA),
    `pay_vs_income_all=.7-1` = ifelse(pay_vs_income_all > 0.7 & pay_vs_income_all <= 1, 1, NA),
    `pay_vs_income_all>1` = ifelse(pay_vs_income_all > 1, 1, NA),
    
    
    ##debt vs income
    `debt_vs_income_moj=0-.005` = ifelse(debt_vs_income_moj >= 0 & debt_vs_income_moj <= 0.005, 1, NA),
    `debt_vs_income_moj=0.005-.01` = ifelse(debt_vs_income_moj > 0.005 & debt_vs_income_moj <= 0.01, 1, NA),
    `debt_vs_income_moj=0.01-.0.05` = ifelse(debt_vs_income_moj > 0.01 & debt_vs_income_moj <= 0.05, 1, NA),
    `debt_vs_income_moj=0.05-.1` = ifelse(debt_vs_income_moj > 0.05 & debt_vs_income_moj <= 0.1, 1, NA),
    `debt_vs_income_moj=0.1-.2` = ifelse(debt_vs_income_moj > 0.1 & debt_vs_income_moj <= 0.2, 1, NA),
    `debt_vs_income_moj=.2-.4` = ifelse(debt_vs_income_moj > 0.2 & debt_vs_income_moj <= 0.4, 1, NA),
    `debt_vs_income_moj=.4-.7` = ifelse(debt_vs_income_moj > 0.4 & debt_vs_income_moj <= 0.7, 1, NA),
    `debt_vs_income_moj=.7-1` = ifelse(debt_vs_income_moj > 0.7 & debt_vs_income_moj <= 1, 1, NA),
    `debt_vs_income_moj>1` = ifelse(debt_vs_income_moj > 1, 1, NA),
    `debt_vs_income_msd=0-.005` = ifelse(debt_vs_income_msd >= 0 & debt_vs_income_msd <= 0.005, 1, NA),
    `debt_vs_income_msd=0.005-.01` = ifelse(debt_vs_income_msd > 0.005 & debt_vs_income_msd <= 0.01, 1, NA),
    `debt_vs_income_msd=0.01-.0.05` = ifelse(debt_vs_income_msd > 0.01 & debt_vs_income_msd <= 0.05, 1, NA),
    `debt_vs_income_msd=0.05-.1` = ifelse(debt_vs_income_msd > 0.05 & debt_vs_income_msd <= 0.1, 1, NA),
    `debt_vs_income_msd=0.1-.2` = ifelse(debt_vs_income_msd > 0.1 & debt_vs_income_msd <= 0.2, 1, NA),
    `debt_vs_income_msd=.2-.4` = ifelse(debt_vs_income_msd > 0.2 & debt_vs_income_msd <= 0.4, 1, NA),
    `debt_vs_income_msd=.4-.7` = ifelse(debt_vs_income_msd > 0.4 & debt_vs_income_msd <= 0.7, 1, NA),
    `debt_vs_income_msd=.7-1` = ifelse(debt_vs_income_msd > 0.7 & debt_vs_income_msd <= 1, 1, NA),
    `debt_vs_income_msd>1` = ifelse(debt_vs_income_msd > 1, 1, NA),
    `debt_vs_income_ird=0-.005` = ifelse(debt_vs_income_ird >= 0 & debt_vs_income_ird <= 0.005, 1, NA),
    `debt_vs_income_ird=0.005-.01` = ifelse(debt_vs_income_ird > 0.005 & debt_vs_income_ird <= 0.01, 1, NA),
    `debt_vs_income_ird=0.01-.0.05` = ifelse(debt_vs_income_ird > 0.01 & debt_vs_income_ird <= 0.05, 1, NA),
    `debt_vs_income_ird=0.05-.1` = ifelse(debt_vs_income_ird > 0.05 & debt_vs_income_ird <= 0.1, 1, NA),
    `debt_vs_income_ird=0.1-.2` = ifelse(debt_vs_income_ird > 0.1 & debt_vs_income_ird <= 0.2, 1, NA),
    `debt_vs_income_ird=.2-.4` = ifelse(debt_vs_income_ird > 0.2 & debt_vs_income_ird <= 0.4, 1, NA),
    `debt_vs_income_ird=.4-.7` = ifelse(debt_vs_income_ird > 0.4 & debt_vs_income_ird <= 0.7, 1, NA),
    `debt_vs_income_ird=.7-1` = ifelse(debt_vs_income_ird > 0.7 & debt_vs_income_ird <= 1, 1, NA),
    `debt_vs_income_ird>1` = ifelse(debt_vs_income_ird > 1, 1, NA),
    `debt_vs_income_all=0-.005` = ifelse(debt_vs_income_all >= 0 & debt_vs_income_all <= 0.005, 1, NA),
    `debt_vs_income_all=0.005-.01` = ifelse(debt_vs_income_all > 0.005 & debt_vs_income_all <= 0.01, 1, NA),
    `debt_vs_income_all=0.01-.0.05` = ifelse(debt_vs_income_all > 0.01 & debt_vs_income_all <= 0.05, 1, NA),
    `debt_vs_income_all=0.05-.1` = ifelse(debt_vs_income_all > 0.05 & debt_vs_income_all <= 0.1, 1, NA),
    `debt_vs_income_all=0.1-.2` = ifelse(debt_vs_income_all > 0.1 & debt_vs_income_all <= 0.2, 1, NA),
    `debt_vs_income_all=.2-.4` = ifelse(debt_vs_income_all > 0.2 & debt_vs_income_all <= 0.4, 1, NA),
    `debt_vs_income_all=.4-.7` = ifelse(debt_vs_income_all > 0.4 & debt_vs_income_all <= 0.7, 1, NA),
    `debt_vs_income_all=.7-1` = ifelse(debt_vs_income_all > 0.7 & debt_vs_income_all <= 1, 1, NA),
    `debt_vs_income_all>1` = ifelse(debt_vs_income_all > 1, 1, NA),
    
    
    ws_ben_mth_group = dplyr::case_when(
      ws_ben_mth >= 0 & ws_ben_mth <= 5 ~ '0--5',
      ws_ben_mth >= 6 & ws_ben_mth <= 10 ~ '6--10',
      ws_ben_mth >= 11 & ws_ben_mth <= 15 ~ '11--15',
      ws_ben_mth >= 16 & ws_ben_mth <= 20 ~ '16--20'
    ),
    
    # support receipt type
    ben_JS = ifelse(`ben_type=Job Seeker` > 0, 1, 0),
    ben_SPS = ifelse(`ben_type=Sole Parent Support` > 0
                     | `ben_type=Young Parent Payment` > 0, 1, 0),
    ben_SLP = ifelse(`ben_type=Sickness` > 0
                     | `ben_type=Invalids` > 0, 1, 0),
    ben_NZS = ifelse(`ben_type=NZSuper` > 0, 1, 0),
    #ben_PEN = ifelse(`ben_type=Pension` > 0 
    #                 | `ben_type=Veterans Pension` > 0),
    ben_OTH = ifelse(`ben_type=Student` > 0
                     | `ben_type=Youth` > 0
                     | `ben_type=Caring For Sick Or Infirm` > 0, 1, 0),
    # main activities
    main_activity_employed = ifelse(coalesce(days_employed,0) > coalesce(days_study,0)
                                    & coalesce(days_employed,0) > coalesce(days_neet,0)
                                    & coalesce(days_employed,0) >= 365, 1, NA),
    main_activity_study = ifelse(coalesce(days_study,0) > coalesce(days_employed,0)
                                 & coalesce(days_study,0) > coalesce(days_neet,0)
                                 & coalesce(days_study,0) >= 365, 1, NA),
    main_activity_NEET = ifelse(coalesce(days_neet,0) > coalesce(days_study,0)
                                & coalesce(days_neet,0) > coalesce(days_employed ,0)
                                & coalesce(days_neet,0) >= 365, 1, NA),
    # did you spend at least half time of the past two years in activity
    days_benefit_365 = ifelse(days_benefit >= 365, 1, NA),
    days_dead_365 = ifelse(days_dead >= 365, 1, NA),
    days_employed_365 = ifelse(days_employed >= 365, 1, NA),
    days_neet_365 = ifelse(days_neet >= 365, 1, NA),
    days_overseas_365 = ifelse(days_overseas >= 365, 1, NA),
    days_sickness_benefit_365 = ifelse(days_sickness_benefit >= 365, 1, NA),
    days_study_365 = ifelse(days_study >= 365, 1, NA),
    days_social_housing_365 = ifelse(days_social_housing >= 365, 1, NA),
    days_corrections_any_365 = ifelse(days_corrections_any >= 365, 1, NA),
    days_corrections_prison_365 = ifelse(days_corrections_prison >= 365, 1, NA),
    
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
    moj_payment_3mth_fine = ifelse(moj_payment_3mth_fine >= 1, 1, NA),
    moj_payment_6mth_fine = ifelse(moj_payment_6mth_fine >= 1, 1, NA),
    moj_payment_9mth_fine = ifelse(moj_payment_9mth_fine >= 1, 1, NA),
    moj_payment_12mth_fine = ifelse(moj_payment_12mth_fine >= 1, 1, NA),
    moj_persistence_3mth_fine = ifelse(moj_persistence_3mth_fine >= 1, 1, NA),
    moj_persistence_6mth_fine = ifelse(moj_persistence_6mth_fine >= 1, 1, NA),
    moj_persistence_9mth_fine = ifelse(moj_persistence_9mth_fine >= 1, 1, NA),
    moj_persistence_12mth_fine = ifelse(moj_persistence_12mth_fine >= 1, 1, NA),
    moj_persistence_15mth_fine = ifelse(moj_persistence_15mth_fine >= 1, 1, NA),
    moj_persistence_18mth_fine = ifelse(moj_persistence_18mth_fine >= 1, 1, NA),
    moj_persistence_21mth_fine = ifelse(moj_persistence_21mth_fine >= 1, 1, NA),
    moj_payment_3mth_fcco = ifelse(moj_payment_3mth_fcco >= 1, 1, NA),
    moj_payment_6mth_fcco = ifelse(moj_payment_6mth_fcco >= 1, 1, NA),
    moj_payment_9mth_fcco = ifelse(moj_payment_9mth_fcco >= 1, 1, NA),
    moj_payment_12mth_fcco = ifelse(moj_payment_12mth_fcco >= 1, 1, NA),
    moj_persistence_3mth_fcco = ifelse(moj_persistence_3mth_fcco >= 1, 1, NA),
    moj_persistence_6mth_fcco = ifelse(moj_persistence_6mth_fcco >= 1, 1, NA),
    moj_persistence_9mth_fcco = ifelse(moj_persistence_9mth_fcco >= 1, 1, NA),
    moj_persistence_12mth_fcco = ifelse(moj_persistence_12mth_fcco >= 1, 1, NA),
    moj_persistence_15mth_fcco = ifelse(moj_persistence_15mth_fcco >= 1, 1, NA),
    moj_persistence_18mth_fcco = ifelse(moj_persistence_18mth_fcco >= 1, 1, NA),
    moj_persistence_21mth_fcco = ifelse(moj_persistence_21mth_fcco >= 1, 1, NA),
    msd_payment_3mth_OV = ifelse(msd_payment_3mth_OV >= 1, 1, NA),
    msd_payment_6mth_OV = ifelse(msd_payment_6mth_OV >= 1, 1, NA),
    msd_payment_9mth_OV = ifelse(msd_payment_9mth_OV >= 1, 1, NA),
    msd_payment_12mth_OV = ifelse(msd_payment_12mth_OV >= 1, 1, NA),
    msd_persistence_3mth_OV = ifelse(msd_persistence_3mth_OV >= 1, 1, NA),
    msd_persistence_6mth_OV = ifelse(msd_persistence_6mth_OV >= 1, 1, NA),
    msd_persistence_9mth_OV = ifelse(msd_persistence_9mth_OV >= 1, 1, NA),
    msd_persistence_12mth_OV = ifelse(msd_persistence_12mth_OV >= 1, 1, NA),
    msd_persistence_15mth_OV = ifelse(msd_persistence_15mth_OV >= 1, 1, NA),
    msd_persistence_18mth_OV = ifelse(msd_persistence_18mth_OV >= 1, 1, NA),
    msd_persistence_21mth_OV = ifelse(msd_persistence_21mth_OV >= 1, 1, NA),
    msd_payment_3mth_RA = ifelse(msd_payment_3mth_RA >= 1, 1, NA),
    msd_payment_6mth_RA = ifelse(msd_payment_6mth_RA >= 1, 1, NA),
    msd_payment_9mth_RA = ifelse(msd_payment_9mth_RA >= 1, 1, NA),
    msd_payment_12mth_RA = ifelse(msd_payment_12mth_RA >= 1, 1, NA),
    msd_persistence_3mth_RA = ifelse(msd_persistence_3mth_RA >= 1, 1, NA),
    msd_persistence_6mth_RA = ifelse(msd_persistence_6mth_RA >= 1, 1, NA),
    msd_persistence_9mth_RA = ifelse(msd_persistence_9mth_RA >= 1, 1, NA),
    msd_persistence_12mth_RA = ifelse(msd_persistence_12mth_RA >= 1, 1, NA),
    msd_persistence_15mth_RA = ifelse(msd_persistence_15mth_RA >= 1, 1, NA),
    msd_persistence_18mth_RA = ifelse(msd_persistence_18mth_RA >= 1, 1, NA),
    msd_persistence_21mth_RA = ifelse(msd_persistence_21mth_RA >= 1, 1, NA),
    ird_payment_3mth_IT = ifelse(ird_payment_3mth_IT >= 1, 1, NA),
    ird_payment_6mth_IT = ifelse(ird_payment_6mth_IT >= 1, 1, NA),
    ird_payment_9mth_IT = ifelse(ird_payment_9mth_IT >= 1, 1, NA),
    ird_payment_12mth_IT = ifelse(ird_payment_12mth_IT >= 1, 1, NA),
    ird_persistence_3mth_IT = ifelse(ird_persistence_3mth_IT >= 1, 1, NA),
    ird_persistence_6mth_IT = ifelse(ird_persistence_6mth_IT >= 1, 1, NA),
    ird_persistence_9mth_IT = ifelse(ird_persistence_9mth_IT >= 1, 1, NA),
    ird_persistence_12mth_IT = ifelse(ird_persistence_12mth_IT >= 1, 1, NA),
    ird_persistence_15mth_IT = ifelse(ird_persistence_15mth_IT >= 1, 1, NA),
    ird_persistence_18mth_IT = ifelse(ird_persistence_18mth_IT >= 1, 1, NA),
    ird_persistence_21mth_IT = ifelse(ird_persistence_21mth_IT >= 1, 1, NA),
    ird_payment_3mth_LP = ifelse(ird_payment_3mth_LP >= 1, 1, NA),
    ird_payment_6mth_LP = ifelse(ird_payment_6mth_LP >= 1, 1, NA),
    ird_payment_9mth_LP = ifelse(ird_payment_9mth_LP >= 1, 1, NA),
    ird_payment_12mth_LP = ifelse(ird_payment_12mth_LP >= 1, 1, NA),
    ird_payment_12mth_LP = ifelse(ird_payment_12mth_LP >= 1, 1, NA),
    ird_persistence_3mth_LP = ifelse(ird_persistence_3mth_LP >= 1, 1, NA),
    ird_persistence_6mth_LP = ifelse(ird_persistence_6mth_LP >= 1, 1, NA),
    ird_persistence_9mth_LP = ifelse(ird_persistence_9mth_LP >= 1, 1, NA),
    ird_persistence_12mth_LP = ifelse(ird_persistence_12mth_LP >= 1, 1, NA),
    ird_persistence_15mth_LP = ifelse(ird_persistence_15mth_LP >= 1, 1, NA),
    ird_persistence_18mth_LP = ifelse(ird_persistence_18mth_LP >= 1, 1, NA),
    ird_persistence_21mth_LP = ifelse(ird_persistence_21mth_LP >= 1, 1, NA),
    ird_payment_3mth_F = ifelse(ird_payment_3mth_F >= 1, 1, NA),
    ird_payment_6mth_F = ifelse(ird_payment_6mth_F >= 1, 1, NA),
    ird_payment_9mth_F = ifelse(ird_payment_9mth_F >= 1, 1, NA),
    ird_payment_12mth_F = ifelse(ird_payment_12mth_F >= 1, 1, NA),
    ird_persistence_3mth_F = ifelse(ird_persistence_3mth_F >= 1, 1, NA),
    ird_persistence_6mth_F = ifelse(ird_persistence_6mth_F >= 1, 1, NA),
    ird_persistence_9mth_F = ifelse(ird_persistence_9mth_F >= 1, 1, NA),
    ird_persistence_12mth_F = ifelse(ird_persistence_12mth_F >= 1, 1, NA),
    ird_persistence_15mth_F = ifelse(ird_persistence_15mth_F >= 1, 1, NA),
    ird_persistence_18mth_F = ifelse(ird_persistence_18mth_F >= 1, 1, NA),
    ird_persistence_21mth_F = ifelse(ird_persistence_21mth_F >= 1, 1, NA),
    ird_payment_3mth_SL = ifelse(ird_payment_3mth_SL >= 1, 1, NA),
    ird_payment_6mth_SL = ifelse(ird_payment_6mth_SL >= 1, 1, NA),
    ird_payment_9mth_SL = ifelse(ird_payment_9mth_SL >= 1, 1, NA),
    ird_payment_12mth_SL = ifelse(ird_payment_12mth_SL >= 1, 1, NA),
    ird_persistence_3mth_SL = ifelse(ird_persistence_3mth_SL >= 1, 1, NA),
    ird_persistence_6mth_SL = ifelse(ird_persistence_6mth_SL >= 1, 1, NA),
    ird_persistence_9mth_SL = ifelse(ird_persistence_9mth_SL >= 1, 1, NA),
    ird_persistence_12mth_SL = ifelse(ird_persistence_12mth_SL >= 1, 1, NA),
    ird_persistence_15mth_SL = ifelse(ird_persistence_15mth_SL >= 1, 1, NA),
    ird_persistence_18mth_SL = ifelse(ird_persistence_18mth_SL >= 1, 1, NA),
    ird_persistence_21mth_SL = ifelse(ird_persistence_21mth_SL >= 1, 1, NA),
    
    
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
    ben_ind = ifelse(is.na(ben_ind), 0, ben_ind),
    child_at_address = ifelse(is.na(child_at_address), 0, child_at_address),
    confident_child_at_address = ifelse(is.na(confident_child_at_address), 0, confident_child_at_address),
    low_income = ifelse(is.na(low_income), 0, low_income),
    
    income_variability = ifelse(inc_mld_2019 > 0.5 | inc_mld_2020 > 0.5, 1, 
                                ifelse(is.na(inc_mld_2019) & is.na(inc_mld_2020), NA, 0)),
    abatement_indicator = ifelse(is.na(ws_ben_mth), NA, 
                                 ifelse(is.na(abatement_indicator), 0, abatement_indicator))
    
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
