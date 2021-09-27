/**************************************************************************************************
Title: 2018 household population
Author:  Freya Li
Reviewer: Simon Anastasiadis

Inputs & Dependencies:
- [IDI_Clean].[cen_clean].[census_individual_2018]
Outputs:
- [DL-MAA2020-01].[d2gP2_household_ID]

Description:
List of household values of New Zealand in 2020.

Intended purpose:
Producing summary statistics for the entire household population.

Notes:

Parameters & Present values:
  Current refresh = 20201020
  Prefix = d2gP2_
  Project schema = [DL-MAA2020-01]
   
Issues:
 
History (reverse order):
2021-09-20 SA reviewed
2021-07-12 FL
**************************************************************************************************/
/* Establish database for writing views */
USE IDI_UserCode
GO

IF OBJECT_ID('[DL-MAA2020-01].[d2gP2_household]','V') IS NOT NULL
DROP VIEW [DL-MAA2020-01].[d2gP2_household];
GO

CREATE VIEW [DL-MAA2020-01].[d2gP2_household] AS
SELECT snz_uid
	  ,ur_snz_cen_dwell_uid -- Dwelling ID for census night
	  ,IIF(cen_ind_record_type_code = 4 OR cen_ind_record_type_code = 6 OR cen_ind_record_type_code = 8 ,1, 0) AS child_ind_census_night -- child indicator on census night
	  -- 4 – NZ child, 6 – Overseas Child, 8 – Absentee elsewhere in NZ or away < 12 months (Child) 
	  ,IIF(DATEDIFF(MONTH, DATEFROMPARTS(cen_ind_birth_year_nbr, cen_ind_birth_month_nbr,1), '2020-09-30') <= 12*18, 1, 0) AS child_ind_custom_date -- child indicator at 2020-09-30
FROM [IDI_Clean_20201020].[cen_clean].[census_individual_2018]
WHERE ur_snz_cen_dwell_uid IS NOT NULL
GO


IF OBJECT_ID('[DL-MAA2020-01].[d2gP2_household_ID]','V') IS NOT NULL
DROP VIEW [DL-MAA2020-01].[d2gP2_household_ID];
GO

CREATE VIEW [DL-MAA2020-01].[d2gP2_household_ID] AS
SELECT DISTINCT ur_snz_cen_dwell_uid
FROM [IDI_Clean_20201020].[cen_clean].[census_individual_2018]
WHERE ur_snz_cen_dwell_uid IS NOT NULL

